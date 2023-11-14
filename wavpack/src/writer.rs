use crate::{Error, Result};
use std::{
    ffi::{c_char, c_int, c_void},
    fs::File,
    io::{self, BufWriter, Seek, Write},
    path::Path,
    ptr::NonNull,
};
use wavpack_sys::*;

const FALSE: c_int = 0;
const TRUE: c_int = 1;

/// File format
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub enum FileFormat {
    Wav,
    W64,
    Caf,
    Dff,
    Dsf,
}

impl FileFormat {
    fn to_u8(self) -> u8 {
        match self {
            Self::Wav => WP_FORMAT_WAV as u8,
            Self::W64 => WP_FORMAT_W64 as u8,
            Self::Caf => WP_FORMAT_CAF as u8,
            Self::Dff => WP_FORMAT_DFF as u8,
            Self::Dsf => WP_FORMAT_DSF as u8,
        }
    }
}

/// File information
#[derive(Debug, Clone)]
pub struct FileInfomation {
    pub extension: String,
    pub format: FileFormat,
}

#[derive(Clone, Debug, Default)]
struct BuilderConfig {
    bitrate: Option<f32>,
    shaping_weight: Option<f32>,
    bits_per_sample: Option<i32>,
    bytes_per_sample: Option<i32>,
    qmode: Option<i32>,
    flags: Option<i32>,
    xmode: Option<i32>,
    num_channels: Option<i32>,
    float_norm_exp: Option<i32>,
    block_samples: Option<i32>,
    extra_flags: Option<i32>,
    sample_rate: Option<i32>,
    channel_mask: Option<i32>,
    md5_checksum: Option<[u8; 16]>,
    md5_read: Option<u8>,
    //num_tag_strings: Option<i32>,
    //tag_strings: Option<Vec<String>>,
}

impl TryInto<WavpackConfig> for BuilderConfig {
    type Error = Error;

    fn try_into(self) -> Result<WavpackConfig> {
        let mut v = Vec::new();
        if self.bytes_per_sample.is_none() {
            v.push("bytes_per_sample");
        }
        if self.bits_per_sample.is_none() {
            v.push("bits_per_sample");
        }
        if self.channel_mask.is_none() {
            v.push("channel_mask");
        }
        if self.num_channels.is_none() {
            v.push("num_channels");
        }
        if self.sample_rate.is_none() {
            v.push("sample_rate");
        }
        if !v.is_empty() {
            return Err(Error::RequiredParameterNotSet(v));
        }

        let result = WavpackConfig {
            bitrate: self.bitrate.unwrap_or(0.),
            shaping_weight: self.shaping_weight.unwrap_or(0.),
            bits_per_sample: self.bits_per_sample.unwrap(),
            bytes_per_sample: self.bytes_per_sample.unwrap(),
            qmode: self.qmode.unwrap_or(0),
            flags: self.flags.unwrap_or(0),
            xmode: self.xmode.unwrap_or(0),
            num_channels: self.num_channels.unwrap(),
            float_norm_exp: self.float_norm_exp.unwrap_or(0),
            block_samples: self.block_samples.unwrap_or(0),
            extra_flags: self.extra_flags.unwrap_or(0),
            sample_rate: self.sample_rate.unwrap(),
            channel_mask: self.channel_mask.unwrap(),
            md5_checksum: self.md5_checksum.unwrap_or([0; 16]),
            md5_read: self.md5_read.unwrap_or(0),
            num_tag_strings: 0,
            tag_strings: std::ptr::null::<*mut c_char>() as *mut _,
        };

        Ok(result)
    }
}

/// A builder for [WavpackWriter]s
///
/// Created with [WavpackWriter::new].
pub struct WavpackWriterBuilder {
    wv_handle: Box<WriteHandle>,
    wvc_handle: Option<Box<WriteHandle>>,
    file_info: Option<FileInfomation>,
    wrap_header: Option<Vec<u8>>,
    config: BuilderConfig,
}

macro_rules! add_opt {
    ($fn_name:ident, $param:ident, $param_t:ty) => {
        #[must_use]
        pub fn $fn_name(mut self, $param: $param_t) -> Self {
            self.$param = Some($param);
            self
        }
    };
}

macro_rules! add_config_opt {
    ($fn_name:ident, $param:ident, $param_t:ty) => {
        #[must_use]
        pub fn $fn_name(mut self, $param: $param_t) -> Self {
            self.config.$param = Some($param);
            self
        }
    };
}

impl WavpackWriterBuilder {
    fn new(writer: Box<WriteHandle>) -> Self {
        WavpackWriterBuilder {
            wv_handle: writer,
            wvc_handle: None,
            file_info: None,
            wrap_header: None,
            config: BuilderConfig::default(),
        }
    }

    pub fn build(mut self) -> Result<WavpackWriter> {
        let mut config = self.config.try_into()?;

        let wv_ptr = &mut *self.wv_handle as *mut WriteHandle as *mut c_void;
        let wvc_ptr = match &mut self.wvc_handle {
            Some(x) => &mut **x as *mut WriteHandle,
            None => std::ptr::null::<WriteHandle>(),
        } as *mut c_void;

        let context = unsafe { WavpackOpenFileOutput(Some(write_block), wv_ptr, wvc_ptr) };
        if context.is_null() {
            return Err(Error::OpenFileOutputFailed);
        }

        if let Some(file_info) = self.file_info {
            let mut file_extension = file_info.extension.as_bytes().to_vec();
            let file_extension_ptr = file_extension.as_mut_ptr() as *mut _;
            let file_format = file_info.format.to_u8();
            unsafe {
                WavpackSetFileInformation(context, file_extension_ptr, file_format);
            }
        }

        if let Some(wrap_header) = &mut self.wrap_header {
            let ptr = wrap_header.as_mut_ptr() as *mut c_void;
            let len = wrap_header.len() as u32;
            if unsafe { WavpackAddWrapper(context, ptr, len) } == FALSE {
                unsafe { WavpackCloseFile(context) };
                return Err(Error::AddWrapperFailed);
            }
        }

        let config_ptr = &mut config as *mut _;
        unsafe {
            WavpackSetConfiguration64(
                context,
                config_ptr,
                -1, // The total sample count is updated when the writer is closed
                std::ptr::null(),
            );
            WavpackPackInit(context);
        }

        Ok(WavpackWriter {
            context: NonNull::new(context).unwrap(),
            wv_handle: self.wv_handle,
            _wvc: self.wvc_handle,
            config,
            is_flushed: true,
            is_closed: false,
        })
    }

    /// Adds an optional .wvc correction output to the writer
    #[must_use]
    pub fn add_wvc(mut self, writeable: impl WavpackWrite + 'static) -> Self {
        self.wvc_handle = Some(Box::new(WriteHandle::new(writeable)));
        self
    }

    add_opt!(add_file_info, file_info, FileInfomation);
    add_opt!(add_wrapper, wrap_header, Vec<u8>);
    add_config_opt!(add_bitrate, bitrate, f32);
    add_config_opt!(add_shaping_weight, shaping_weight, f32);
    add_config_opt!(add_bits_per_sample, bits_per_sample, i32);
    add_config_opt!(add_bytes_per_sample, bytes_per_sample, i32);
    add_config_opt!(add_qmode, qmode, i32);
    add_config_opt!(add_flags, flags, i32);
    add_config_opt!(add_xmode, xmode, i32);
    add_config_opt!(add_num_channels, num_channels, i32);
    add_config_opt!(add_float_norm_exp, float_norm_exp, i32);
    add_config_opt!(add_block_samples, block_samples, i32);
    add_config_opt!(add_extra_flags, extra_flags, i32);
    add_config_opt!(add_sample_rate, sample_rate, i32);
    add_config_opt!(add_channel_mask, channel_mask, i32);
    add_config_opt!(add_md5_checksum, md5_checksum, [u8; 16]);
    add_config_opt!(add_md5_read, md5_read, u8);
}

/// A writer for WavPack files
///
/// See [WavPackWriterBuilder]
pub struct WavpackWriter {
    context: NonNull<WavpackContext>,
    wv_handle: Box<WriteHandle>,
    _wvc: Option<Box<WriteHandle>>,
    config: WavpackConfig,
    is_flushed: bool,
    is_closed: bool,
}

impl WavpackWriter {
    /// Creates a WavPack file at the given path for writing
    ///
    /// See [`WavpackWriter::with_writer`] for more advanced options.
    pub fn create(file_path: impl AsRef<Path>) -> Result<WavpackWriterBuilder> {
        let file_writer = BufWriter::new(File::create(file_path.as_ref())?);
        let write_handle = Box::new(WriteHandle::new(file_writer));
        Ok(WavpackWriterBuilder::new(write_handle))
    }

    /// Prepares a WavPack writer that uses the provided [`WavpackWrite`] implementation
    pub fn with_writer(writer: impl WavpackWrite + 'static) -> WavpackWriterBuilder {
        WavpackWriterBuilder {
            wv_handle: Box::new(WriteHandle::new(writer)),
            wvc_handle: None,
            file_info: None,
            wrap_header: None,
            config: BuilderConfig::default(),
        }
    }

    /// Pack the specified samples.
    ///
    /// Requires: `samples.len() <= u32::MAX`
    pub fn pack_samples(&mut self, samples: &mut [i32]) -> Result<()> {
        let len = samples.len() / self.config.num_channels as usize;
        if usize::BITS >= u32::BITS && len > u32::MAX as usize {
            return Err(Error::IllegalArgument("samples"));
        }
        let wpc = self.context.as_ptr();
        let ptr = samples.as_mut_ptr();
        let len = len as u32;
        self.is_flushed = false;
        if unsafe { WavpackPackSamples(wpc, ptr, len) } == FALSE {
            return Err(Error::PackSamplesFailed);
        }
        Ok(())
    }

    /// Flush all accumulated samples into WavPack blocks.
    ///
    /// This is normally called after all samples have been sent to
    /// `pack_samples()`, but can also be called to terminate a WavPack block
    /// at a specific sample (in other words it is possible to continue after
    /// this operation). This also must be called to dump non-audio blocks like
    /// those holding metadata for MD5 sums or file trailers
    pub fn flush(&mut self) -> Result<()> {
        let wpc = self.context.as_ptr();
        if unsafe { WavpackFlushSamples(wpc) } == FALSE {
            return Err(Error::PackFlushFailed);
        }
        self.is_flushed = true;
        Ok(())
    }

    pub fn close(&mut self) -> Result<()> {
        if self.is_closed {
            return Err(Error::AlreadyClosed);
        }

        if !self.is_flushed {
            self.flush()?;
        }

        let wv = &mut self.wv_handle;
        if !wv.first_block.is_empty() {
            // It's the client's responsibility to ensure that WavpackUpdateNumSamples has been
            // called, and that the updated first block is re-written back to the output.

            unsafe {
                WavpackUpdateNumSamples(
                    self.context.as_ptr(),
                    wv.first_block.as_mut_ptr() as *mut c_void,
                )
            }

            wv.writeable.rewind()?;
            wv.writeable.write_all(&wv.first_block)?;
        }

        self.is_closed = true;
        Ok(())
    }

    /// Store computed MD5 sum in WavPack metadata.
    ///
    /// Note that the user must compute the 16 byte sum; it is not done here.
    /// It is also required that `flush()` be called after this to make sure
    /// the block containing the MD5 sum is actually written.
    pub fn store_md5sum(&mut self, data: &mut [u8; 16]) -> Result<()> {
        let wpc = self.context.as_ptr();
        let ptr = data.as_mut_ptr();
        self.is_flushed = false;
        if unsafe { WavpackStoreMD5Sum(wpc, ptr) } == FALSE {
            return Err(Error::StoreMD5SumFailed);
        }
        Ok(())
    }
}

impl Drop for WavpackWriter {
    fn drop(&mut self) {
        if !self.is_closed {
            let _ = self.close();
        }
        let wpc = self.context.as_ptr();
        unsafe { WavpackCloseFile(wpc) };
    }
}

pub trait WavpackWrite: Write + Seek {}

struct WriteHandle {
    writeable: Box<dyn WavpackWrite>,
    error: Option<io::Error>,
    // The first block needs to be updated and re-written when closing the file to ensure that the
    // correct sample count is included in the file header. Updating and re-writing the first block
    // is the caller's responsibility, so it gets cached here.
    first_block: Vec<u8>,
}

impl WriteHandle {
    fn new(writeable: impl WavpackWrite + 'static) -> Self {
        Self {
            writeable: Box::new(writeable),
            error: None,
            first_block: Vec::new(),
        }
    }
}

impl WavpackWrite for BufWriter<File> {}

// Writes the WavPack block to the WavPackWriter's writeable
extern "C" fn write_block(write_handle: *mut c_void, data: *mut c_void, bcount: i32) -> c_int {
    let write_handle = write_handle as *mut WriteHandle;
    if write_handle.is_null() {
        return FALSE;
    } else if data.is_null() || bcount == 0 {
        return TRUE;
    } else if bcount < 0 {
        return FALSE;
    }

    let write_handle = unsafe { &mut *write_handle };
    if write_handle.error.is_some() {
        return FALSE;
    }

    let data = unsafe { std::slice::from_raw_parts_mut(data as *mut u8, bcount as usize) };
    match write_handle.writeable.write_all(data) {
        Ok(_) => {
            if write_handle.first_block.is_empty() {
                write_handle.first_block.extend(data.iter());
            }

            TRUE
        }
        Err(x) => {
            write_handle.error = Some(x);
            FALSE
        }
    }
}
