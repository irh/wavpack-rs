use crate::{Error, Result};
use std::{
    ffi::{c_char, c_int, c_void},
    io::Write,
    ptr::NonNull,
};
use wavpack_sys::*;

fn option_to_ptr<T>(x: &mut Option<T>) -> *mut T {
    match x {
        Some(x) => x as *mut _,
        None => std::ptr::null::<T>() as *mut _,
    }
}

struct WriteId {
    writeable: Box<dyn Write>,
    error: Option<std::io::Error>,
}

impl WriteId {
    fn new(writeable: impl Write + 'static) -> Self {
        Self {
            writeable: Box::new(writeable),
            error: None,
        }
    }
}

unsafe extern "C" fn block_output(id: *mut c_void, data: *mut c_void, bcount: i32) -> c_int {
    const FALSE: c_int = 0;
    const TRUE: c_int = 1;
    let id = id as *mut WriteId;
    if id.is_null() {
        return FALSE;
    } else if data.is_null() || bcount == 0 {
        return TRUE;
    } else if bcount < 0 {
        return FALSE;
    }
    let id = &mut *id;
    if id.error.is_some() {
        return FALSE;
    }
    let data = std::slice::from_raw_parts_mut(data as *mut u8, bcount as usize);
    match id.writeable.write_all(data) {
        Ok(_) => TRUE,
        Err(x) => {
            id.error = Some(x);
            FALSE
        }
    }
}

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
struct Config {
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

impl Config {
    fn validate(&self) -> Result<()> {
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
        if v.is_empty() {
            Ok(())
        } else {
            Err(Error::RequiredParameterNotSet(v))
        }
    }

    fn convert(self) -> WavpackConfig {
        WavpackConfig {
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
        }
    }
}

pub struct WriteBuilder<'a> {
    wv: Box<WriteId>,
    wvc: Option<Box<WriteId>>,
    file_info: Option<FileInfomation>,
    wrap_header: Option<&'a mut [u8]>,
    config: Config,
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

impl<'a> WriteBuilder<'a> {
    pub fn new(writeable: impl Write + 'static) -> Self {
        Self {
            wv: Box::new(WriteId::new(writeable)),
            wvc: None,
            file_info: None,
            wrap_header: None,
            config: Config::default(),
        }
    }

    fn set_file_info(wpc: *mut WavpackContext, x: FileInfomation) {
        let mut file_extension = x.extension.as_bytes().to_vec();
        let file_extension_ptr = file_extension.as_mut_ptr() as *mut _;
        let file_format = x.format.to_u8();
        unsafe {
            WavpackSetFileInformation(wpc, file_extension_ptr, file_format);
        }
    }

    pub fn build(mut self, total_samples: i64) -> Result<WriteContext> {
        self.config.validate()?;
        let wv_ptr = &mut *self.wv as *mut WriteId as *mut c_void;
        let wvc_ptr = option_to_ptr(&mut self.wvc) as *mut _;
        let wpc = unsafe { WavpackOpenFileOutput(Some(block_output), wv_ptr, wvc_ptr) };
        if wpc.is_null() {
            return Err(Error::OpenFileOutputFailed);
        }
        if let Some(x) = self.file_info {
            Self::set_file_info(wpc, x);
        }
        if let Some(x) = self.wrap_header {
            let ptr = x.as_mut_ptr() as *mut c_void;
            let len = x.len() as u32;
            if unsafe { WavpackAddWrapper(wpc, ptr, len) } == 0 {
                unsafe { WavpackCloseFile(wpc) };
                return Err(Error::AddWrapperFailed);
            }
        }
        let mut config = self.config.convert();
        let config_ptr = &mut config as *mut _;
        unsafe {
            WavpackSetConfiguration64(wpc, config_ptr, total_samples, std::ptr::null());
            WavpackPackInit(wpc);
        }
        let context = WriteContext {
            context: NonNull::new(wpc).unwrap(),
            _wv: self.wv,
            _wvc: self.wvc,
            config,
            is_flushed: true,
        };
        Ok(context)
    }

    #[must_use]
    pub fn add_wvc(mut self, writeable: impl Write + 'static) -> Self {
        self.wvc = Some(Box::new(WriteId::new(writeable)));
        self
    }

    add_opt!(add_file_info, file_info, FileInfomation);
    add_opt!(add_wrapper, wrap_header, &'a mut [u8]);
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

pub struct WriteContext {
    context: NonNull<WavpackContext>,
    _wv: Box<WriteId>,
    _wvc: Option<Box<WriteId>>,
    config: WavpackConfig,
    is_flushed: bool,
}

impl WriteContext {
    /// Pack the specified samples.
    ///
    /// Requires: `buffer.len() <= u32::MAX`
    pub fn pack_samples(&mut self, samples: &mut [i32]) -> Result<()> {
        let len = samples.len() / self.config.num_channels as usize;
        if usize::BITS >= u32::BITS && len > u32::MAX as usize {
            return Err(Error::IllegalArgument("samples"));
        }
        let wpc = self.context.as_ptr();
        let ptr = samples.as_mut_ptr();
        let len = len as u32;
        self.is_flushed = false;
        if unsafe { WavpackPackSamples(wpc, ptr, len) } == 0 {
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
        if unsafe { WavpackFlushSamples(wpc) } == 0 {
            return Err(Error::PackFlushFailed);
        }
        self.is_flushed = true;
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
        if unsafe { WavpackStoreMD5Sum(wpc, ptr) } == 0 {
            return Err(Error::StoreMD5SumFailed);
        }
        Ok(())
    }

    pub fn add_wrapper(&mut self, data: &mut [u8]) -> Result<()> {
        let wpc = self.context.as_ptr();
        let ptr = data.as_mut_ptr() as *mut c_void;
        let len = data.len() as u32;
        self.is_flushed = false;
        if unsafe { WavpackAddWrapper(wpc, ptr, len) } == 0 {
            return Err(Error::AddWrapperFailed);
        }
        Ok(())
    }
}

impl Drop for WriteContext {
    fn drop(&mut self) {
        if !self.is_flushed {
            let _ = self.flush();
        }
        let wpc = self.context.as_ptr();
        unsafe { WavpackCloseFile(wpc) };
    }
}
