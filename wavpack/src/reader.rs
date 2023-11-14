use crate::{Error, Result};
use std::{
    collections::BTreeMap,
    ffi::{c_char, c_int, c_void, CStr, CString},
    fmt,
    fs::File,
    io::{BufReader, Read, Seek, SeekFrom},
    path::Path,
    ptr::NonNull,
    slice,
};
use wavpack_sys::*;

/// A builder for [WavpackReader]s
///
/// usage
/// ```
/// fn open_wavpack_file(path: &str) -> wavpack::WavpackReader {
///     wavpack::WavpackReader::open(path).unwrap().build().unwrap()
/// }
/// ```
pub struct WavpackReaderBuilder {
    stream_reader: Box<dyn WavpackRead>,
    flags: u32,
    norm_offset: Option<i32>,
}

macro_rules! add_flag {
    ($fn_name:ident, $flag:ident) => {
        #[must_use]
        pub fn $fn_name(mut self) -> Self {
            self.flags |= $flag;
            self
        }
    };
}

impl WavpackReaderBuilder {
    fn new(stream_reader: Box<dyn WavpackRead>) -> Self {
        Self {
            stream_reader,
            flags: 0,
            norm_offset: None,
        }
    }

    pub fn build(self) -> Result<WavpackReader> {
        let mut error_buffer = [0 as c_char; 81]; // 80 chars + NUL
        let error_ptr = error_buffer.as_mut_ptr();

        let mut stream_reader = Box::new(WavpackStreamReader::new(self.stream_reader));

        let context = unsafe {
            WavpackOpenFileInputEx64(
                &mut (*stream_reader.function_table) as *mut WavpackStreamReader64,
                &mut (*stream_reader) as *mut WavpackStreamReader as *mut c_void,
                std::ptr::null::<c_void>() as *mut c_void, // Reading with a .wvc file is currently unsupported
                error_ptr,
                self.flags as i32,
                self.norm_offset.unwrap_or(0),
            )
        };

        match NonNull::new(context) {
            Some(context) => Ok(WavpackReader {
                context,
                _stream_reader: stream_reader,
            }),
            None => {
                let error = char_ptr_to_string(error_ptr)?;
                Err(Error::Message(error))
            }
        }
    }

    #[must_use]
    pub fn normalize(mut self, norm_offset: i32) -> Self {
        self.flags |= OPEN_NORMALIZE;
        self.norm_offset = Some(norm_offset);
        self
    }

    add_flag!(apt_types, OPEN_ALT_TYPES);
    add_flag!(dsd_as_pcm, OPEN_DSD_AS_PCM);
    add_flag!(dsd_native, OPEN_DSD_NATIVE);
    add_flag!(edit_tags, OPEN_EDIT_TAGS);
    add_flag!(file_utf8, OPEN_FILE_UTF8);
    add_flag!(no_checksum, OPEN_NO_CHECKSUM);
    add_flag!(open_2ch_max, OPEN_2CH_MAX);
    add_flag!(streaming, OPEN_STREAMING);
    add_flag!(tags, OPEN_TAGS);
    add_flag!(wrapper, OPEN_WRAPPER);
    add_flag!(wvc, OPEN_WVC);
}

/// The set of reading operations needed by [WavpackReader]
pub trait WavpackRead: Read + Seek {
    fn stream_length(&mut self) -> Option<u64>;
}

impl WavpackRead for BufReader<File> {
    fn stream_length(&mut self) -> Option<u64> {
        self.get_ref()
            .metadata()
            .ok()
            .map(|metadata| metadata.len())
    }
}

struct WavpackStreamReader {
    reader: Option<Box<dyn WavpackRead>>,
    function_table: Box<WavpackStreamReader64>,
    // WavPack's `push_back_byte` function expects `ungetc`-like behaviour.
    // As of 5.6.0, it's only used for a single byte of lookahead.
    // This may change in the future, and this will need to be adapted.
    pushed_back_byte: Option<u8>,
}

impl WavpackStreamReader {
    fn new(reader: Box<dyn WavpackRead>) -> Self {
        Self {
            reader: Some(reader),
            function_table: Box::new(Self::create_function_table()),
            pushed_back_byte: None,
        }
    }

    fn create_function_table() -> WavpackStreamReader64 {
        WavpackStreamReader64 {
            read_bytes: Some(Self::read_bytes),
            get_pos: Some(Self::get_pos),
            set_pos_abs: Some(Self::set_pos_abs),
            set_pos_rel: Some(Self::set_pos_rel),
            push_back_byte: Some(Self::push_back_byte),
            get_length: Some(Self::get_length),
            can_seek: Some(Self::can_seek),
            close: Some(Self::close),

            // Operations needed for reader tag editing, which is currently unsupported
            truncate_here: None,
            write_bytes: None,
        }
    }

    fn from_ptr<'a>(ptr: *mut c_void) -> &'a mut Self {
        assert!(!ptr.is_null());
        unsafe { &mut *(ptr as *mut WavpackStreamReader) }
    }

    extern "C" fn read_bytes(instance_ptr: *mut c_void, data: *mut c_void, count: i32) -> i32 {
        let instance = Self::from_ptr(instance_ptr);
        let Some(reader) = &mut instance.reader else {
            return -1;
        };

        assert!(count >= 0);
        if count == 0 {
            return 0;
        }
        let count = count as usize;

        let buffer = unsafe { slice::from_raw_parts_mut(data as *mut u8, count) };

        let mut bytes_read = match instance.pushed_back_byte.take() {
            Some(byte) => {
                buffer[0] = byte;
                1
            }
            None => 0,
        };

        while bytes_read < count {
            match reader.read(&mut buffer[bytes_read..]) {
                Ok(count) => {
                    if count == 0 {
                        return bytes_read as i32;
                    }
                    bytes_read += count;
                }
                Err(_) => {
                    return -1;
                }
            }
        }

        bytes_read as i32
    }

    extern "C" fn get_pos(instance_ptr: *mut c_void) -> i64 {
        Self::from_ptr(instance_ptr)
            .reader
            .as_mut()
            .and_then(|reader| reader.as_mut().stream_position().ok())
            .and_then(|position| i64::try_from(position).ok())
            .unwrap_or(-1)
    }

    extern "C" fn get_length(instance_ptr: *mut c_void) -> i64 {
        Self::from_ptr(instance_ptr)
            .reader
            .as_mut()
            .and_then(|reader| reader.stream_length())
            .and_then(|position| i64::try_from(position).ok())
            .unwrap_or(-1)
    }

    extern "C" fn set_pos_abs(instance_ptr: *mut c_void, pos: i64) -> c_int {
        match Self::from_ptr(instance_ptr)
            .reader
            .as_mut()
            .and_then(|reader| reader.seek(SeekFrom::Start(pos as u64)).ok())
        {
            Some(_) => 0,
            None => -1,
        }
    }

    extern "C" fn set_pos_rel(instance_ptr: *mut c_void, delta: i64, mode: c_int) -> c_int {
        let seek_mode = match mode {
            // SEEK_SET
            0 => SeekFrom::Start(delta as u64),
            // SEEK_CUR
            1 => SeekFrom::Current(delta),
            // SEEK_END
            2 => SeekFrom::End(delta),
            _ => return -1,
        };

        match Self::from_ptr(instance_ptr)
            .reader
            .as_mut()
            .and_then(|reader| reader.seek(seek_mode).ok())
        {
            Some(_) => 0,
            None => -1,
        }
    }

    extern "C" fn push_back_byte(instance_ptr: *mut c_void, c: c_int) -> c_int {
        let instance = Self::from_ptr(instance_ptr);

        if instance.pushed_back_byte.is_some() {
            // As of writing, WavPack only performs single-byte lookahead.
            // If this is no longer the case then a buffer will be needed instead of a single byte.
            debug_assert!(false);
            -1
        } else {
            instance.pushed_back_byte = Some(c as u8);
            c
        }
    }

    extern "C" fn can_seek(_instance_ptr: *mut c_void) -> c_int {
        1
    }

    extern "C" fn close(instance_ptr: *mut c_void) -> c_int {
        Self::from_ptr(instance_ptr).reader.take();
        1
    }
}

macro_rules! add_wavpack_fn {
    ($vis:vis $fn_name:ident, $c_func:ident, $ret:ty $(, $param:ident, $param_t:ty)*) => {
        $vis fn $fn_name(&mut self $(, $param: $param_t)*) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc $(, $param)*) };
            self.get_error_message()?;
            Ok(r)
        }
    };
}

macro_rules! wavpack_fn {
    ($($token:tt)*) => {
        add_wavpack_fn!(pub $($token)*);
    };
}

macro_rules! wavpack_fn_private {
    ($($token:tt)*) => {
        add_wavpack_fn!($($token)*);
    };
}

/// A WavPack file reader
pub struct WavpackReader {
    context: NonNull<WavpackContext>,
    _stream_reader: Box<WavpackStreamReader>,
}

impl WavpackReader {
    /// Opens a WavPack file at the given path for reading
    ///
    /// See [`WavpackReader::with_reader`] for more advanced options.
    pub fn open(file_path: impl AsRef<Path>) -> Result<WavpackReaderBuilder> {
        let file_reader = BufReader::new(File::open(file_path.as_ref())?);
        Ok(WavpackReaderBuilder::new(Box::new(file_reader)))
    }

    /// Prepares a WavPack reader that uses the provided stream reader for reading operations
    pub fn with_reader(stream_reader: impl WavpackRead + 'static) -> WavpackReaderBuilder {
        WavpackReaderBuilder::new(Box::new(stream_reader))
    }

    // TODO: WavpackOpenFileInputEx, WavpackOpenFileInputEx64
    wavpack_fn!(get_mode, WavpackGetMode, i32);
    wavpack_fn!(get_num_channels, WavpackGetNumChannels, i32);
    wavpack_fn!(get_reduced_channels, WavpackGetReducedChannels, i32);
    wavpack_fn!(get_channel_mask, WavpackGetChannelMask, i32);
    //wavpack_fn!(
    //    get_channel_layout,
    //    WavpackGetChannelLayout,
    //    u32,
    //    reorder,
    //    *mut u8
    //);
    //wavpack_fn!(
    //    get_channel_identities,
    //    WavpackGetChannelIdentities,
    //    (),
    //    identities,
    //    *mut u8
    //);
    wavpack_fn!(get_sample_rate, WavpackGetSampleRate, u32);
    wavpack_fn!(get_native_sample_rate, WavpackGetNativeSampleRate, u32);
    wavpack_fn!(get_bits_per_sample, WavpackGetBitsPerSample, i32);
    wavpack_fn!(get_bytes_per_sample, WavpackGetBytesPerSample, i32);
    wavpack_fn!(get_version, WavpackGetVersion, i32);
    wavpack_fn!(get_file_format, WavpackGetFileFormat, u8);
    pub fn get_file_extension(&mut self) -> Result<String> {
        let wpc = self.context.as_ptr();
        let r = unsafe { WavpackGetFileExtension(wpc) };
        self.get_error_message()?;
        let r = char_ptr_to_string(r)?;
        Ok(r)
    }
    wavpack_fn!(get_qualify_mode, WavpackGetQualifyMode, i32);
    wavpack_fn!(get_num_samples, WavpackGetNumSamples, u32);
    wavpack_fn!(get_num_samples64, WavpackGetNumSamples64, i64);
    wavpack_fn!(get_file_size, WavpackGetFileSize, u32);
    wavpack_fn!(get_file_size64, WavpackGetFileSize64, i64);
    wavpack_fn!(get_ratio, WavpackGetRatio, f64);
    wavpack_fn!(
        get_average_bitrate,
        WavpackGetAverageBitrate,
        f64,
        count_wvc,
        i32
    );
    wavpack_fn!(get_float_norm_exp, WavpackGetFloatNormExp, i32);

    pub fn get_md5_sum(&mut self, data: &mut [u8; 16]) -> Result<i32> {
        let wpc = self.context.as_ptr();
        let r = unsafe { WavpackGetMD5Sum(wpc, data.as_mut_ptr()) };
        self.get_error_message()?;
        Ok(r)
    }

    //wavpack_fn!(get_wrapper_bytes, WavpackGetWrapperBytes, u32);
    //wavpack_fn!(get_wrapper_data, WavpackGetWrapperData, *mut u8);
    //wavpack_fn!(free_wrapper, WavpackFreeWrapper, ());
    //wavpack_fn!(seek_trailing_wrapper, WavpackSeekTrailingWrapper, ());

    /// Unpack the specified number of samples from the current file position.
    ///
    /// Requires: `buffer.len() <= u32::MAX`
    pub fn unpack_samples(&mut self, buffer: &mut [i32]) -> Result<u32> {
        let len = buffer.len() / self.get_num_channels()? as usize;
        if usize::BITS >= u32::BITS && len > u32::MAX as usize {
            return Err(Error::IllegalArgument("buffer"));
        }
        let ptr = buffer.as_mut_ptr();
        let len = len as u32;
        let wpc = self.context.as_ptr();
        let r = unsafe { WavpackUnpackSamples(wpc, ptr, len) };
        self.get_error_message()?;
        Ok(r)
    }

    wavpack_fn!(seek_sample64, WavpackSeekSample64, i32, sample, i64);
    wavpack_fn!(seek_sample, WavpackSeekSample, i32, sample, u32);
    wavpack_fn!(get_sample_index64, WavpackGetSampleIndex64, i64);
    wavpack_fn!(get_sample_index, WavpackGetSampleIndex, u32);
    wavpack_fn!(get_instant_bitrate, WavpackGetInstantBitrate, f64);
    wavpack_fn!(get_num_errors, WavpackGetNumErrors, i32);
    wavpack_fn!(get_lossy_blocks, WavpackLossyBlocks, i32);
    wavpack_fn!(get_progress, WavpackGetProgress, f64);

    /// unpack from `start` (frame) to `start+length` (frame)
    ///
    /// [frames per second = 75](https://en.wikipedia.org/wiki/Compact_Disc_Digital_Audio#Frames_and_timecode_frames)
    pub fn unpack(&mut self, start: i64, length: i64) -> Result<Vec<i32>> {
        const FRAME_PER_SECOND: i64 = 75;
        let channels = self.get_num_channels()? as i64;
        let sample_rate_par_frame = self.get_sample_rate()? as i64 / FRAME_PER_SECOND;
        let sample_start = sample_rate_par_frame * start;
        let sample_length = {
            let l = channels * sample_rate_par_frame * length;
            if l > u32::MAX as i64 {
                return Err(Error::LengthTooLong);
            } else {
                l as usize
            }
        };
        self.seek_sample64(sample_start)?;
        let mut buf = vec![0; sample_length];
        self.unpack_samples(&mut buf)?;
        Ok(buf)
    }

    /// Get text tags
    pub fn get_text_tag_items(&mut self) -> Result<Vec<(String, String)>> {
        let n = self.get_num_tag_items()?;
        let mut v = Vec::with_capacity(n as usize);
        for i in 0..n {
            let key = self.get_tag_item_indexed(i)?;
            let val = self.get_tag_item(&key)?.into_string()?;
            v.push((key.into_string()?, val));
        }
        Ok(v)
    }

    /// Get binary tags
    pub fn get_binary_tag_items(&mut self) -> Result<Vec<(String, BinaryTag)>> {
        let n = self.get_num_binary_tag_items()?;
        let mut v = Vec::with_capacity(n as usize);
        for i in 0..n {
            let key = self.get_binary_tag_item_indexed(i)?;
            let (v_name, v_data) = self.get_binary_tag_item(&key)?;
            v.push((key.into_string()?, (v_name.into_string()?, v_data)));
        }
        Ok(v)
    }

    /// Get text and binary tags
    pub fn get_all_tag_items(&mut self) -> Result<BTreeMap<String, TagData>> {
        use TagData::*;
        let mut tags = BTreeMap::new();
        for (key, val) in self.get_text_tag_items()? {
            tags.insert(key, Text(val));
        }
        for (key, (v_name, v_data)) in self.get_binary_tag_items()? {
            tags.insert(key, Binary((v_name, v_data)));
        }
        Ok(tags)
    }

    wavpack_fn_private!(get_num_tag_items, WavpackGetNumTagItems, i32);
    wavpack_fn_private!(get_num_binary_tag_items, WavpackGetNumBinaryTagItems, i32);

    fn get_tag_item_indexed(&mut self, index: i32) -> Result<CString> {
        let wpc = self.context.as_ptr();
        let func = |ptr, size| {
            let r = unsafe { WavpackGetTagItemIndexed(wpc, index, ptr, size) };
            self.get_error_message()?;
            Ok(r)
        };
        check_and_get_text(func)
    }

    fn get_tag_item(&mut self, key: &CStr) -> Result<CString> {
        let key = key.as_ptr();
        let wpc = self.context.as_ptr();
        let func = |ptr, size| {
            let r = unsafe { WavpackGetTagItem(wpc, key, ptr, size) };
            self.get_error_message()?;
            Ok(r)
        };
        check_and_get_text(func)
    }

    fn get_binary_tag_item_indexed(&mut self, index: i32) -> Result<CString> {
        let wpc = self.context.as_ptr();
        let func = |ptr, size| {
            let r = unsafe { WavpackGetBinaryTagItemIndexed(wpc, index, ptr, size) };
            self.get_error_message()?;
            Ok(r)
        };
        check_and_get_text(func)
    }

    fn get_binary_tag_item(&mut self, key: &CStr) -> Result<(CString, Vec<u8>)> {
        let key = key.as_ptr();
        let wpc = self.context.as_ptr();
        let func = |ptr, size| {
            let r = unsafe { WavpackGetBinaryTagItem(wpc, key, ptr, size) };
            self.get_error_message()?;
            Ok(r)
        };
        let mut v = check_and_get_binary(func)?;
        let v_ptr = v.as_mut_ptr() as *mut i8;
        let name = char_ptr_to_cstring(v_ptr)?;
        let name_len = name.to_bytes_with_nul().len();
        Ok((name, v[name_len..].to_vec()))
    }

    fn get_error_message(&mut self) -> Result<()> {
        let wpc = self.context.as_ptr();
        let error_message = unsafe { WavpackGetErrorMessage(wpc) };
        if error_message.is_null() {
            return Ok(());
        }
        let error_message = char_ptr_to_string(error_message)?;
        if error_message.is_empty() {
            Ok(())
        } else {
            Err(Error::Message(error_message))
        }
    }
}

impl Drop for WavpackReader {
    fn drop(&mut self) {
        let wpc = self.context.as_ptr();
        unsafe { WavpackCloseFile(wpc) };
    }
}

/// Filename, binary data
pub type BinaryTag = (String, Vec<u8>);

/// Tag data
#[derive(Clone)]
pub enum TagData {
    /// Text data
    Text(String),
    /// Binary data
    Binary(BinaryTag),
}

impl fmt::Debug for TagData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TagData::*;
        match self {
            Text(x) => f.debug_tuple("Text").field(&x).finish(),
            Binary((name, data)) => {
                let len = data.len();
                f.debug_tuple("Binary")
                    .field(&name)
                    .field(&format!("{len}-byte binary item"))
                    .finish()
            }
        }
    }
}

fn check_and_get_text<F>(mut func: F) -> Result<CString>
where
    F: FnMut(*mut c_char, i32) -> Result<i32>,
{
    let nullptr = std::ptr::null::<c_char>() as *mut c_char;
    let len = func(nullptr, 0)?;
    let size = len + 1; // NUL byte
    let mut buf = vec![0 as c_char; size as usize];
    let buf_ptr = buf.as_mut_ptr();
    let n_len = func(buf_ptr, size)?;
    assert_eq!(len, n_len);
    char_ptr_to_cstring(buf_ptr)
}

fn check_and_get_binary<F>(mut func: F) -> Result<Vec<u8>>
where
    F: FnMut(*mut c_char, i32) -> Result<i32>,
{
    let nullptr = std::ptr::null::<c_char>() as *mut c_char;
    let len = func(nullptr, 0)?;
    let mut buf = vec![0u8; len as usize];
    let buf_ptr = buf.as_mut_ptr() as *mut i8;
    let n_len = func(buf_ptr, len)?;
    assert_eq!(len, n_len);
    Ok(buf)
}

fn char_ptr_to_cstring(src: *const c_char) -> Result<CString> {
    if src.is_null() {
        Err(Error::NullPointer)
    } else {
        let cstr = unsafe { CStr::from_ptr(src) };
        Ok(cstr.to_owned())
    }
}

fn char_ptr_to_string(src: *const c_char) -> Result<String> {
    Ok(char_ptr_to_cstring(src)?.into_string()?)
}
