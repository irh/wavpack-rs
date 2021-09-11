#![doc = include_str!("../README.md")]
use std::collections::BTreeMap;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::Write;
use std::os::raw::{c_char, c_int, c_void};
use std::path::Path;
use std::ptr::NonNull;
use thiserror::Error;
use wavpack_sys::*;

/// Error type
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum Error {
    /// message from `WavpackGetErrorMessage`
    #[error("{0}")]
    Message(String),
    /// illegal argument
    #[error("illegal argument {0}")]
    IllegalArgument(&'static str),
    #[error("length is too long")]
    LengthTooLong,
    #[error("Null pointer")]
    NullPointer,
    #[error("Required parameter {0:?} is not set")]
    RequiredParameterNotSet(Vec<&'static str>),
    #[error("WavpackOpenFileOutput failed")]
    OpenFileOutputFailed,
    #[error("Unkown format")]
    UnkownFormat,
    #[error("WavpackPackSamples failed")]
    PackSamplesFailed,
    #[error("WavpackFlushSamples failed")]
    PackFlushFailed,
    #[error("WavpackStoreMD5Sum failed")]
    StoreMD5SumFailed,
    #[error("WavpacAddWrapper failed")]
    AddWrapperFailed,
    #[error(transparent)]
    NulError(#[from] std::ffi::NulError),
    #[error(transparent)]
    IntoStringError(#[from] std::ffi::IntoStringError),
}
pub type Result<T> = std::result::Result<T, Error>;

unsafe fn char_ptr_to_cstring(src: *const c_char) -> Result<CString> {
    if src.is_null() {
        Err(Error::NullPointer)
    } else {
        Ok(CStr::from_ptr(src).to_owned())
    }
}
unsafe fn char_ptr_to_string(src: *const c_char) -> Result<String> {
    Ok(char_ptr_to_cstring(src)?.into_string()?)
}

fn option_to_ptr<T>(x: &mut Option<T>) -> *mut T {
    match x {
        Some(x) => x as *mut _,
        None => std::ptr::null::<T>() as *mut _,
    }
}

/// Return the WavPack library version in a packed integer.
///
/// Bits 0-7 give the micro version, bits 8-15 give the minor version, and
/// bits 16-23 give the major version. As of this writing the version is 5.0.0.
/// ```
/// use wavpack::get_library_version;
/// let version = get_library_version();
/// println!("{:x}", version); // If version is 5.1.0, this prints `50100`.
/// ```
pub fn get_library_version() -> u32 {
    unsafe { WavpackGetLibraryVersion() }
}
/// Return the WavPack library version as a string.
///
/// As of this writing this is "5.0.0".
/// ```
/// use wavpack::get_library_version_string;
/// let version = get_library_version_string();
/// println!("{}", version); // If version is 5.1.0, this prints `5.1.0`.
/// ```
pub fn get_library_version_string() -> &'static str {
    let c_str = unsafe { CStr::from_ptr(WavpackGetLibraryVersionString()) };
    c_str.to_str().unwrap()
}
/// Builder of Context
///
/// usage
/// ```
/// use std::path::PathBuf;
/// use wavpack::ContextBuilder;
/// let path = PathBuf::from("/path/to/foo.wv");
/// let context = ContextBuilder::new(&path).tags().edit_tags().build();
/// ```
pub struct ContextBuilder<'a> {
    file_name: &'a Path,
    flag: u32,
    norm_offset: Option<i32>,
}
macro_rules! add_flag {
    ($fn_name:ident, $flag:ident) => {
        pub fn $fn_name(mut self) -> Self {
            self.flag |= $flag;
            self
        }
    };
}
impl<'a> ContextBuilder<'a> {
    pub fn new(file_name: &'a Path) -> Self {
        Self {
            file_name,
            flag: 0,
            norm_offset: None,
        }
    }
    pub fn build(self) -> Result<Context> {
        Context::new(
            self.file_name,
            self.flag as i32,
            self.norm_offset.unwrap_or(0),
        )
    }
    add_flag!(wvc, OPEN_WVC);
    add_flag!(tags, OPEN_TAGS);
    add_flag!(wrapper, OPEN_WRAPPER);
    add_flag!(open_2ch_max, OPEN_2CH_MAX);
    pub fn normalize(mut self, norm_offset: i32) -> Self {
        self.flag |= OPEN_NORMALIZE;
        self.norm_offset = Some(norm_offset);
        self
    }
    add_flag!(streaming, OPEN_STREAMING);
    add_flag!(edit_tags, OPEN_EDIT_TAGS);
    add_flag!(file_utf8, OPEN_FILE_UTF8);
    add_flag!(dsd_native, OPEN_DSD_NATIVE);
    add_flag!(dsd_as_pcm, OPEN_DSD_AS_PCM);
    add_flag!(apt_types, OPEN_ALT_TYPES);
    add_flag!(no_checksum, OPEN_NO_CHECKSUM);
}

/// WavpackContext
pub struct Context {
    context: NonNull<WavpackContext>,
}
// TODO: improve this
macro_rules! def_fn {
    ($fn_name:ident, $c_func:ident, $ret:ty) => {
        pub fn $fn_name(&mut self) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc) };
            self.get_error_message()?;
            Ok(r)
        }
    };
    ($fn_name:ident, $c_func:ident, $ret:ty, $param:ident, $param_t:ty) => {
        pub fn $fn_name(&mut self, $param: $param_t) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc, $param) };
            self.get_error_message()?;
            Ok(r)
        }
    };
    ($fn_name:ident, $c_func:ident, $ret:ty, $p1:ident, $p1_t:ty, $p2:ident, $p2_t:ty) => {
        pub fn $fn_name(&mut self, $p1: $p1_t, $p2: $p2_t) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc, $p1, $p2) };
            self.get_error_message()?;
            Ok(r)
        }
    };
}
macro_rules! def_private_fn {
    ($fn_name:ident, $c_func:ident, $ret:ty) => {
        fn $fn_name(&mut self) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc) };
            self.get_error_message()?;
            Ok(r)
        }
    };
    ($fn_name:ident, $c_func:ident, $ret:ty, $param:ident, $param_t:ty) => {
        fn $fn_name(&mut self, $param: $param_t) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc, $param) };
            self.get_error_message()?;
            Ok(r)
        }
    };
    ($fn_name:ident, $c_func:ident, $ret:ty, $p1:ident, $p1_t:ty, $p2:ident, $p2_t:ty) => {
        fn $fn_name(&mut self, $p1: $p1_t, $p2: $p2_t) -> Result<$ret> {
            let wpc = self.context.as_ptr();
            let r = unsafe { $c_func(wpc, $p1, $p2) };
            self.get_error_message()?;
            Ok(r)
        }
    };
}
/// Reading WavPack Files
impl Context {
    fn get_error_message(&mut self) -> Result<()> {
        let wpc = self.context.as_ptr();
        let error_message = unsafe { WavpackGetErrorMessage(wpc) };
        if error_message.is_null() {
            return Ok(());
        }
        let error_message = unsafe { char_ptr_to_string(error_message) }?;
        if error_message.is_empty() {
            Ok(())
        } else {
            Err(Error::Message(error_message))
        }
    }
    /// Open file to read.
    ///
    /// I recommend you use [`ContextBuilder`].
    pub fn new(file_name: &Path, flag: i32, norm_offset: i32) -> Result<Self> {
        let file_name = CString::new(file_name.display().to_string())?;
        let file_name = file_name.as_ptr();
        let mut error = vec![0 as c_char; 81]; // 80 chars + NUL
        let error_ptr = error.as_mut_ptr();
        let context = unsafe { WavpackOpenFileInput(file_name, error_ptr, flag, norm_offset) };
        match NonNull::new(context) {
            None => {
                let error = unsafe { char_ptr_to_string(error_ptr) }?;
                Err(Error::Message(error))
            }
            Some(context) => Ok(Self { context }),
        }
    }
    // TODO: WavpackOpenFileInputEx, WavpackOpenFileInputEx64
    def_fn!(get_mode, WavpackGetMode, i32);
    def_fn!(get_num_channels, WavpackGetNumChannels, i32);
    def_fn!(get_reduced_channels, WavpackGetReducedChannels, i32);
    def_fn!(get_channel_mask, WavpackGetChannelMask, i32);
    //def_fn!(
    //    get_channel_layout,
    //    WavpackGetChannelLayout,
    //    u32,
    //    reorder,
    //    *mut u8
    //);
    //def_fn!(
    //    get_channel_identities,
    //    WavpackGetChannelIdentities,
    //    (),
    //    identities,
    //    *mut u8
    //);
    def_fn!(get_sample_rate, WavpackGetSampleRate, u32);
    def_fn!(get_native_sample_rate, WavpackGetNativeSampleRate, u32);
    def_fn!(get_bits_par_sample, WavpackGetBitsPerSample, i32);
    def_fn!(get_bytes_par_sample, WavpackGetBytesPerSample, i32);
    def_fn!(get_version, WavpackGetVersion, i32);
    def_fn!(get_file_format, WavpackGetFileFormat, u8);
    pub fn get_file_extension(&mut self) -> Result<String> {
        let wpc = self.context.as_ptr();
        let r = unsafe { WavpackGetFileExtension(wpc) };
        self.get_error_message()?;
        let r = unsafe { char_ptr_to_string(r) }?;
        Ok(r)
    }
    def_fn!(get_qualify_mode, WavpackGetQualifyMode, i32);
    def_fn!(get_num_samples, WavpackGetNumSamples, u32);
    def_fn!(get_num_samples64, WavpackGetNumSamples64, i64);
    def_fn!(get_file_size, WavpackGetFileSize, u32);
    def_fn!(get_file_size64, WavpackGetFileSize64, i64);
    def_fn!(get_ratio, WavpackGetRatio, f64);
    def_fn!(
        get_average_bitrate,
        WavpackGetAverageBitrate,
        f64,
        count_wvc,
        i32
    );
    def_fn!(get_float_norm_exp, WavpackGetFloatNormExp, i32);
    pub fn get_md5_sum(&mut self, data: &mut [u8; 16]) -> Result<i32> {
        let wpc = self.context.as_ptr();
        let r = unsafe { WavpackGetMD5Sum(wpc, data.as_mut_ptr()) };
        self.get_error_message()?;
        Ok(r)
    }
    //def_fn!(get_wrapper_bytes, WavpackGetWrapperBytes, u32);
    //def_fn!(get_wrapper_data, WavpackGetWrapperData, *mut u8);
    //def_fn!(free_wrapper, WavpackFreeWrapper, ());
    //def_fn!(seek_trailing_wrapper, WavpackSeekTrailingWrapper, ());
    /// Unpack the specified number of samples from the current file position.
    ///
    /// Requires: `buffer.len() <= u32::MAX`
    pub fn unpack_samples(&mut self, buffer: &mut [i32]) -> Result<u32> {
        let len = buffer.len();
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
    def_fn!(seek_sample64, WavpackSeekSample64, i32, sample, i64);
    def_fn!(seek_sample, WavpackSeekSample, i32, sample, u32);
    def_fn!(get_sample_index64, WavpackGetSampleIndex64, i64);
    def_fn!(get_sample_index, WavpackGetSampleIndex, u32);
    def_fn!(get_instant_bitrate, WavpackGetInstantBitrate, f64);
    def_fn!(get_num_errors, WavpackGetNumErrors, i32);
    def_fn!(get_lossy_blocks, WavpackLossyBlocks, i32);
    def_fn!(get_progress, WavpackGetProgress, f64);
    /// unpack from `start` (frame) to `start+length` (frame)
    ///
    /// [frames par second = 75](https://en.wikipedia.org/wiki/Compact_Disc_Digital_Audio#Frames_and_timecode_frames)
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
}

pub type BinaryTag = (String, Vec<u8>);
#[derive(Clone)]
/// Tag data
pub enum TagData {
    /// text data
    Text(String),
    /// filename, binary data
    Binary(String, Vec<u8>),
}
impl std::fmt::Debug for TagData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TagData::*;
        match self {
            Text(x) => f.debug_tuple("Text").field(&x).finish(),
            Binary(name, data) => {
                let len = data.len();
                f.debug_tuple("Binary")
                    .field(&name)
                    .field(&format!("{}-byte binary item", len))
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
    unsafe { char_ptr_to_cstring(buf_ptr) }
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
/// Tagging Functions
impl Context {
    def_private_fn!(get_num_tag_items, WavpackGetNumTagItems, i32);
    def_private_fn!(get_num_binary_tag_items, WavpackGetNumBinaryTagItems, i32);
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
        let name = unsafe { char_ptr_to_cstring(v_ptr) }?;
        let name_len = name.to_bytes_with_nul().len();
        Ok((name, v[name_len..].to_vec()))
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
            tags.insert(key, Binary(v_name, v_data));
        }
        Ok(tags)
    }
    // TODO
}
impl Drop for Context {
    fn drop(&mut self) {
        let wpc = self.context.as_ptr();
        unsafe { WavpackCloseFile(wpc) };
    }
}

#[derive(Debug)]
struct WriteId {
    file: std::fs::File,
    error: Option<std::io::Error>,
}
impl WriteId {
    fn new(file: File) -> Self {
        Self { file, error: None }
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
    match id.file.write_all(data) {
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
    WAV,
    W64,
    CAF,
    DFF,
    DSF,
}
impl FileFormat {
    fn to_u8(self) -> u8 {
        match self {
            Self::WAV => WP_FORMAT_WAV as u8,
            Self::W64 => WP_FORMAT_W64 as u8,
            Self::CAF => WP_FORMAT_CAF as u8,
            Self::DFF => WP_FORMAT_DFF as u8,
            Self::DSF => WP_FORMAT_DSF as u8,
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

#[derive(Debug)]
pub struct WriteBuilder<'a> {
    wv: WriteId,
    wvc: Option<WriteId>,
    file_info: Option<FileInfomation>,
    wrap_header: Option<&'a mut [u8]>,
    config: Config,
}
macro_rules! add_opt {
    ($fn_name:ident, $param:ident, $param_t:ty) => {
        pub fn $fn_name(mut self, $param: $param_t) -> Self {
            self.$param = Some($param);
            self
        }
    };
}
macro_rules! add_config_opt {
    ($fn_name:ident, $param:ident, $param_t:ty) => {
        pub fn $fn_name(mut self, $param: $param_t) -> Self {
            self.config.$param = Some($param);
            self
        }
    };
}
impl<'a> WriteBuilder<'a> {
    pub fn new(file: File) -> Self {
        Self {
            wv: WriteId::new(file),
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
        let wv_ptr = (&mut self.wv as *mut WriteId) as *mut c_void;
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
            _config: config,
            is_flushed: true,
        };
        Ok(context)
    }
    pub fn add_wvc(mut self, file: File) -> Self {
        self.wvc = Some(WriteId::new(file));
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
    _wv: WriteId,
    _wvc: Option<WriteId>,
    _config: WavpackConfig,
    is_flushed: bool,
}
impl WriteContext {
    /// Pack the specified samples.
    ///
    /// Requires: `buffer.len() <= u32::MAX`
    pub fn pack_samples(&mut self, samples: &mut [i32]) -> Result<()> {
        let len = samples.len();
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

#[cfg(test)]
mod tests {
    // TODO
}
