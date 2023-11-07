use crate::{Error, Result};
use ffi::*;
use std::{
    collections::BTreeMap,
    ffi::{c_char, CStr, CString},
    path::Path,
    ptr::NonNull,
};

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
        #[must_use]
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

    #[must_use]
    pub fn normalize(mut self, norm_offset: i32) -> Self {
        self.flag |= OPEN_NORMALIZE;
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
    def_fn!(get_bits_per_sample, WavpackGetBitsPerSample, i32);
    def_fn!(get_bytes_per_sample, WavpackGetBytesPerSample, i32);
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

/// Tag data
#[derive(Clone)]
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
}

impl Drop for Context {
    fn drop(&mut self) {
        let wpc = self.context.as_ptr();
        unsafe { WavpackCloseFile(wpc) };
    }
}
