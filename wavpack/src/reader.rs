use crate::{Error, Result};
use std::{
    collections::BTreeMap,
    ffi::{c_char, CStr, CString},
    fmt,
    path::Path,
    ptr::NonNull,
};
use wavpack_sys::*;

/// A builder for [WavPackReader]s
///
/// usage
/// ```
/// use std::path::PathBuf;
/// use wavpack::WavPackReader;
///
/// let path = PathBuf::from("/path/to/foo.wv");
/// let reader = WavPackReader::builder(&path).tags().edit_tags().build();
/// ```
pub struct WavPackReaderBuilder<'a> {
    file_name: &'a Path,
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

impl<'a> WavPackReaderBuilder<'a> {
    pub fn build(self) -> Result<WavPackReader> {
        let file_name = CString::new(self.file_name.display().to_string())?;
        let mut error = vec![0 as c_char; 81]; // 80 chars + NUL
        let error_ptr = error.as_mut_ptr();
        let context = unsafe {
            WavpackOpenFileInput(
                file_name.as_ptr(),
                error_ptr,
                self.flags as i32,
                self.norm_offset.unwrap_or(0),
            )
        };
        match NonNull::new(context) {
            None => {
                let error = char_ptr_to_string(error_ptr)?;
                Err(Error::Message(error))
            }
            Some(context) => Ok(WavPackReader { context }),
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
pub struct WavPackReader {
    context: NonNull<WavpackContext>,
}

/// Reading WavPack Files
impl WavPackReader {
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

    /// Opens a WavPack file at the given path for reading
    ///
    /// See [`WavPackReaderBuilder`] for more advanced options.
    pub fn builder(file_name: &Path) -> WavPackReaderBuilder {
        WavPackReaderBuilder {
            file_name,
            flags: 0,
            norm_offset: None,
        }
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
}

impl Drop for WavPackReader {
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
