use std::ffi::CStr;
use wavpack_sys::*;

/// Return the WavPack library version in a packed integer.
///
/// Bits 0-7 give the micro version, bits 8-15 give the minor version, and
/// bits 16-23 give the major version. As of this writing the version is 5.6.0.
/// ```
/// use wavpack::get_library_version;
/// let version = get_library_version();
/// assert_eq!(version, 0x050600);
/// ```
pub fn get_library_version() -> u32 {
    unsafe { WavpackGetLibraryVersion() }
}

/// Return the WavPack library version as a string.
///
/// As of this writing this is "5.6.0".
/// ```
/// use wavpack::get_library_version_string;
/// let version = get_library_version_string();
/// assert_eq!(version, "5.6.0");
/// ```
pub fn get_library_version_string() -> &'static str {
    let c_str = unsafe { CStr::from_ptr(WavpackGetLibraryVersionString()) };
    c_str.to_str().unwrap()
}
