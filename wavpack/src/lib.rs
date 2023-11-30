#![doc = include_str!(concat!("../", std::env!("CARGO_PKG_README")))]

mod error;
mod reader;
mod version;
mod writer;

pub use error::{Error, Result};
pub use reader::{TagData, WavpackRead, WavpackReader, WavpackReaderBuilder};
pub use version::{get_library_version, get_library_version_string};
pub use wavpack_sys::WavpackHeader;
pub use writer::{WavpackWrite, WavpackWriter, WavpackWriterBuilder};
