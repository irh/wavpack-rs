#![doc = include_str!("../../README.md")]

mod error;
mod reader;
mod version;
mod writer;

pub use error::{Error, Result};
pub use reader::{TagData, WavpackRead, WavpackReader, WavpackReaderBuilder};
pub use version::{get_library_version, get_library_version_string};
pub use writer::{WavpackWriter, WavpackWriterBuilder};
