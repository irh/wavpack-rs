#![doc = include_str!("../../README.md")]

mod error;
mod reader;
mod version;
mod writer;

pub use error::{Error, Result};
pub use reader::{TagData, WavPackReader, WavPackReaderBuilder};
pub use version::{get_library_version, get_library_version_string};
pub use writer::{WavPackWriter, WavPackWriterBuilder};
