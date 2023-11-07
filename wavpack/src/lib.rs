#![doc = include_str!("../../README.md")]

mod error;
mod reader;
mod version;
mod writer;

pub use error::{Error, Result};
pub use reader::{Context, ContextBuilder, TagData};
pub use version::{get_library_version, get_library_version_string};
pub use writer::{WriteBuilder, WriteContext};
