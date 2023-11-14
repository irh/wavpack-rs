use thiserror::Error;

/// Error type used throughout the crate
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum Error {
    /// Used for errors coming from `WavpackGetErrorMessage`
    #[error("{0}")]
    Message(String),
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
    #[error("The file is already closed")]
    AlreadyClosed,
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    NulError(#[from] std::ffi::NulError),
    #[error(transparent)]
    IntoStringError(#[from] std::ffi::IntoStringError),
}

pub type Result<T> = std::result::Result<T, Error>;
