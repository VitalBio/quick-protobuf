//! A module to handle all errors

/// An error enum
#[derive(Debug, Clone, Copy)]
pub enum Error {
    /// Io error
    Io,
    /// Utf8 Error
    Utf8(::core::str::Utf8Error),
    /// Deprecated feature (in protocol buffer specification)
    Deprecated(&'static str),
    /// Unknown wire type
    UnknownWireType(u8),
    /// Varint decoding error
    Varint,
    /// Error while parsing protocol buffer message
    /// Unexpected map tag
    Map(u8),
    /// Out of data when reading from or writing to a byte buffer
    UnexpectedEndOfBuffer,
    /// The supplied output buffer is not large enough to serialize the message
    OutputBufferTooSmall,
}

/// A wrapper for `Result<T, Error>`
pub type Result<T> = ::core::result::Result<T, Error>;

impl From<::core::str::Utf8Error> for Error {
    fn from(e: ::core::str::Utf8Error) -> Error {
        Error::Utf8(e)
    }
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Error::Io => write!(f, "IO error"),
            Error::Utf8(e) => write!(f, "{}", e),
            Error::Deprecated(feature) => write!(f, "Feature '{}' has been deprecated", feature),
            Error::UnknownWireType(e) => {
                write!(f, "Unknown wire type '{}', must be less than 6", e)
            }
            Error::Varint => write!(f, "Cannot decode varint"),
            Error::Map(tag) => write!(f, "Unexpected map tag: '{}', expecting 1 or 2", tag),
            Error::UnexpectedEndOfBuffer => write!(f, "Unexpected end of buffer"),
            Error::OutputBufferTooSmall => write!(f, "Output buffer too small"),
        }
    }
}
