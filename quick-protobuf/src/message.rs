//! A module to deserialize a `Message` as defined in a .proto file
//!
//! Creates the struct and implements a reader

use crate::errors::Result;
use crate::reader::BytesReader;
use crate::writer::{Writer, WriterBackend};

/// A trait to handle deserialization based on parsed `Field`s
pub trait MessageWrite: Sized {
    /// Writes `Self` into W writer
    fn write_message<W: WriterBackend>(&self, _: &mut Writer<W>) -> Result<()> {
        Ok(())
    }

    /// Computes necessary binary size of self once serialized in protobuf
    fn get_size(&self) -> usize {
        0
    }
}

/// A trait to handle deserialization from protocol buffers.
pub trait MessageRead<'a>: Sized {
    /// Constructs an instance of `Self` by reading from the given bytes
    /// via the given reader.
    ///
    /// It does NOT read message length first. If you want to read a variable
    /// length message, use `BytesReader::read_message` directly
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self>;
}

/// A trait to provide basic information about a given message
pub trait MessageInfo {
    /// Full message path, in form of Module.Message
    const PATH: &'static str;
}
