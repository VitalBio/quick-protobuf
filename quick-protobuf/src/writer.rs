//! A module to manage protobuf serialization

use crate::errors::{Error, Result};
use crate::message::MessageWrite;
use crate::sizeofs::sizeof_varint;

use byteorder::{ByteOrder, LittleEndian as LE};
use heapless::Vec;

/// A struct to write protobuf messages
///
/// # Examples
///
/// ```rust
/// // an automatically generated module which is in a separate file in general
/// mod foo_bar {
///     # use quick_protobuf::{MessageWrite, Writer, WriterBackend, Result};
///     # use std::borrow::Cow;
///     pub struct Foo<'a> { pub name: Option<Cow<'a, str>>, }
///     pub struct Bar { pub id: Option<u32> }
///     pub struct FooBar<'a> { pub foos: Vec<Foo<'a>>, pub bars: Vec<Bar>, }
///     impl<'a> MessageWrite for FooBar<'a> {
///         // implements
///         // fn get_size(&self) -> usize { ... }
///         // fn write_message<W: WriterBackend>(&self, r: &mut Writer<W>) -> Result<()> { ... }
///         # fn get_size(&self) -> usize { 0 }
///         # fn write_message<W: WriterBackend>(&self, _: &mut Writer<W>) -> Result<()> { Ok(()) }
///     }
/// }
///
/// // FooBar is a message generated from a proto file
/// // in parcicular it contains a `write_message` function
/// use foo_bar::{FooBar, Foo, Bar};
/// use std::borrow::Cow;
/// use quick_protobuf::Writer;
///
/// fn main() {
///     // let mut r = File::create("...").expect("Cannot create file");
///     // for the sake of example, we'll use a simpler struct which impl `Write`
///     let mut r = Vec::new();
///     let mut writer = Writer::new(&mut r);
///
///     // manually generates a FooBar for the example
///     let foobar = FooBar {
///         foos: vec![Foo { name: Some(Cow::Borrowed("test!")) }, Foo { name: None }],
///         bars: vec![Bar { id: Some(43) }, Bar { id: None }],
///     };
///
///     // now using the generated module
///     writer.write_message(&foobar).expect("Cannot write FooBar");
/// }
/// ```
pub struct Writer<W: WriterBackend> {
    inner: W,
}

impl<W: WriterBackend> Writer<W> {
    /// Creates a new `ProtobufWriter`
    pub fn new(w: W) -> Writer<W> {
        Writer { inner: w }
    }

    /// Writes a byte which is NOT internally coded as a `varint`
    pub fn write_u8(&mut self, byte: u8) -> Result<()> {
        self.inner.pb_write_u8(byte)
    }

    /// Writes a `varint` (compacted `u64`)
    pub fn write_varint(&mut self, mut v: u64) -> Result<()> {
        while v > 0x7F {
            self.inner.pb_write_u8(((v as u8) & 0x7F) | 0x80)?;
            v >>= 7;
        }
        self.inner.pb_write_u8(v as u8)
    }

    /// Writes a tag, which represents both the field number and the wire type
    pub fn write_tag(&mut self, tag: u32) -> Result<()> {
        self.write_varint(tag as u64)
    }

    /// Writes a `int32` which is internally coded as a `varint`
    pub fn write_int32(&mut self, v: i32) -> Result<()> {
        self.write_varint(v as u64)
    }

    /// Writes a `int64` which is internally coded as a `varint`
    
    pub fn write_int64(&mut self, v: i64) -> Result<()> {
        self.write_varint(v as u64)
    }

    /// Writes a `uint32` which is internally coded as a `varint`
    pub fn write_uint32(&mut self, v: u32) -> Result<()> {
        self.write_varint(v as u64)
    }

    /// Writes a `uint64` which is internally coded as a `varint`
    pub fn write_uint64(&mut self, v: u64) -> Result<()> {
        self.write_varint(v)
    }

    /// Writes a `sint32` which is internally coded as a `varint`
    pub fn write_sint32(&mut self, v: i32) -> Result<()> {
        self.write_varint(((v << 1) ^ (v >> 31)) as u64)
    }

    /// Writes a `sint64` which is internally coded as a `varint`
    pub fn write_sint64(&mut self, v: i64) -> Result<()> {
        self.write_varint(((v << 1) ^ (v >> 63)) as u64)
    }

    /// Writes a `fixed64` which is little endian coded `u64`
    pub fn write_fixed64(&mut self, v: u64) -> Result<()> {
        self.inner.pb_write_u64(v)
    }

    /// Writes a `fixed32` which is little endian coded `u32`
    pub fn write_fixed32(&mut self, v: u32) -> Result<()> {
        self.inner.pb_write_u32(v)
    }

    /// Writes a `sfixed64` which is little endian coded `i64`
    pub fn write_sfixed64(&mut self, v: i64) -> Result<()> {
        self.inner.pb_write_i64(v)
    }

    /// Writes a `sfixed32` which is little endian coded `i32`
    pub fn write_sfixed32(&mut self, v: i32) -> Result<()> {
        self.inner.pb_write_i32(v)
    }

    /// Writes a `float`
    pub fn write_float(&mut self, v: f32) -> Result<()> {
        self.inner.pb_write_f32(v)
    }

    /// Writes a `double`
    pub fn write_double(&mut self, v: f64) -> Result<()> {
        self.inner.pb_write_f64(v)
    }

    /// Writes a `bool` 1 = true, 0 = false
    pub fn write_bool(&mut self, v: bool) -> Result<()> {
        self.inner.pb_write_u8(if v { 1 } else { 0 })
    }

    /// Writes an `enum` converting it to a `i32` first
    pub fn write_enum(&mut self, v: i32) -> Result<()> {
        self.write_int32(v)
    }

    /// Writes `bytes`: length first then the chunk of data
    pub fn write_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.write_varint(bytes.len() as u64)?;
        self.inner.pb_write_all(bytes)
    }

    /// Writes `string`: length first then the chunk of data
    pub fn write_string(&mut self, s: &str) -> Result<()> {
        self.write_bytes(s.as_bytes())
    }

    /// Writes packed repeated field: length first then the chunk of data
    pub fn write_packed<M, F, S>(&mut self, v: &[M], mut write: F, size: &S) -> Result<()>
    where
        F: FnMut(&mut Self, &M) -> Result<()>,
        S: Fn(&M) -> usize,
    {
        if v.is_empty() {
            return Ok(());
        }
        let len: usize = v.iter().map(|m| size(m)).sum();
        self.write_varint(len as u64)?;
        for m in v {
            write(self, m)?;
        }
        Ok(())
    }

    /// Writes packed repeated field when we know the size of items
    ///
    /// `item_size` is internally used to compute the total length
    /// As the length is fixed (and the same as rust internal representation, we can directly dump
    /// all data at once
    #[inline]
    pub fn write_packed_fixed<M>(&mut self, v: &[M]) -> Result<()> {
        let len = v.len() * ::core::mem::size_of::<M>();
        let bytes = unsafe { ::core::slice::from_raw_parts(v.as_ptr() as *const u8, len) };
        self.write_bytes(bytes)
    }

    /// Writes a message which implements `MessageWrite`
    pub fn write_message<M: MessageWrite>(&mut self, m: &M) -> Result<()> {
        let len = m.get_size();
        self.write_varint(len as u64)?;
        m.write_message(self)
    }

    /// Writes a message which implements `MessageWrite` without adding the length prefix
    #[cfg_attr(std, inline)]
    pub fn write_message_without_len<M: MessageWrite>(&mut self, m: &M) -> Result<()> {
        m.write_message(self)
    }

    /// Writes another item prefixed with tag
    pub fn write_with_tag<F>(&mut self, tag: u32, mut write: F) -> Result<()>
    where
        F: FnMut(&mut Self) -> Result<()>,
    {
        self.write_tag(tag)?;
        write(self)
    }
    
    /// Writes tag then repeated field
    ///
    /// If array is empty, then do nothing (do not even write the tag)
    pub fn write_packed_with_tag<M, F, S>(
        &mut self,
        tag: u32,
        v: &[M],
        mut write: F,
        size: &S,
    ) -> Result<()>
    where
        F: FnMut(&mut Self, &M) -> Result<()>,
        S: Fn(&M) -> usize,
    {
        if v.is_empty() {
            return Ok(());
        }
        self.write_tag(tag)?;
        let len: usize = v.iter().map(|m| size(m)).sum();
        self.write_varint(len as u64)?;
        for m in v {
            write(self, m)?;
        }
        Ok(())
    }
    
    /// Writes tag then repeated field
    ///
    /// If array is empty, then do nothing (do not even write the tag)
    pub fn write_packed_fixed_with_tag<M>(&mut self, tag: u32, v: &[M]) -> Result<()> {
        if v.is_empty() {
            return Ok(());
        }

        self.write_tag(tag)?;
        let len = ::core::mem::size_of::<M>() * v.len();
        let bytes = unsafe { ::core::slice::from_raw_parts(v.as_ptr() as *const u8, len) };
        self.write_bytes(bytes)
    }

    /// Writes tag then repeated field with fixed length item size
    ///
    /// If array is empty, then do nothing (do not even write the tag)
    pub fn write_packed_fixed_size_with_tag<M>(
        &mut self,
        tag: u32,
        v: &[M],
        item_size: usize,
    ) -> Result<()> {
        if v.is_empty() {
            return Ok(());
        }
        self.write_tag(tag)?;
        let len = v.len() * item_size;
        let bytes = unsafe { ::core::slice::from_raw_parts(v as *const [M] as *const M as *const u8, len) };
        self.write_bytes(bytes)
    }

    /// Write entire map
    pub fn write_map<FK, FV>(
        &mut self,
        size: usize,
        tag_key: u32,
        mut write_key: FK,
        tag_val: u32,
        mut write_val: FV,
    ) -> Result<()>
    where
        FK: FnMut(&mut Self) -> Result<()>,
        FV: FnMut(&mut Self) -> Result<()>,
    {
        self.write_varint(size as u64)?;
        self.write_tag(tag_key)?;
        write_key(self)?;
        self.write_tag(tag_val)?;
        write_val(self)
    }
}

/// Serialize a `MessageWrite` into a byte slice
pub fn serialize_into_slice<M: MessageWrite>(message: &M, out: &mut [u8]) -> Result<()> {
    let len = message.get_size();
    if out.len() < len {
        return Err(Error::OutputBufferTooSmall);
    }
    {
        let mut writer = Writer::new(BytesWriter::new(out));
        writer.write_message_without_len(message)?;
    }

    Ok(())
}

/// Serialize a `MessageWrite` into a u8 heapless::vec
pub fn serialize_into_vec<M: MessageWrite, const T: usize>(message: &M, out: &mut Vec<u8, T>) -> Result<()> {
    let len = message.get_size();
    if out.capacity() < len {
        return Err(Error::OutputBufferTooSmall);
    }
    out.resize_default(len).unwrap();
    {
        let mut writer = Writer::new(BytesWriter::new(out));
        writer.write_message_without_len(message)?;
    }

    Ok(())
}

/// Writer backend abstraction
pub trait WriterBackend {
    /// Write a u8
    fn pb_write_u8(&mut self, x: u8) -> Result<()>;

    /// Write a u32
    fn pb_write_u32(&mut self, x: u32) -> Result<()>;

    /// Write a i32
    fn pb_write_i32(&mut self, x: i32) -> Result<()>;

    /// Write a f32
    fn pb_write_f32(&mut self, x: f32) -> Result<()>;

    /// Write a u64
    fn pb_write_u64(&mut self, x: u64) -> Result<()>;

    /// Write a i64
    fn pb_write_i64(&mut self, x: i64) -> Result<()>;

    /// Write a f64
    fn pb_write_f64(&mut self, x: f64) -> Result<()>;

    /// Write all bytes in buf
    fn pb_write_all(&mut self, buf: &[u8]) -> Result<()>;
}

/// A writer backend for byte buffers
pub struct BytesWriter<'a> {
    buf: &'a mut [u8],
    cursor: usize,
}

impl<'a> BytesWriter<'a> {
    /// Create a new BytesWriter to write into `buf`
    pub fn new(buf: &'a mut [u8]) -> BytesWriter<'a> {
        BytesWriter { buf, cursor: 0 }
    }
}

impl<'a> WriterBackend for BytesWriter<'a> {
    fn pb_write_u8(&mut self, x: u8) -> Result<()> {
        if self.buf.len() - self.cursor < 1 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            self.buf[self.cursor] = x;
            self.cursor += 1;
            Ok(())
        }
    }

    fn pb_write_u32(&mut self, x: u32) -> Result<()> {
        if self.buf.len() - self.cursor < 4 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            LE::write_u32(&mut self.buf[self.cursor..], x);
            self.cursor += 4;
            Ok(())
        }
    }

    fn pb_write_i32(&mut self, x: i32) -> Result<()> {
        if self.buf.len() - self.cursor < 4 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            LE::write_i32(&mut self.buf[self.cursor..], x);
            self.cursor += 4;
            Ok(())
        }
    }

    fn pb_write_f32(&mut self, x: f32) -> Result<()> {
        if self.buf.len() - self.cursor < 4 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            LE::write_f32(&mut self.buf[self.cursor..], x);
            self.cursor += 4;
            Ok(())
        }
    }

    fn pb_write_u64(&mut self, x: u64) -> Result<()> {
        if self.buf.len() - self.cursor < 8 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            LE::write_u64(&mut self.buf[self.cursor..], x);
            self.cursor += 8;
            Ok(())
        }
    }

    fn pb_write_i64(&mut self, x: i64) -> Result<()> {
        if self.buf.len() - self.cursor < 8 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            LE::write_i64(&mut self.buf[self.cursor..], x);
            self.cursor += 8;
            Ok(())
        }
    }

    fn pb_write_f64(&mut self, x: f64) -> Result<()> {
        if self.buf.len() - self.cursor < 8 {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            LE::write_f64(&mut self.buf[self.cursor..], x);
            self.cursor += 8;
            Ok(())
        }
    }

    fn pb_write_all(&mut self, buf: &[u8]) -> Result<()> {
        if self.buf.len() - self.cursor < buf.len() {
            Err(Error::UnexpectedEndOfBuffer)
        } else {
            self.buf[self.cursor..(self.cursor + buf.len())].copy_from_slice(buf);
            self.cursor += buf.len();
            Ok(())
        }
    }
}