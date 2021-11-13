use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};

use log::debug;

use crate::errors::{Error, Result};
use crate::keywords::sanitize_keyword;
use crate::parser::file_descriptor;

fn sizeof_varint(v: u32) -> usize {
    match v {
        0x0..=0x7F => 1,
        0x80..=0x3FFF => 2,
        0x4000..=0x1F_FFFF => 3,
        0x20_0000..=0xFFF_FFFF => 4,
        _ => 5,
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Syntax {
    Proto2,
    Proto3,
}

impl Default for Syntax {
    fn default() -> Syntax {
        Syntax::Proto2
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Frequency {
    Optional,
    Required,
}

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct MessageIndex {
    indexes: Vec<usize>,
}

impl fmt::Debug for MessageIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
        f.debug_set().entries(self.indexes.iter()).finish()
    }
}

impl MessageIndex {
    pub fn get_message<'a>(&self, desc: &'a FileDescriptor) -> &'a Message {
        let first_message = self.indexes.first().and_then(|i| desc.messages.get(*i));
        self.indexes
            .iter()
            .skip(1)
            .fold(first_message, |cur, next| {
                cur.and_then(|msg| msg.messages.get(*next))
            })
            .expect("Message index not found")
    }

    fn push(&mut self, i: usize) {
        self.indexes.push(i);
    }

    fn pop(&mut self) {
        self.indexes.pop();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct EnumIndex {
    msg_index: MessageIndex,
    index: usize,
}

impl EnumIndex {
    pub fn get_enum<'a>(&self, desc: &'a FileDescriptor) -> &'a Enumerator {
        let enums = if self.msg_index.indexes.is_empty() {
            &desc.enums
        } else {
            &self.msg_index.get_message(desc).enums
        };
        enums.get(self.index).expect("Enum index not found")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldType {
    Int32,
    Int64,
    Uint32,
    Uint64,
    Sint32,
    Sint64,
    Bool,
    Enum(EnumIndex),
    Fixed64,
    Sfixed64,
    Double,
    Message(MessageIndex),
    MessageOrEnum(String),
    Fixed32,
    Sfixed32,
    Float,
}

impl FieldType {
    pub fn is_primitive(&self) -> bool {
        match *self {
            FieldType::Message(_) => false,
            _ => true,
        }
    }

    fn wire_type_num(&self, packed: bool) -> u32 {
        if packed {
            2
        } else {
            self.wire_type_num_non_packed()
        }
    }

    fn wire_type_num_non_packed(&self) -> u32 {
        /*
        0	Varint	int32, int64, uint32, uint64, sint32, sint64, bool, enum
        1	64-bit	fixed64, sfixed64, double
        2	Length-delimited	embedded messages
        3	Start group	groups (deprecated)
        4	End group	groups (deprecated)
        5	32-bit	fixed32, sfixed32, float
        */
        match *self {
            FieldType::Int32
            | FieldType::Sint32
            | FieldType::Int64
            | FieldType::Sint64
            | FieldType::Uint32
            | FieldType::Uint64
            | FieldType::Bool
            | FieldType::Enum(_) => 0,
            FieldType::Fixed64 | FieldType::Sfixed64 | FieldType::Double => 1,
            | FieldType::Message(_) => 2,
            FieldType::Fixed32 | FieldType::Sfixed32 | FieldType::Float => 5,
            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
        }
    }

    fn proto_type(&self) -> &str {
        match *self {
            FieldType::Int32 => "int32",
            FieldType::Sint32 => "sint32",
            FieldType::Int64 => "int64",
            FieldType::Sint64 => "sint64",
            FieldType::Uint32 => "uint32",
            FieldType::Uint64 => "uint64",
            FieldType::Bool => "bool",
            FieldType::Enum(_) => "enum",
            FieldType::Fixed32 => "fixed32",
            FieldType::Sfixed32 => "sfixed32",
            FieldType::Float => "float",
            FieldType::Fixed64 => "fixed64",
            FieldType::Sfixed64 => "sfixed64",
            FieldType::Double => "double",
            FieldType::Message(_) => "message",
            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
        }
    }

    fn is_fixed_size(&self) -> bool {
        match self.wire_type_num_non_packed() {
            1 | 5 => true,
            _ => false,
        }
    }

    fn regular_default<'a, 'b>(&'a self, desc: &'b FileDescriptor) -> Option<&'b str> {
        match *self {
            FieldType::Int32 => Some("0i32"),
            FieldType::Sint32 => Some("0i32"),
            FieldType::Int64 => Some("0i64"),
            FieldType::Sint64 => Some("0i64"),
            FieldType::Uint32 => Some("0u32"),
            FieldType::Uint64 => Some("0u64"),
            FieldType::Bool => Some("false"),
            FieldType::Fixed32 => Some("0u32"),
            FieldType::Sfixed32 => Some("0i32"),
            FieldType::Float => Some("0f32"),
            FieldType::Fixed64 => Some("0u64"),
            FieldType::Sfixed64 => Some("0i64"),
            FieldType::Double => Some("0f64"),
            FieldType::Enum(ref e) => {
                let e = e.get_enum(desc);
                Some(&*e.fully_qualified_fields[0].0)
            }
            FieldType::Message(_) => None,
            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
        }
    }

    pub fn message(&self) -> Option<&MessageIndex> {
        if let FieldType::Message(ref m) = self {
            Some(m)
        } else {
            None
        }
    }

    fn has_lifetime(
        &self,
        desc: &FileDescriptor,
        packed: bool,
        ignore: &mut Vec<MessageIndex>,
    ) -> bool {
        match *self {
            FieldType::Message(ref m) => m.get_message(desc).has_lifetime(desc, ignore),
            FieldType::Fixed64
            | FieldType::Sfixed64
            | FieldType::Double
            | FieldType::Fixed32
            | FieldType::Sfixed32
            | FieldType::Float => packed,
            _ => false,
        }
    }

    fn rust_type(&self, desc: &FileDescriptor) -> Result<String> {
        Ok(match *self {
            FieldType::Int32 | FieldType::Sint32 | FieldType::Sfixed32 => "i32".to_string(),
            FieldType::Int64 | FieldType::Sint64 | FieldType::Sfixed64 => "i64".to_string(),
            FieldType::Uint32 | FieldType::Fixed32 => "u32".to_string(),
            FieldType::Uint64 | FieldType::Fixed64 => "u64".to_string(),
            FieldType::Double => "f64".to_string(),
            FieldType::Float => "f32".to_string(),
            FieldType::Bool => "bool".to_string(),
            FieldType::Enum(ref e) => {
                let e = e.get_enum(desc);
                format!("{}{}", e.get_modules(desc), e.name)
            }
            FieldType::Message(ref msg) => {
                let m = msg.get_message(desc);
                let lifetime = if m.has_lifetime(desc, &mut Vec::new()) {
                    "<'a>"
                } else {
                    ""
                };
                format!("{}{}{}", m.get_modules(desc), m.name, lifetime)
            },
            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
        })
    }

    /// Returns the relevant function to read the data
    fn read_fn(&self, desc: &FileDescriptor) -> Result<String> {
        Ok(match *self {
            FieldType::Message(ref msg) => {
                let m = msg.get_message(desc);
                format!(
                    "r.read_message::<{}{}>(bytes)?",
                    m.get_modules(desc),
                    m.name
                )
            }
            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
            _ => {
                format!("r.read_{}(bytes)?", self.proto_type())
            }
        })
    }

    fn get_size(&self, s: &str) -> String {
        match *self {
            FieldType::Int32
            | FieldType::Int64
            | FieldType::Uint32
            | FieldType::Uint64
            | FieldType::Bool
            | FieldType::Enum(_) => format!("sizeof_varint(*({}) as u64)", s),
            FieldType::Sint32 => format!("sizeof_sint32(*({}))", s),
            FieldType::Sint64 => format!("sizeof_sint64(*({}))", s),

            FieldType::Fixed64 | FieldType::Sfixed64 | FieldType::Double => "8".to_string(),
            FieldType::Fixed32 | FieldType::Sfixed32 | FieldType::Float => "4".to_string(),

            FieldType::Message(_) => format!("sizeof_len(({}).get_size())", s),

            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
        }
    }

    fn get_write(&self, s: &str) -> String {
        match *self {
            FieldType::Enum(_) => format!("write_enum(*{} as i32)", s),

            FieldType::Int32
            | FieldType::Sint32
            | FieldType::Int64
            | FieldType::Sint64
            | FieldType::Uint32
            | FieldType::Uint64
            | FieldType::Bool
            | FieldType::Fixed64
            | FieldType::Sfixed64
            | FieldType::Double
            | FieldType::Fixed32
            | FieldType::Sfixed32
            | FieldType::Float => format!("write_{}(*{})", self.proto_type(), s),

            FieldType::Message(_) => format!("write_message({})", s),

            FieldType::MessageOrEnum(_) => unreachable!("Message / Enum not resolved"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub frequency: Frequency,
    pub typ: FieldType,
    pub number: i32,
    pub default: Option<String>,
    pub packed: Option<bool>,
    pub deprecated: bool,
}

impl Field {
    fn packed(&self) -> bool {
        self.packed.unwrap_or(false)
    }

    fn sanitize_default(&mut self, desc: &FileDescriptor) -> Result<()> {
        if let Some(ref mut d) = self.default {
            *d = match &*self.typ.rust_type(desc)? {
                "u32" => format!("{}u32", *d),
                "u64" => format!("{}u64", *d),
                "i32" => format!("{}i32", *d),
                "i64" => format!("{}i64", *d),
                "f32" => match &*d.to_lowercase() {
                    "inf" => "::core::f32::INFINITY".to_string(),
                    "-inf" => "::core::f32::NEG_INFINITY".to_string(),
                    "nan" => "::core::f32::NAN".to_string(),
                    _ => format!("{}f32", *d),
                },
                "f64" => match &*d.to_lowercase() {
                    "inf" => "::core::f64::INFINITY".to_string(),
                    "-inf" => "::core::f64::NEG_INFINITY".to_string(),
                    "nan" => "::core::f64::NAN".to_string(),
                    _ => format!("{}f64", *d),
                },
                "bool" => format!("{}", d.parse::<bool>().unwrap()),
                e => format!("{}::{}", e, d), // enum, as message and map do not have defaults
            }
        }
        Ok(())
    }

    fn has_regular_default(&self, desc: &FileDescriptor) -> bool {
        self.default.is_none()
            || self.default.as_ref().map(|d| &**d) == self.typ.regular_default(desc)
    }

    fn tag(&self) -> u32 {
        tag(self.number as u32, &self.typ, self.packed())
    }

    fn write_definition<W: Write>(
        &self,
        w: &mut W,
        desc: &FileDescriptor,
        config: &Config,
    ) -> Result<()> {
        if self.deprecated {
            if config.add_deprecated_fields {
                writeln!(w, "    #[deprecated]")?;
            } else {
                return Ok(())
            }
        }
        write!(w, "    pub {}: ", self.name)?;
        let rust_type = self.typ.rust_type(desc)?;
        match self.frequency {
            Frequency::Optional
                if desc.syntax == Syntax::Proto2 && self.default.is_none()
                    || self.typ.message().is_some() =>
            {
                writeln!(w, "Option<{}>,", rust_type)?
            }
            Frequency::Required | Frequency::Optional => writeln!(w, "{},", rust_type)?,
        }
        Ok(())
    }

    fn write_match_tag<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        if self.deprecated && !config.add_deprecated_fields {
            return Ok(());
        }

        let val = self.typ.read_fn(desc)?;
        let name = &self.name;
        write!(w, "                Ok({}) => ", self.tag())?;
        match self.frequency {
            Frequency::Optional
                if desc.syntax == Syntax::Proto2 && self.default.is_none()
                    || self.typ.message().is_some() =>
            {
                writeln!(w, "msg.{} = Some({}),", name, val)?
            }
            Frequency::Required | Frequency::Optional => {
                writeln!(w, "msg.{} = {},", name, val)?
            }
        }
        Ok(())
    }

    fn write_get_size<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        if self.deprecated && !config.add_deprecated_fields {
            return Ok(());
        }

        write!(w, "        + ")?;
        let tag_size = sizeof_varint(self.tag());
        match self.frequency {
            Frequency::Optional
                if desc.syntax == Syntax::Proto2 || self.typ.message().is_some() =>
            {
                // TODO this might be incorrect behavior for proto2
                match self.default.as_ref() {
                    None => {
                        write!(w, "self.{}.as_ref().map_or(0, ", self.name)?;
                        if self.typ.is_fixed_size() {
                            writeln!(w, "|_| {} + {})", tag_size, self.typ.get_size(""))?;
                        } else {
                            writeln!(w, "|m| {} + {})", tag_size, self.typ.get_size("m"))?;
                        }
                    }
                    Some(d) => {
                        writeln!(
                            w,
                            "if self.{} == {} {{ 0 }} else {{ {} + {} }}",
                            self.name,
                            d,
                            tag_size,
                            self.typ.get_size(&format!("&self.{}", self.name))
                        )?;
                    }
                }
            }
            Frequency::Optional => match self.typ {
                _ => writeln!(
                    w,
                    "if self.{} == {} {{ 0 }} else {{ {} + {} }}",
                    self.name,
                    self.default.as_ref().map_or_else(
                        || self.typ.regular_default(desc).unwrap_or("None"),
                        |s| s.as_str()
                    ),
                    tag_size,
                    self.typ.get_size(&format!("&self.{}", self.name))
                )?,
            },
            Frequency::Required => writeln!(
                w,
                "{} + {}",
                tag_size,
                self.typ.get_size(&format!("&self.{}", self.name))
            )?,
        }
        Ok(())
    }

    fn write_write<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        if self.deprecated && !config.add_deprecated_fields {
            return Ok(());
        }

        match self.frequency {
            Frequency::Optional
                if desc.syntax == Syntax::Proto2 || self.typ.message().is_some() =>
            {
                match self.default.as_ref() {
                    None => {
                        writeln!(
                            w,
                            "        if let Some(ref s) = \
                             self.{} {{ w.write_with_tag({}, |w| w.{})?; }}",
                            self.name,
                            self.tag(),
                            self.typ.get_write("s")
                        )?;
                    }
                    Some(d) => {
                        writeln!(
                            w,
                            "        if self.{} != {} {{ w.write_with_tag({}, |w| w.{})?; }}",
                            self.name,
                            d,
                            self.tag(),
                            self.typ
                                .get_write(&format!("&self.{}", self.name))
                        )?;
                    }
                }
            }
            Frequency::Optional => match self.typ {
                _ => {
                    writeln!(
                        w,
                        "        if self.{} != {} {{ w.write_with_tag({}, |w| w.{})?; }}",
                        self.name,
                        self.default.as_ref().map_or_else(
                            || self.typ.regular_default(desc).unwrap_or("None"),
                            |s| s.as_str()
                        ),
                        self.tag(),
                        self.typ
                            .get_write(&format!("&self.{}", self.name))
                    )?;
                }
            },
            Frequency::Required => {
                writeln!(
                    w,
                    "        w.write_with_tag({}, |w| w.{})?;",
                    self.tag(),
                    self.typ
                        .get_write(&format!("&self.{}", self.name))
                )?;
            }
        }
        Ok(())
    }
}

fn get_modules(module: &str, imported: bool, desc: &FileDescriptor) -> String {
    let skip = if desc.package.is_empty() && !imported {
        1
    } else {
        0
    };
    module
        .split('.')
        .filter(|p| !p.is_empty())
        .skip(skip)
        .map(|p| format!("{}::", p))
        .collect()
}

#[derive(Debug, Clone, Default)]
pub struct Message {
    pub name: String,
    pub fields: Vec<Field>,
    pub oneofs: Vec<OneOf>,
    pub reserved_nums: Option<Vec<i32>>,
    pub reserved_names: Option<Vec<String>>,
    pub imported: bool,
    pub package: String,        // package from imports + nested items
    pub messages: Vec<Message>, // nested messages
    pub enums: Vec<Enumerator>, // nested enums
    pub module: String,         // 'package' corresponding to actual generated Rust module
    pub path: PathBuf,
    pub import: PathBuf,
    pub index: MessageIndex,
}

impl Message {
    fn convert_field_types(&mut self, from: &FieldType, to: &FieldType) {
        for f in self.all_fields_mut().filter(|f| f.typ == *from) {
            f.typ = to.clone();
        }

        for message in &mut self.messages {
            message.convert_field_types(from, to);
        }
    }

    fn has_lifetime(&self, desc: &FileDescriptor, ignore: &mut Vec<MessageIndex>) -> bool {
        if ignore.contains(&&self.index) {
            return false;
        }
        ignore.push(self.index.clone());
        let res = self
            .all_fields()
            .any(|f| f.typ.has_lifetime(desc, f.packed(), ignore));
        ignore.pop();
        res
    }

    fn set_imported(&mut self) {
        self.imported = true;
        for o in self.oneofs.iter_mut() {
            o.imported = true;
        }
        for m in self.messages.iter_mut() {
            m.set_imported();
        }
        for e in self.enums.iter_mut() {
            e.imported = true;
        }
    }

    fn get_modules(&self, desc: &FileDescriptor) -> String {
        get_modules(&self.module, self.imported, desc)
    }

    fn is_unit(&self) -> bool {
        self.fields.is_empty() && self.oneofs.is_empty()
    }

    fn write<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        println!("Writing message {}{}", self.get_modules(desc), self.name);
        writeln!(w)?;

        self.write_definition(w, desc, config)?;
        writeln!(w)?;
        self.write_impl_message_read(w, desc, config)?;
        writeln!(w)?;
        self.write_impl_message_write(w, desc, config)?;

        if config.gen_info {
            self.write_impl_message_info(w, desc)?;
            writeln!(w)?;
        }

        if !(self.messages.is_empty() && self.enums.is_empty() && self.oneofs.is_empty()) {
            writeln!(w)?;
            writeln!(w, "pub mod mod_{} {{", self.name)?;
            writeln!(w)?;
            if !self.messages.is_empty() || !self.oneofs.is_empty() {
                writeln!(w, "use super::*;")?;
            }
            for m in &self.messages {
                m.write(w, desc, config)?;
            }
            for e in &self.enums {
                e.write(w)?;
            }
            for o in &self.oneofs {
                o.write(w, desc, config)?;
            }

            writeln!(w)?;
            writeln!(w, "}}")?;
        }

        Ok(())
    }

    fn write_definition<W: Write>(
        &self,
        w: &mut W,
        desc: &FileDescriptor,
        config: &Config,
    ) -> Result<()> {
        let mut custom_struct_derive = config.custom_struct_derive.join(", ");
        if !custom_struct_derive.is_empty() {
            custom_struct_derive += ", ";
        }

        writeln!(
            w,
            "#[derive({}Debug, Default, PartialEq, Clone)]",
            custom_struct_derive
        )?;

        if let Some(repr) = &config.custom_repr {
            writeln!(w, "#[repr({})]", repr)?;
        }

        if self.is_unit() {
            writeln!(w, "pub struct {} {{ }}", self.name)?;
            return Ok(());
        }

        let mut ignore = Vec::new();
        ignore.push(self.index.clone());
        if self.has_lifetime(desc, &mut ignore) {
            writeln!(w, "pub struct {}<'a> {{", self.name)?;
        } else {
            writeln!(w, "pub struct {} {{", self.name)?;
        }
        for f in &self.fields {
            f.write_definition(w, desc, config)?;
        }
        for o in &self.oneofs {
            o.write_message_definition(w, desc)?;
        }
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_impl_message_info<W: Write>(
        &self,
        w: &mut W,
        desc: &FileDescriptor,
    ) -> Result<()> {
        let mut ignore = Vec::new();
        ignore.push(self.index.clone());
        if self.has_lifetime(desc, &mut ignore) {
            writeln!(w, "impl<'a> MessageInfo for {}<'a> {{", self.name)?;
        } else {
            writeln!(w, "impl MessageInfo for {} {{", self.name)?;
        }
        writeln!(
            w,
            "    const PATH : &'static str = \"{}.{}\";",
            self.module, self.name
        )?;
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_impl_message_read<W: Write>(
        &self,
        w: &mut W,
        desc: &FileDescriptor,
        config: &Config,
    ) -> Result<()> {
        if self.is_unit() {
            writeln!(w, "impl<'a> MessageRead<'a> for {} {{", self.name)?;
            writeln!(
                w,
                "    fn from_reader(r: &mut BytesReader, _: &[u8]) -> Result<Self> {{"
            )?;
            writeln!(w, "        r.read_to_end();")?;
            writeln!(w, "        Ok(Self::default())")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
            return Ok(());
        }

        let mut ignore = Vec::new();
        ignore.push(self.index.clone());
        if self.has_lifetime(desc, &mut ignore) {
            writeln!(w, "impl<'a> MessageRead<'a> for {}<'a> {{", self.name)?;
            writeln!(
                w,
                "    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {{"
            )?;
        } else {
            writeln!(w, "impl<'a> MessageRead<'a> for {} {{", self.name)?;
            writeln!(
                w,
                "    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {{"
            )?;
        }

        let unregular_defaults = self
            .fields
            .iter()
            .filter(|f| !f.has_regular_default(desc))
            .collect::<Vec<_>>();
        if unregular_defaults.is_empty() {
            writeln!(w, "        let mut msg = Self::default();")?;
        } else {
            writeln!(w, "        let mut msg = {} {{", self.name)?;
            for f in unregular_defaults {
                writeln!(
                    w,
                    "            {}: {},",
                    f.name,
                    f.default.as_ref().unwrap()
                )?;
            }
            writeln!(w, "            ..Self::default()")?;
            writeln!(w, "        }};")?;
        }
        writeln!(w, "        while !r.is_eof() {{")?;
        writeln!(w, "            match r.next_tag(bytes) {{")?;
        for f in &self.fields {
            f.write_match_tag(w, desc, config)?;
        }
        for o in &self.oneofs {
            o.write_match_tag(w, desc, config)?;
        }
        writeln!(
            w,
            "                Ok(t) => {{ r.read_unknown(bytes, t)?; }}"
        )?;
        writeln!(w, "                Err(e) => return Err(e),")?;
        writeln!(w, "            }}")?;
        writeln!(w, "        }}")?;
        writeln!(w, "        Ok(msg)")?;
        writeln!(w, "    }}")?;
        writeln!(w, "}}")?;

        // TODO: write impl default when special default?
        // alternatively set the default value directly when reading

        Ok(())
    }

    fn write_impl_message_write<W: Write>(
        &self,
        w: &mut W,
        desc: &FileDescriptor,
        config: &Config,
    ) -> Result<()> {
        if self.is_unit() {
            writeln!(w, "impl MessageWrite for {} {{ }}", self.name)?;
            return Ok(());
        }

        let mut ignore = Vec::new();
        ignore.push(self.index.clone());
        if self.has_lifetime(desc, &mut ignore) {
            writeln!(w, "impl<'a> MessageWrite for {}<'a> {{", self.name)?;
        } else {
            writeln!(w, "impl MessageWrite for {} {{", self.name)?;
        }
        self.write_get_size(w, desc, config)?;
        writeln!(w)?;
        self.write_write_message(w, desc, config)?;
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_get_size<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        writeln!(w, "    fn get_size(&self) -> usize {{")?;
        writeln!(w, "        0")?;
        for f in &self.fields {
            f.write_get_size(w, desc, config)?;
        }
        for o in self.oneofs.iter() {
            o.write_get_size(w, desc, config)?;
        }
        writeln!(w, "    }}")?;
        Ok(())
    }

    fn write_write_message<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        writeln!(
            w,
            "    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {{"
        )?;
        for f in &self.fields {
            f.write_write(w, desc, config)?;
        }
        for o in &self.oneofs {
            o.write_write(w, desc, config)?;
        }
        writeln!(w, "        Ok(())")?;
        writeln!(w, "    }}")?;
        Ok(())
    }

    fn sanity_checks(&self, desc: &FileDescriptor) -> Result<()> {
        for f in self.all_fields() {
            // check reserved
            if self
                .reserved_names
                .as_ref()
                .map_or(false, |names| names.contains(&f.name))
                || self
                    .reserved_nums
                    .as_ref()
                    .map_or(false, |nums| nums.contains(&f.number))
            {
                return Err(Error::InvalidMessage(format!(
                    "Error in message {}\n\
                     Field {:?} conflict with reserved fields",
                    self.name, f
                )));
            }

            // check default enums
            if let Some(var) = f.default.as_ref() {
                if let FieldType::Enum(ref e) = f.typ {
                    let e = e.get_enum(desc);
                    e.fields.iter().find(|&(ref name, _)| name == var)
                    .ok_or_else(|| Error::InvalidDefaultEnum(format!(
                                "Error in message {}\n\
                                Enum field {:?} has a default value '{}' which is not valid for enum index {:?}",
                                self.name, f, var, e)))?;
                }
            }
        }
        Ok(())
    }

    fn set_package(&mut self, package: &str, module: &str) {
        // The complication here is that the _package_ (as declared in the proto file) does
        // not directly map to the _module_. For example, the package 'a.A' where A is a
        // message will be the module 'a.mod_A', since we can't reuse the message name A as
        // the submodule containing nested items. Also, protos with empty packages always
        // have a module corresponding to the file name.
        let (child_package, child_module) = if package.is_empty() && module.is_empty() {
            (self.name.clone(), format!("mod_{}", self.name))
        } else if package.is_empty() {
            self.module = module.to_string();
            (self.name.clone(), format!("{}.mod_{}", module, self.name))
        } else {
            self.package = package.to_string();
            self.module = module.to_string();
            (
                format!("{}.{}", package, self.name),
                format!("{}.mod_{}", module, self.name),
            )
        };

        for m in &mut self.messages {
            m.set_package(&child_package, &child_module);
        }
        for m in &mut self.enums {
            m.set_package(&child_package, &child_module);
        }
        for m in &mut self.oneofs {
            m.set_package(&child_package, &child_module);
        }
    }

    fn unset_packed_non_primitives(&mut self) {
        for f in self.all_fields_mut() {
            if !f.typ.is_primitive() && f.packed.is_some() {
                f.packed = None;
            }
        }
    }

    fn sanitize_defaults(&mut self, desc: &FileDescriptor) -> Result<()> {
        for f in self.all_fields_mut() {
            f.sanitize_default(desc)?;
        }
        for m in &mut self.messages {
            m.sanitize_defaults(desc)?;
        }
        Ok(())
    }

    fn sanitize_names(&mut self) {
        sanitize_keyword(&mut self.name);
        sanitize_keyword(&mut self.package);
        for f in self.fields.iter_mut() {
            sanitize_keyword(&mut f.name);
        }
        for m in &mut self.messages {
            m.sanitize_names();
        }
        for e in &mut self.enums {
            e.sanitize_names();
        }
        for o in &mut self.oneofs {
            o.sanitize_names();
        }
    }

    /// Return an iterator producing references to all the `Field`s of `self`,
    /// including both direct and `oneof` fields.
    pub fn all_fields(&self) -> impl Iterator<Item = &Field> {
        self.fields
            .iter()
            .chain(self.oneofs.iter().flat_map(|o| o.fields.iter()))
    }

    /// Return an iterator producing mutable references to all the `Field`s of
    /// `self`, including both direct and `oneof` fields.
    fn all_fields_mut(&mut self) -> impl Iterator<Item = &mut Field> {
        self.fields
            .iter_mut()
            .chain(self.oneofs.iter_mut().flat_map(|o| o.fields.iter_mut()))
    }
}

#[derive(Debug, Clone, Default)]
pub struct RpcFunctionDeclaration {
    pub name: String,
    pub arg: String,
    pub ret: String,
}

#[derive(Debug, Clone, Default)]
pub struct RpcService {
    pub service_name: String,
    pub functions: Vec<RpcFunctionDeclaration>,
}

impl RpcService {
    fn write_definition<W: Write>(&self, w: &mut W, config: &Config) -> Result<()> {
        (config.custom_rpc_generator)(self, w)
    }
}

pub type RpcGeneratorFunction = Box<dyn Fn(&RpcService, &mut dyn Write) -> Result<()>>;

#[derive(Debug, Clone, Default)]
pub struct Enumerator {
    pub name: String,
    pub fields: Vec<(String, i32)>,
    pub fully_qualified_fields: Vec<(String, i32)>,
    pub partially_qualified_fields: Vec<(String, i32)>,
    pub imported: bool,
    pub package: String,
    pub module: String,
    pub path: PathBuf,
    pub import: PathBuf,
    pub index: EnumIndex,
}

impl Enumerator {
    fn set_package(&mut self, package: &str, module: &str) {
        self.package = package.to_string();
        self.module = module.to_string();
        self.partially_qualified_fields = self
            .fields
            .iter()
            .map(|f| (format!("{}::{}", &self.name, f.0), f.1))
            .collect();
        self.fully_qualified_fields = self
            .partially_qualified_fields
            .iter()
            .map(|pqf| {
                let fqf = if self.module.is_empty() {
                    pqf.0.clone()
                } else {
                    format!("{}::{}", self.module.replace(".", "::"), pqf.0)
                };
                (fqf, pqf.1)
            })
            .collect();
    }

    fn sanitize_names(&mut self) {
        sanitize_keyword(&mut self.name);
        sanitize_keyword(&mut self.package);
        for f in self.fields.iter_mut() {
            sanitize_keyword(&mut f.0);
        }
    }

    fn get_modules(&self, desc: &FileDescriptor) -> String {
        get_modules(&self.module, self.imported, desc)
    }

    fn write<W: Write>(&self, w: &mut W) -> Result<()> {
        println!("Writing enum {}", self.name);
        writeln!(w)?;
        self.write_definition(w)?;
        writeln!(w)?;
        if self.fields.is_empty() {
            Ok(())
        } else {
            self.write_impl_default(w)?;
            writeln!(w)?;
            self.write_from_i32(w)?;
            writeln!(w)?;
            self.write_from_str(w)
        }
    }

    fn write_definition<W: Write>(&self, w: &mut W) -> Result<()> {
        writeln!(w, "#[derive(Debug, PartialEq, Eq, Clone, Copy)]")?;
        writeln!(w, "pub enum {} {{", self.name)?;
        for &(ref f, ref number) in &self.fields {
            writeln!(w, "    {} = {},", f, number)?;
        }
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_impl_default<W: Write>(&self, w: &mut W) -> Result<()> {
        writeln!(w, "impl Default for {} {{", self.name)?;
        writeln!(w, "    fn default() -> Self {{")?;
        // TODO: check with default field and return error if there is no field
        writeln!(w, "        {}", self.partially_qualified_fields[0].0)?;
        writeln!(w, "    }}")?;
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_from_i32<W: Write>(&self, w: &mut W) -> Result<()> {
        writeln!(w, "impl From<i32> for {} {{", self.name)?;
        writeln!(w, "    fn from(i: i32) -> Self {{")?;
        writeln!(w, "        match i {{")?;
        for &(ref f, ref number) in &self.fields {
            writeln!(w, "            {} => {}::{},", number, self.name, f)?;
        }
        writeln!(w, "            _ => Self::default(),")?;
        writeln!(w, "        }}")?;
        writeln!(w, "    }}")?;
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_from_str<W: Write>(&self, w: &mut W) -> Result<()> {
        writeln!(w, "impl<'a> From<&'a str> for {} {{", self.name)?;
        writeln!(w, "    fn from(s: &'a str) -> Self {{")?;
        writeln!(w, "        match s {{")?;
        for &(ref f, _) in &self.fields {
            writeln!(w, "            {:?} => {}::{},", f, self.name, f)?;
        }
        writeln!(w, "            _ => Self::default(),")?;
        writeln!(w, "        }}")?;
        writeln!(w, "    }}")?;
        writeln!(w, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct OneOf {
    pub name: String,
    pub fields: Vec<Field>,
    pub package: String,
    pub module: String,
    pub imported: bool,
}

impl OneOf {
    fn has_lifetime(&self, desc: &FileDescriptor) -> bool {
        self.fields
            .iter()
            .any(|f| !f.deprecated && f.typ.has_lifetime(desc, f.packed(), &mut Vec::new()))
    }

    fn set_package(&mut self, package: &str, module: &str) {
        self.package = package.to_string();
        self.module = module.to_string();
    }

    fn sanitize_names(&mut self) {
        sanitize_keyword(&mut self.name);
        sanitize_keyword(&mut self.package);
        for f in self.fields.iter_mut() {
            sanitize_keyword(&mut f.name);
        }
    }

    fn get_modules(&self, desc: &FileDescriptor) -> String {
        get_modules(&self.module, self.imported, desc)
    }

    fn write<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        writeln!(w)?;
        self.write_definition(w, desc, config)?;
        writeln!(w)?;
        self.write_impl_default(w, desc)?;
        Ok(())
    }

    fn write_definition<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        writeln!(w, "#[derive(Debug, PartialEq, Clone)]")?;
        if self.has_lifetime(desc) {
            writeln!(w, "pub enum OneOf{}<'a> {{", self.name)?;
        } else {
            writeln!(w, "pub enum OneOf{} {{", self.name)?;
        }
        for f in &self.fields {
            if f.deprecated {
                if config.add_deprecated_fields {
                    writeln!(w, "    #[deprecated]")?;
                } else {
                    continue;
                }
            }

            let rust_type = f.typ.rust_type(desc)?;
            writeln!(w, "    {}({}),", f.name, rust_type)?;
        }
        writeln!(w, "    None,")?;
        writeln!(w, "}}")?;

        if cfg!(feature = "generateImplFromForEnums") {
            self.generate_impl_from_for_enums(w, desc, config)
        } else {
            Ok(())
        }
    }

    fn generate_impl_from_for_enums<W: Write>(
        &self,
        w: &mut W,
        desc: &FileDescriptor,
        config: &Config,
    ) -> Result<()> {
        // For the first of each enumeration type, generate an impl From<> for it.
        let mut handled_fields = Vec::new();
        for f in self.fields.iter().filter(|f| !f.deprecated || config.add_deprecated_fields) {
            let rust_type = f.typ.rust_type(desc)?;
            if handled_fields.contains(&rust_type) {
                continue;
            }
            writeln!(w, "impl From<{}> for OneOf{} {{", rust_type, self.name)?; // TODO: lifetime.
            writeln!(w, "   fn from(f: {}) -> OneOf{} {{", rust_type, self.name)?;
            writeln!(w, "      OneOf{}::{}(f)", self.name, f.name)?;
            writeln!(w, "   }}")?;
            writeln!(w, "}}")?;

            handled_fields.push(rust_type);
        }

        Ok(())
    }

    fn write_impl_default<W: Write>(&self, w: &mut W, desc: &FileDescriptor) -> Result<()> {
        if self.has_lifetime(desc) {
            writeln!(w, "impl<'a> Default for OneOf{}<'a> {{", self.name)?;
        } else {
            writeln!(w, "impl Default for OneOf{} {{", self.name)?;
        }
        writeln!(w, "    fn default() -> Self {{")?;
        writeln!(w, "        OneOf{}::None", self.name)?;
        writeln!(w, "    }}")?;
        writeln!(w, "}}")?;
        Ok(())
    }

    fn write_message_definition<W: Write>(&self, w: &mut W, desc: &FileDescriptor) -> Result<()> {
        if self.has_lifetime(desc) {
            writeln!(
                w,
                "    pub {}: {}OneOf{}<'a>,",
                self.name,
                self.get_modules(desc),
                self.name
            )?;
        } else {
            writeln!(
                w,
                "    pub {}: {}OneOf{},",
                self.name,
                self.get_modules(desc),
                self.name
            )?;
        }
        Ok(())
    }

    fn write_match_tag<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        for f in self.fields.iter().filter(|f| !f.deprecated || config.add_deprecated_fields) {
            let val = f.typ.read_fn(desc)?;
            writeln!(
                w,
                "                Ok({}) => msg.{} = {}OneOf{}::{}({}),",
                f.tag(),
                self.name,
                self.get_modules(desc),
                self.name,
                f.name,
                val
            )?;
        }
        Ok(())
    }

    fn write_get_size<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        writeln!(w, "        + match self.{} {{", self.name)?;
        for f in self.fields.iter().filter(|f| !f.deprecated || config.add_deprecated_fields) {
            let tag_size = sizeof_varint(f.tag());
            if f.typ.is_fixed_size() {
                writeln!(
                    w,
                    "            {}OneOf{}::{}(_) => {} + {},",
                    self.get_modules(desc),
                    self.name,
                    f.name,
                    tag_size,
                    f.typ.get_size("")
                )?;
            } else {
                writeln!(
                    w,
                    "            {}OneOf{}::{}(ref m) => {} + {},",
                    self.get_modules(desc),
                    self.name,
                    f.name,
                    tag_size,
                    f.typ.get_size("m")
                )?;
            }
        }
        writeln!(
            w,
            "            {}OneOf{}::None => 0,",
            self.get_modules(desc),
            self.name
        )?;
        write!(w, "    }}")?;
        Ok(())
    }

    fn write_write<W: Write>(&self, w: &mut W, desc: &FileDescriptor, config: &Config) -> Result<()> {
        write!(w, "        match self.{} {{", self.name)?;
        for f in self.fields.iter().filter(|f| !f.deprecated || config.add_deprecated_fields) {
            writeln!(
                w,
                "            {}OneOf{}::{}(ref m) => {{ w.write_with_tag({}, |w| w.{})? }},",
                self.get_modules(desc),
                self.name,
                f.name,
                f.tag(),
                f.typ.get_write("m")
            )?;
        }
        writeln!(
            w,
            "            {}OneOf{}::None => {{}},",
            self.get_modules(desc),
            self.name
        )?;
        write!(w, "    }}")?;
        Ok(())
    }
}

pub struct Config {
    pub in_file: PathBuf,
    pub out_file: PathBuf,
    pub single_module: bool,
    pub import_search_path: Vec<PathBuf>,
    pub no_output: bool,
    pub headers: bool,
    pub custom_struct_derive: Vec<String>,
    pub custom_repr: Option<String>,
    pub custom_rpc_generator: RpcGeneratorFunction,
    pub custom_includes: Vec<String>,
    pub gen_info: bool,
    pub add_deprecated_fields: bool,
}

#[derive(Debug, Default, Clone)]
pub struct FileDescriptor {
    pub import_paths: Vec<PathBuf>,
    pub package: String,
    pub syntax: Syntax,
    pub messages: Vec<Message>,
    pub enums: Vec<Enumerator>,
    pub module: String,
    pub rpc_services: Vec<RpcService>,
}

impl FileDescriptor {
    pub fn run(configs: &[Config]) -> Result<()> {
        for config in configs {
            Self::write_proto(&config)?
        }
        Ok(())
    }

    pub fn write_proto(config: &Config) -> Result<()> {
        let mut desc = FileDescriptor::read_proto(&config.in_file, &config.import_search_path)?;

        if desc.messages.is_empty() && desc.enums.is_empty() {
            // There could had been unsupported structures, so bail early
            return Err(Error::EmptyRead);
        }

        desc.resolve_types()?;
        desc.break_cycles()?;
        desc.sanity_checks()?;
        desc.set_defaults()?;
        desc.sanitize_names();

        if config.single_module {
            desc.package = "".to_string();
        }

        let (prefix, file_package) = split_package(&desc.package);

        let mut file_stem = if file_package.is_empty() {
            get_file_stem(&config.out_file)?
        } else {
            file_package.to_string()
        };

        if !file_package.is_empty() {
            sanitize_keyword(&mut file_stem);
        }
        let mut out_file = config.out_file.with_file_name(format!("{}.rs", file_stem));

        if !prefix.is_empty() {
            use std::fs::create_dir_all;
            // e.g. package is a.b; we need to create directory 'a' and insert it into the path
            let file = PathBuf::from(out_file.file_name().unwrap());
            out_file.pop();
            for p in prefix.split('.') {
                out_file.push(p);

                if !out_file.exists() {
                    create_dir_all(&out_file)?;
                    update_mod_file(&out_file)?;
                }
            }
            out_file.push(file);
        }
        if config.no_output {
            let imported = |b| if b { " imported" } else { "" };
            println!("source will be written to {}\n", out_file.display());
            for m in &desc.messages {
                println!(
                    "message {} module {}{}",
                    m.name,
                    m.module,
                    imported(m.imported)
                );
            }
            for e in &desc.enums {
                println!(
                    "enum {} module {}{}",
                    e.name,
                    e.module,
                    imported(e.imported)
                );
            }
            return Ok(());
        }

        let name = config.in_file.file_name().and_then(|e| e.to_str()).unwrap();
        let mut w = BufWriter::new(File::create(&out_file)?);
        desc.write(&mut w, name, config)?;
        update_mod_file(&out_file)
    }

    pub fn convert_field_types(&mut self, from: &FieldType, to: &FieldType) {
        // Messages and enums are the only structures with types
        for m in &mut self.messages {
            m.convert_field_types(from, to);
        }
    }

    /// Opens a proto file, reads it and returns raw parsed data
    pub fn read_proto(in_file: &Path, import_search_path: &[PathBuf]) -> Result<FileDescriptor> {
        let mut buf = Vec::new();
        {
            let f = File::open(in_file)?;
            let mut reader = BufReader::new(f);
            reader.read_to_end(&mut buf)?;
        }
        let mut desc = file_descriptor(&buf).to_result().map_err(Error::Nom)?;
        for mut m in &mut desc.messages {
            if m.path.as_os_str().is_empty() {
                m.path = in_file.to_path_buf();
                if !import_search_path.is_empty() {
                    if let Ok(p) = m.path.clone().strip_prefix(&import_search_path[0]) {
                        m.import = p.to_path_buf();
                    }
                }
            }
        }
        // proto files with no packages are given an implicit module,
        // since every generated Rust source file represents a module
        desc.module = if desc.package.is_empty() {
            get_file_stem(in_file)?
        } else {
            desc.package.clone()
        };

        desc.fetch_imports(&in_file, import_search_path)?;
        Ok(desc)
    }

    fn sanity_checks(&self) -> Result<()> {
        for m in &self.messages {
            m.sanity_checks(&self)?;
        }
        Ok(())
    }

    /// Get messages and enums from imports
    fn fetch_imports(&mut self, in_file: &Path, import_search_path: &[PathBuf]) -> Result<()> {
        for m in &mut self.messages {
            m.set_package("", &self.module);
        }
        for m in &mut self.enums {
            m.set_package("", &self.module);
        }

        for import in &self.import_paths {
            // this is the same logic as the C preprocessor;
            // if the include path item is absolute, then append the filename,
            // otherwise it is always relative to the file.
            let mut matching_file = None;
            for path in import_search_path {
                let candidate = if path.is_absolute() {
                    let import_path = import.to_string_lossy().replace("/", "\\");
                    path.join(Path::new(&import_path))
                } else {
                    let import_path = import.to_string_lossy().replace("/", "\\");
                    in_file
                        .parent()
                        .map_or_else(|| path.join(&import_path), |p| p.join(path).join(&import))
                };
                if candidate.exists() {
                    matching_file = Some(candidate);
                    break;
                }
            }
            if matching_file.is_none() {
                return Err(Error::InvalidImport(format!(
                    "file {} not found on import path",
                    import.display()
                )));
            }
            let proto_file = matching_file.unwrap();
            let mut f = FileDescriptor::read_proto(&proto_file, import_search_path)?;

            // if the proto has a packge then the names will be prefixed
            let package = f.package.clone();
            let module = f.module.clone();
            self.messages.extend(f.messages.drain(..).map(|mut m| {
                if m.package.is_empty() {
                    m.set_package(&package, &module);
                }
                if m.path.as_os_str().is_empty() {
                    m.path = proto_file.clone();
                }
                if m.import.as_os_str().is_empty() {
                    m.import = import.clone();
                }
                m.set_imported();
                m
            }));
            self.enums.extend(f.enums.drain(..).map(|mut e| {
                if e.package.is_empty() {
                    e.set_package(&package, &module);
                }
                if e.path.as_os_str().is_empty() {
                    e.path = proto_file.clone();
                }
                if e.import.as_os_str().is_empty() {
                    e.import = import.clone();
                }
                e.imported = true;
                e
            }));
        }
        Ok(())
    }

    fn set_defaults(&mut self) -> Result<()> {
        // this is very inefficient but we don't care ...
        //let msgs = self.messages.clone();
        let copy = self.clone();
        for m in &mut self.messages {
            m.sanitize_defaults(&copy)?; //&msgs, &self.enums)?; ???
        }
        // force packed only on primitives
        for m in &mut self.messages {
            m.unset_packed_non_primitives();
        }
        Ok(())
    }

    fn sanitize_names(&mut self) {
        for m in &mut self.messages {
            m.sanitize_names();
        }
        for e in &mut self.enums {
            e.sanitize_names();
        }
    }

    /// Breaks cycles by adding boxes when necessary
    fn break_cycles(&mut self) -> Result<()> {
        // get strongly connected components
        let sccs = self.sccs();

        fn is_cycle(scc: &[MessageIndex], desc: &FileDescriptor) -> bool {
            scc.iter()
                .map(|m| m.get_message(desc))
                .flat_map(|m| m.all_fields())
                .filter_map(|f| f.typ.message())
                .any(|m| scc.contains(m))
        }

        // sccs are sub DFS trees so if there is a edge connecting a node to
        // another node higher in the scc list, then this is a cycle. (Note that
        // we may have several cycles per scc).
        //
        // Technically we only need to box one edge (optional field) per cycle to
        // have Sized structs. Unfortunately, scc root depend on the order we
        // traverse the graph so such a field is not guaranteed to always be the same.
        //
        // For now, we decide (see discussion in #121) to box all optional fields
        // within a scc. We favor generated code stability over performance.
        for scc in &sccs {
            debug!("scc: {:?}", scc);
            for (i, v) in scc.iter().enumerate() {
                // cycles with v as root
                let cycles = v
                    .get_message(self)
                    .all_fields()
                    .filter_map(|f| f.typ.message())
                    .filter_map(|m| scc[i..].iter().position(|n| n == m))
                    .collect::<Vec<_>>();
                for cycle in cycles {
                    let cycle = &scc[i..i + cycle + 1];
                    debug!("cycle: {:?}", &cycle);
                    if is_cycle(cycle, self) {
                        return Err(Error::Cycle(
                            cycle
                                .iter()
                                .map(|m| m.get_message(self).name.clone())
                                .collect(),
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn get_full_names(&mut self) -> (HashMap<String, MessageIndex>, HashMap<String, EnumIndex>) {
        fn rec_full_names(
            m: &mut Message,
            index: &mut MessageIndex,
            full_msgs: &mut HashMap<String, MessageIndex>,
            full_enums: &mut HashMap<String, EnumIndex>,
        ) {
            m.index = index.clone();
            if m.package.is_empty() {
                full_msgs.insert(m.name.clone(), index.clone());
            } else {
                full_msgs.insert(format!("{}.{}", m.package, m.name), index.clone());
            }
            for (i, e) in m.enums.iter_mut().enumerate() {
                let index = EnumIndex {
                    msg_index: index.clone(),
                    index: i,
                };
                e.index = index.clone();
                full_enums.insert(format!("{}.{}", e.package, e.name), index);
            }
            for (i, m) in m.messages.iter_mut().enumerate() {
                index.push(i);
                rec_full_names(m, index, full_msgs, full_enums);
                index.pop();
            }
        }

        let mut full_msgs = HashMap::new();
        let mut full_enums = HashMap::new();
        let mut index = MessageIndex { indexes: vec![] };
        for (i, m) in self.messages.iter_mut().enumerate() {
            index.push(i);
            rec_full_names(m, &mut index, &mut full_msgs, &mut full_enums);
            index.pop();
        }
        for (i, e) in self.enums.iter_mut().enumerate() {
            let index = EnumIndex {
                msg_index: index.clone(),
                index: i,
            };
            e.index = index.clone();
            if e.package.is_empty() {
                full_enums.insert(e.name.clone(), index.clone());
            } else {
                full_enums.insert(format!("{}.{}", e.package, e.name), index.clone());
            }
        }
        (full_msgs, full_enums)
    }

    fn resolve_types(&mut self) -> Result<()> {
        let (full_msgs, full_enums) = self.get_full_names();

        fn rec_resolve_types(
            m: &mut Message,
            full_msgs: &HashMap<String, MessageIndex>,
            full_enums: &HashMap<String, EnumIndex>,
        ) -> Result<()> {
            // Interestingly, we can't call all_fields_mut to iterate over the
            // fields here: writing out the field traversal as below lets Rust
            // split m's mutable borrow, permitting the loop body to use fields
            // of `m` other than `fields` and `oneofs`.
            'types: for typ in m
                .fields
                .iter_mut()
                .chain(m.oneofs.iter_mut().flat_map(|o| o.fields.iter_mut()))
                .map(|f| &mut f.typ)
                .flat_map(|typ| match *typ {
                    _ => vec![typ].into_iter(),
                })
            {
                if let FieldType::MessageOrEnum(name) = typ.clone() {
                    let test_names: Vec<String> = if name.starts_with('.') {
                        vec![name.clone().split_off(1)]
                    } else if m.package.is_empty() {
                        vec![name.clone(), format!("{}.{}", m.name, name)]
                    } else {
                        vec![
                            name.clone(),
                            format!("{}.{}", m.package, name),
                            format!("{}.{}.{}", m.package, m.name, name),
                        ]
                    };
                    for name in &test_names {
                        if let Some(msg) = full_msgs.get(&*name) {
                            *typ = FieldType::Message(msg.clone());
                            continue 'types;
                        } else if let Some(e) = full_enums.get(&*name) {
                            *typ = FieldType::Enum(e.clone());
                            continue 'types;
                        }
                    }
                    return Err(Error::MessageOrEnumNotFound(name));
                }
            }
            for m in m.messages.iter_mut() {
                rec_resolve_types(m, full_msgs, full_enums)?;
            }
            Ok(())
        }

        for m in self.messages.iter_mut() {
            rec_resolve_types(m, &full_msgs, &full_enums)?;
        }
        Ok(())
    }

    fn write<W: Write>(&self, w: &mut W, filename: &str, config: &Config) -> Result<()> {
        println!(
            "Found {} messages, and {} enums",
            self.messages.len(),
            self.enums.len()
        );
        if config.headers {
            self.write_headers(w, filename, config)?;
        }
        self.write_package_start(w)?;
        self.write_uses(w, config)?;
        self.write_imports(w)?;
        self.write_enums(w)?;
        self.write_messages(w, config)?;
        self.write_rpc_services(w, config)?;
        self.write_package_end(w)?;
        Ok(())
    }

    fn write_headers<W: Write>(&self, w: &mut W, filename: &str, config: &Config) -> Result<()> {
        writeln!(
            w,
            "// Automatically generated rust module for '{}' file",
            filename
        )?;
        writeln!(w)?;
        writeln!(w, "#![allow(non_snake_case)]")?;
        writeln!(w, "#![allow(non_upper_case_globals)]")?;
        writeln!(w, "#![allow(non_camel_case_types)]")?;
        writeln!(w, "#![allow(unused_imports)]")?;
        writeln!(w, "#![allow(unknown_lints)]")?;
        writeln!(w, "#![allow(clippy::all)]")?;

        if config.add_deprecated_fields {
            writeln!(w, "#![allow(deprecated)]")?;
        }

        writeln!(w, "#![cfg_attr(rustfmt, rustfmt_skip)]")?;
        writeln!(w)?;
        Ok(())
    }

    fn write_package_start<W: Write>(&self, w: &mut W) -> Result<()> {
        writeln!(w)?;
        Ok(())
    }

    fn write_uses<W: Write>(&self, w: &mut W, config: &Config) -> Result<()> {
        if self.messages.iter().all(|m| m.is_unit()) {
            writeln!(
                w,
                "use quick_protobuf::{{BytesReader, Result, MessageInfo, MessageRead, MessageWrite}};"
            )?;
            return Ok(());
        }

        writeln!(
            w,
            "use quick_protobuf::{{MessageInfo, MessageRead, MessageWrite, BytesReader, Writer, WriterBackend, Result}};"
        )?;

        writeln!(w, "use quick_protobuf::sizeofs::*;")?;
        for include in &config.custom_includes {
            writeln!(w, "{}", include)?;
        }
        Ok(())
    }

    fn write_imports<W: Write>(&self, w: &mut W) -> Result<()> {
        // even if we don't have an explicit package, there is an implicit Rust module
        // This `use` allows us to refer to the package root.
        // NOTE! I'm suppressing not-needed 'use super::*' errors currently!
        let mut depth = self.package.split('.').count();
        if depth == 0 {
            depth = 1;
        }
        write!(w, "use ")?;
        for _ in 0..depth {
            write!(w, "super::")?;
        }
        writeln!(w, "*;")?;
        Ok(())
    }

    fn write_package_end<W: Write>(&self, w: &mut W) -> Result<()> {
        writeln!(w)?;
        Ok(())
    }

    fn write_enums<W: Write>(&self, w: &mut W) -> Result<()> {
        for m in self.enums.iter().filter(|e| !e.imported) {
            println!("Writing enum {}", m.name);
            writeln!(w)?;
            m.write_definition(w)?;
            writeln!(w)?;
            m.write_impl_default(w)?;
            writeln!(w)?;
            m.write_from_i32(w)?;
            writeln!(w)?;
            m.write_from_str(w)?;
        }
        Ok(())
    }

    fn write_rpc_services<W: Write>(&self, w: &mut W, config: &Config) -> Result<()> {
        for m in self.rpc_services.iter() {
            println!("Writing Rpc {}", m.service_name);
            writeln!(w)?;
            m.write_definition(w, config)?;
        }
        Ok(())
    }

    fn write_messages<W: Write>(&self, w: &mut W, config: &Config) -> Result<()> {
        for m in self.messages.iter().filter(|m| !m.imported) {
            m.write(w, &self, config)?;
        }
        Ok(())
    }
}

/// Calculates the tag value
fn tag(number: u32, typ: &FieldType, packed: bool) -> u32 {
    number << 3 | typ.wire_type_num(packed)
}

/// "" is ("",""), "a" is ("","a"), "a.b" is ("a"."b"), and so forth.
fn split_package(package: &str) -> (&str, &str) {
    if package.is_empty() {
        ("", "")
    } else if let Some(i) = package.rfind('.') {
        (&package[0..i], &package[i + 1..])
    } else {
        ("", package)
    }
}

const MAGIC_HEADER: &str = "// Automatically generated mod.rs";

/// Given a file path, create or update the mod.rs file within its folder
fn update_mod_file(path: &Path) -> Result<()> {
    let mut file = path.to_path_buf();
    use std::fs::OpenOptions;
    use std::io::prelude::*;

    let name = file.file_stem().unwrap().to_string_lossy().to_string();
    file.pop();
    file.push("mod.rs");
    let matches = "pub mod ";
    let mut present = false;
    let mut exists = false;
    if let Ok(f) = File::open(&file) {
        exists = true;
        let mut first = true;
        for line in BufReader::new(f).lines() {
            let line = line?;
            if first {
                if line.find(MAGIC_HEADER).is_none() {
                    // it is NOT one of our generated mod.rs files, so don't modify it!
                    present = true;
                    break;
                }
                first = false;
            }
            if let Some(i) = line.find(matches) {
                let rest = &line[i + matches.len()..line.len() - 1];
                if rest == name {
                    // we already have a reference to this module...
                    present = true;
                    break;
                }
            }
        }
    }
    if !present {
        let mut f = if exists {
            OpenOptions::new().append(true).open(&file)?
        } else {
            let mut f = File::create(&file)?;
            writeln!(f, "{}", MAGIC_HEADER)?;
            f
        };

        writeln!(f, "pub mod {};", name)?;
    }
    Ok(())
}

/// get the proper sanitized file stem from an input file path
fn get_file_stem(path: &Path) -> Result<String> {
    let mut file_stem = path
        .file_stem()
        .and_then(|f| f.to_str())
        .map(|s| s.to_string())
        .ok_or_else(|| Error::OutputFile(format!("{}", path.display())))?;

    file_stem = file_stem.replace(|c: char| !c.is_alphanumeric(), "_");
    // will now be properly alphanumeric, but may be a keyword!
    sanitize_keyword(&mut file_stem);
    Ok(file_stem)
}
