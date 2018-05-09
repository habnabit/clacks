#![recursion_limit = "80000"]

#[macro_use] extern crate error_chain;
extern crate byteorder;
extern crate extfmt;

pub mod error;
pub mod mtproto;
mod mtproto_prelude;

use std::{fmt, io};
use std::any::Any;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ConstructorNumber(pub u32);

impl fmt::Debug for ConstructorNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{:08x}", self.0)
    }
}

pub type Result<T> = ::std::result::Result<T, ::error::Error>;

pub struct Deserializer<'r> {
    reader: &'r mut io::Read,
}

impl<'r> Deserializer<'r> {
    pub fn read_constructor(&mut self) -> Result<ConstructorNumber> {
        use byteorder::{LittleEndian, ReadBytesExt};
        Ok(ConstructorNumber(self.read_u32::<LittleEndian>()?))
    }

    pub fn read_bare<D: BareDeserialize>(&mut self) -> Result<D> {
        D::deserialize_bare(self)
    }

    pub fn read_boxed<D: BoxedDeserialize>(&mut self) -> Result<D> {
        let constructor = self.read_constructor()?;
        D::deserialize_boxed(constructor, self)
    }
}

impl<'r> io::Read for Deserializer<'r> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

pub trait BareDeserialize
    where Self: Sized,
{
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self>;
}

pub trait BoxedDeserialize
    where Self: Sized,
{
    fn possible_constructors() -> Vec<ConstructorNumber>;
    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self>;
}

pub trait Function {
    type Reply: BoxedDeserialize;
}

pub struct Serializer<'w> {
    writer: &'w mut io::Write,
}

impl<'w> Serializer<'w> {
    pub fn new(writer: &'w mut io::Write) -> Self {
        Serializer { writer }
    }

    pub fn write_constructor(&mut self, id: ConstructorNumber) -> Result<()> {
        use byteorder::{LittleEndian, WriteBytesExt};
        Ok(self.write_u32::<LittleEndian>(id.0)?)
    }

    pub fn write_bare<S: ?Sized + BareSerialize>(&mut self, obj: &S) -> Result<()> {
        obj.serialize_bare(self)
    }

    pub fn write_boxed<S: BoxedSerialize>(&mut self, obj: &S) -> Result<()> {
        let (constructor, bare) = obj.serialize_boxed();
        self.write_constructor(constructor)?;
        self.write_bare(bare)?;
        Ok(())
    }
}

impl<'w> io::Write for Serializer<'w> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

pub trait BareSerialize {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()>;
}

pub trait BoxedSerialize {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize);
}

pub trait IntoBoxed: BareSerialize {
    type Boxed: BoxedSerialize;
    fn into_boxed(self) -> Self::Boxed;
}

pub trait AnyBoxedSerialize: Any + BoxedSerialize {}
impl<T: Any + BoxedSerialize> AnyBoxedSerialize for T {}
