#![recursion_limit = "80000"]

#[macro_use] extern crate error_chain;
extern crate byteorder;
extern crate extfmt;
extern crate void;

pub mod error;
pub mod mtproto;
pub mod mtproto_prelude;

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
        unimplemented!()
    }

    pub fn read_boxed<D: BoxedDeserialize>(&mut self) -> Result<D> {
        unimplemented!()
    }

    pub fn read_generic<D: Deserialize>(&mut self) -> Result<D::Output> {
        unimplemented!()
    }
}

impl<'r> io::Read for Deserializer<'r> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

pub trait BareDeserialize {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self>
        where Self: Sized;
}

pub trait BoxedDeserialize {
    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self>
        where Self: Sized;
}

pub trait Deserialize {
    type Output;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output>;
}

pub trait Function {
    type Reply: Deserialize;
}

pub struct Serializer<'w> {
    writer: &'w mut io::Write,
}

impl<'w> Serializer<'w> {
    pub fn write_constructor(&mut self, id: ConstructorNumber) -> Result<()> {
        unimplemented!()
    }

    pub fn write_bare<S: BareSerialize>(&mut self, obj: &S) -> Result<()> {
        unimplemented!()
    }

    pub fn write_boxed<S: BoxedSerialize>(&mut self, obj: &S) -> Result<()> {
        unimplemented!()
    }

    pub fn write_generic<S: Serialize>(&mut self, obj: &S::Input) -> Result<()> {
        unimplemented!()
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
    fn type_id(&self) -> ConstructorNumber;
    fn serialize_boxed(&self, ser: &mut Serializer) -> Result<()>;
}

pub trait Serialize {
    type Input: ?Sized;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()>;
}

pub trait AnyBoxedSerialize: Any + BoxedSerialize {}
impl<T: Any + BoxedSerialize> AnyBoxedSerialize for T {}
