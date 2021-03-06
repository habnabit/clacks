#![deny(private_in_public, unused_extern_crates)]

#[macro_use] extern crate erased_serde;
#[macro_use] extern crate failure;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde_derive;
extern crate byteorder;
extern crate extfmt;
extern crate flate2;
extern crate rand;
extern crate serde;

macro_rules! _invalid_id {
    ($id:ident) => {
        Err(::InvalidConstructor { expected: Self::possible_constructors(), received: $id }.into())
    };
}

pub mod mtproto;
mod mtproto_prelude;

use std::{fmt, io};
use std::any::Any;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ConstructorNumber(pub u32);

impl fmt::Debug for ConstructorNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{:08x}", self.0)
    }
}

#[derive(Debug, Fail)]
#[fail(display = "expected a constructor in {:?}; got {:?}", expected, received)]
pub struct InvalidConstructor {
    pub expected: Vec<ConstructorNumber>,
    pub received: ConstructorNumber,
}

pub type Result<T> = ::std::result::Result<T, ::failure::Error>;

pub struct Deserializer<'r> {
    reader: &'r mut io::Read,
}

impl<'r> Deserializer<'r> {
    pub fn new(reader: &'r mut io::Read) -> Self {
        Deserializer { reader }
    }

    pub fn read_constructor(&mut self) -> Result<ConstructorNumber> {
        use byteorder::{LittleEndian, ReadBytesExt};
        Ok(ConstructorNumber(self.read_u32::<LittleEndian>()?))
    }

    #[inline(always)]
    pub fn read_bare<D: BareDeserialize>(&mut self) -> Result<D> {
        D::deserialize_bare(self)
    }

    #[inline(always)]
    pub fn read_boxed<D: BoxedDeserialize>(&mut self) -> Result<D> {
        let constructor = self.read_constructor()?;
        D::deserialize_boxed(constructor, self)
    }

    #[inline(always)]
    pub fn just_default<D: Default>(&self) -> Result<D> {
        Ok(Default::default())
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

    fn bare_deserialized_from_bytes(mut bytes: &[u8]) -> Result<Self> {
        Deserializer::new(&mut bytes).read_bare()
    }
}

pub trait BoxedDeserialize
    where Self: Sized,
{
    fn possible_constructors() -> Vec<ConstructorNumber>;
    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self>;

    fn boxed_deserialized_from_bytes(mut bytes: &[u8]) -> Result<Self> {
        Deserializer::new(&mut bytes).read_boxed()
    }
}

pub trait BoxedDeserializeDynamic: BoxedDeserialize + for<'de> serde::Deserialize<'de> {
    fn boxed_deserialize_to_box(id: ConstructorNumber, de: &mut Deserializer) -> Result<mtproto::TLObject>;
    fn serde_deserialize_to_box(de: &mut erased_serde::Deserializer) -> ::std::result::Result<mtproto::TLObject, erased_serde::Error>;
}

impl<D> BoxedDeserializeDynamic for D
    where D: BoxedDeserialize + for<'de> serde::Deserialize<'de> + AnyBoxedSerialize,
{
    fn boxed_deserialize_to_box(id: ConstructorNumber, de: &mut Deserializer) -> Result<mtproto::TLObject> {
        Ok(mtproto::TLObject::new(D::deserialize_boxed(id, de)?))
    }

    fn serde_deserialize_to_box(de: &mut erased_serde::Deserializer) -> ::std::result::Result<mtproto::TLObject, erased_serde::Error>
    {
        Ok(mtproto::TLObject::new(erased_serde::deserialize::<D>(de)?))
    }
}

#[derive(Clone, Copy)]
pub struct DynamicDeserializer {
    id: ConstructorNumber,
    type_name: &'static str,
    mtproto: fn(ConstructorNumber, &mut Deserializer) -> Result<mtproto::TLObject>,
    serde: fn(&mut erased_serde::Deserializer) -> ::std::result::Result<mtproto::TLObject, erased_serde::Error>,
}

impl DynamicDeserializer {
    #[inline(always)]
    pub fn from<D: BoxedDeserializeDynamic>(id: ConstructorNumber, type_name: &'static str) -> Self {
        DynamicDeserializer {
            id, type_name,
            mtproto: D::boxed_deserialize_to_box,
            serde: D::serde_deserialize_to_box,
        }
    }
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

    #[inline(always)]
    pub fn write_bare<S: ?Sized + BareSerialize>(&mut self, obj: &S) -> Result<()> {
        obj.serialize_bare(self)
    }

    #[inline(always)]
    pub fn write_boxed<S: ?Sized + BoxedSerialize>(&mut self, obj: &S) -> Result<()> {
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

    fn bare_serialized_bytes(&self) -> Result<Vec<u8>> {
        let mut buf: Vec<u8> = vec![];
        Serializer::new(&mut buf).write_bare(self)?;
        Ok(buf)
    }
}

pub trait BoxedSerialize {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize);

    fn boxed_serialized_bytes(&self) -> Result<Vec<u8>> {
        let mut buf: Vec<u8> = vec![];
        Serializer::new(&mut buf).write_boxed(self)?;
        Ok(buf)
    }
}

pub trait IntoBoxed: BareSerialize {
    type Boxed: BoxedSerialize;
    fn into_boxed(self) -> Self::Boxed;
}

pub trait AnyBoxedSerialize: Any + Send + BoxedSerialize + erased_serde::Serialize {
    fn as_any(&self) -> &Any;
    fn into_boxed_any(self: Box<Self>) -> Box<Any + Send>;
}

impl<T: Any + Send + BoxedSerialize + erased_serde::Serialize> AnyBoxedSerialize for T {
    fn as_any(&self) -> &Any { self }
    fn into_boxed_any(self: Box<Self>) -> Box<Any + Send> { self }
}

serialize_trait_object!(AnyBoxedSerialize);

pub trait Function: AnyBoxedSerialize + serde::Serialize {
    type Reply: BoxedDeserialize + AnyBoxedSerialize;
}
