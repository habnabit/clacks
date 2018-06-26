#![allow(non_camel_case_types)]

use ::{AnyBoxedSerialize, BareDeserialize, BareSerialize, BoxedDeserialize, BoxedSerialize, ConstructorNumber, Deserializer, DynamicDeserializer, Result, Serializer};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use extfmt::Hexlify;
use rand::{Rand, Rng};
use serde;
use std::fmt;
use std::io::{Read, Write};
use std::marker::PhantomData;

use mtproto::manual::GzipPacked;

const MAX_BYTES_DEBUG_LEN: usize = 4;

macro_rules! impl_byteslike {
    (@common $ty:ident) => {

        impl fmt::Debug for $ty {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.len() <= MAX_BYTES_DEBUG_LEN {
                    write!(f, "<{}>", Hexlify(&self.0))
                } else {
                    write!(f, "<{}... {} bytes>", Hexlify(&self.0[..MAX_BYTES_DEBUG_LEN]), self.0.len())
                }
            }
        }

        impl ::std::ops::Deref for $ty {
            type Target = [u8];
            fn deref(&self) -> &[u8] { &self.0 }
        }

        impl ::std::ops::DerefMut for $ty {
            fn deref_mut(&mut self) -> &mut [u8] { &mut self.0 }
        }

    };

    (@arraylike $ty:ident) => {

        impl_byteslike!(@common $ty);

        impl Rand for $ty {
            fn rand<R: Rng>(rng: &mut R) -> Self {
                let mut ret: Self = Default::default();
                rng.fill_bytes(&mut ret.0);
                ret
            }
        }

        impl BareDeserialize for $ty {
            fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
                let mut ret: Self = Default::default();
                de.read(&mut ret.0)?;
                Ok(ret)
            }
        }

        impl BareSerialize for $ty {
            fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
                ser.write(&self.0)?;
                Ok(())
            }
        }

    };
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct bytes(pub Vec<u8>);

impl BareDeserialize for bytes {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        let vec = de.read_bare::<Vec<u8>>()?;
        Ok(bytes(vec))
    }
}

impl BareSerialize for bytes {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        ser.write_bare::<[u8]>(&self.0)
    }
}

impl From<Vec<u8>> for bytes {
    fn from(v: Vec<u8>) -> Self {
        bytes(v)
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct int128(pub [u8; 16]);

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct int256(pub [u8; 32]);

impl_byteslike!(@common bytes);
impl_byteslike!(@arraylike int128);
impl_byteslike!(@arraylike int256);

pub struct TLObject(Box<AnyBoxedSerialize>);

impl TLObject {
    pub fn new<I: AnyBoxedSerialize>(inner: I) -> Self {
        TLObject(Box::new(inner))
    }

    pub fn is<I: AnyBoxedSerialize>(&self) -> bool {
        self.0.as_any().is::<I>()
    }

    pub fn downcast<I: AnyBoxedSerialize>(self) -> ::std::result::Result<I, Self> {
        if self.is::<I>() {
            Ok(*self.0.into_boxed_any().downcast::<I>().unwrap())
        } else {
            let is_gunzip = match self.0.as_any().downcast_ref::<TransparentGunzip>() {
                Some(gz) => gz.inner.is::<I>(),
                _ => false,
            };
            if is_gunzip {
                Ok(*{
                    self.0.into_boxed_any()
                        .downcast::<TransparentGunzip>().unwrap()
                        .inner.0.into_boxed_any()
                        .downcast::<I>().unwrap()
                })
            } else {
                Err(self)
            }
        }
    }
}

impl Clone for TLObject {
    fn clone(&self) -> Self {
        unimplemented!()
    }
}

impl fmt::Debug for TLObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (type_id, _) = self.0.serialize_boxed();
        write!(f, "(TLObject tl_id:{:?})", type_id)
    }
}

impl BoxedDeserialize for TLObject {
    fn possible_constructors() -> Vec<ConstructorNumber> {
        ::mtproto::dynamic::BY_NUMBER.keys().cloned().collect()
    }

    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
        match ::mtproto::dynamic::BY_NUMBER.get(&id) {
            Some(dyn) => (dyn.mtproto)(id, de),
            None => _invalid_id!(id),
        }
    }
}

impl<'de> serde::Deserialize<'de> for TLObject {
    fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error>
        where D: serde::Deserializer<'de>,
    {
        struct Constructor(DynamicDeserializer);
        struct DynVisitor;
        struct Visitor;

        impl<'de> serde::Deserialize<'de> for Constructor {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error>
                where D: serde::Deserializer<'de>,
            {
                deserializer.deserialize_identifier(DynVisitor)
            }
        }

        impl<'de> serde::de::DeserializeSeed<'de> for Constructor {
            type Value = TLObject;

            fn deserialize<D>(self, deserializer: D) -> ::std::result::Result<Self::Value, D::Error>
                where D: serde::Deserializer<'de>,
            {
                (self.0.serde)(&mut ::erased_serde::Deserializer::erase(deserializer))
                    .map_err(<D::Error as serde::de::Error>::custom)
            }
        }

        impl<'de> serde::de::Visitor<'de> for DynVisitor {
            type Value = Constructor;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "a dynamic deserializer")
            }

            fn visit_i64<E>(self, v: i64) -> ::std::result::Result<Self::Value, E>
                where E: serde::de::Error,
            {
                self.visit_u64(v as u64)
            }

            fn visit_u64<E>(self, v: u64) -> ::std::result::Result<Self::Value, E>
                where E: serde::de::Error,
            {
                let id = ConstructorNumber(v as u32);
                ::mtproto::dynamic::BY_NUMBER
                    .get(&id)
                    .map(|&dyn| Constructor(dyn))
                    .ok_or_else(|| E::invalid_value(serde::de::Unexpected::Unsigned(v), &self))
            }

            fn visit_str<E>(self, v: &str) -> ::std::result::Result<Self::Value, E>
                where E: serde::de::Error,
            {
                ::mtproto::dynamic::BY_NAME
                    .get(&v)
                    .map(|&dyn| Constructor(dyn))
                    .ok_or_else(|| E::invalid_value(serde::de::Unexpected::Str(v), &self))
            }
        }

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = TLObject;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "a TLObject variant")
            }

            fn visit_enum<A>(self, data: A) -> ::std::result::Result<Self::Value, A::Error>
                where A: serde::de::EnumAccess<'de>,
            {
                let (cons, variant): (Constructor, _) = serde::de::EnumAccess::variant(data)?;
                serde::de::VariantAccess::newtype_variant_seed(variant, cons)
            }
        }

        deserializer.deserialize_enum("TLObject", &[], Visitor)
    }
}

impl BoxedSerialize for TLObject {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
        self.0.serialize_boxed()
    }
}

impl serde::Serialize for TLObject {
    fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error>
        where S: serde::Serializer,
    {
        let (id, _) = self.0.serialize_boxed();
        let tl_type_name = ::mtproto::dynamic::BY_NUMBER.get(&id)
            .map(|dyn| dyn.type_name)
            .unwrap_or(&"<bogus>");
        serializer.serialize_newtype_variant("TLObject", id.0, tl_type_name, &self.0)
    }
}

#[derive(Debug, Clone)]
pub struct TransparentGunzip {
    pub inner: TLObject,
    pub original: GzipPacked,
}

impl BoxedDeserialize for TransparentGunzip {
    fn possible_constructors() -> Vec<ConstructorNumber> {
        GzipPacked::possible_constructors()
    }

    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
        use flate2::bufread::GzDecoder;
        use std::io::Read;

        let original = GzipPacked::deserialize_boxed(id, de)?;
        let inner = {
            let mut data: &[u8] = original.packed_data();
            let mut decompressed = vec![];
            GzDecoder::new(&mut data).read_to_end(&mut decompressed)?;
            TLObject::boxed_deserialized_from_bytes(&decompressed)?
        };
        Ok(TransparentGunzip { inner, original })
    }
}

impl BoxedSerialize for TransparentGunzip {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
        self.inner.serialize_boxed()
    }
}

impl serde::Serialize for TransparentGunzip {
    fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error>
        where S: serde::Serializer,
    {
        serde::Serialize::serialize(&self.inner, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for TransparentGunzip {
    fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error>
        where D: serde::Deserializer<'de>,
    {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LengthPrefixed<T>(pub T);

impl<T> From<T> for LengthPrefixed<T> {
    fn from(x: T) -> Self {
        LengthPrefixed(x)
    }
}

impl<T> BareDeserialize for LengthPrefixed<T>
    where T: BoxedDeserialize,
{
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        let len = de.read_i32::<LittleEndian>()? as usize;
        let mut buf = vec![0u8; len];
        de.read_exact(&mut buf)?;
        Ok(LengthPrefixed(T::boxed_deserialized_from_bytes(&buf)?))
    }
}

impl<T> BareSerialize for LengthPrefixed<T>
    where T: BoxedSerialize,
{
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        let inner = self.0.boxed_serialized_bytes()?;
        ser.write_i32::<LittleEndian>(inner.len() as i32)?;
        ser.write(&inner)?;
        Ok(())
    }
}

impl BareSerialize for () {
    fn serialize_bare(&self, _ser: &mut Serializer) -> Result<()> {
        Ok(())
    }
}

use mtproto::Bool;

impl From<bool> for &'static Bool {
    fn from(b: bool) -> Self {
        if b { &Bool::True } else { &Bool::False }
    }
}

impl From<bool> for Bool {
    fn from(b: bool) -> Self {
        let b: &'static Bool = b.into();
        b.clone()
    }
}

impl Into<bool> for Bool {
    fn into(self) -> bool {
        match self {
            Bool::True => true,
            Bool::False => false,
        }
    }
}

impl BoxedDeserialize for bool {
    fn possible_constructors() -> Vec<ConstructorNumber> {
        Bool::possible_constructors()
    }

    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
        Ok(Bool::deserialize_boxed(id, de)?.into())
    }
}

impl BoxedSerialize for bool {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
        let b: &'static Bool = (*self).into();
        Bool::serialize_boxed(b)
    }
}

impl BareDeserialize for String {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        let vec = de.read_bare::<Vec<u8>>()?;
        Ok(String::from_utf8(vec)?)
    }
}

impl BareSerialize for String {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        ser.write_bare::<[u8]>(self.as_bytes())?;
        Ok(())
    }
}

impl<T> BoxedDeserialize for Box<T>
    where T: BoxedDeserialize,
{
    fn possible_constructors() -> Vec<ConstructorNumber> {
        T::possible_constructors()
    }

    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
        Ok(Box::new(T::deserialize_boxed(id, de)?))
    }
}

impl<T> BoxedSerialize for Box<T>
    where T: BoxedSerialize,
{
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
        T::serialize_boxed(self)
    }
}

pub enum Bare {}
pub enum Boxed {}

pub struct Vector<Det, T>(pub Vec<T>, PhantomData<fn() -> Det>);
pub type vector<Det, T> = Vector<Det, T>;

impl<Det, T> Clone for Vector<Det, T>
    where T: Clone,
{
    fn clone(&self) -> Self {
        Vector(self.0.clone(), PhantomData)
    }
}

impl<Det, T> fmt::Debug for Vector<Det, T>
    where T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Vector")
            .field(&self.0)
            .finish()
    }
}

impl<Det, T> serde::Serialize for Vector<Det, T>
    where T: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error>
        where S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for item in &self.0 {
            seq.serialize_element(item)?;
        }
        seq.end()
    }
}

impl<'de, Det, T> serde::Deserialize<'de> for Vector<Det, T>
    where T: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error>
        where D: serde::Deserializer<'de>,
    {
        Ok(Vector(serde::Deserialize::deserialize(deserializer)?, PhantomData))
    }
}

const VECTOR_CONSTRUCTOR: ConstructorNumber = ConstructorNumber(0x1cb5c415);

macro_rules! impl_vector {
    ($det:ident, $det_de:ident, $det_ser:ident, $read_method:ident, $write_method:ident) => {

        impl<T> From<Vec<T>> for Vector<$det, T> {
            fn from(obj: Vec<T>) -> Self {
                Vector(obj, PhantomData)
            }
        }

        impl<T> ::std::ops::Deref for Vector<$det, T> {
            type Target = [T];
            fn deref(&self) -> &[T] { &self.0 }
        }

        impl<T> ::std::ops::DerefMut for Vector<$det, T> {
            fn deref_mut(&mut self) -> &mut [T] { &mut self.0 }
        }

        impl<T> BareDeserialize for Vector<$det, T>
            where T: $det_de,
        {
            fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
                let count = de.read_i32::<LittleEndian>()?;
                let mut ret = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    ret.push(de.$read_method()?);
                }
                Ok(ret.into())
            }
        }

        impl<T> BoxedDeserialize for Vector<$det, T>
            where Self: BareDeserialize,
        {
            fn possible_constructors() -> Vec<ConstructorNumber> { vec![VECTOR_CONSTRUCTOR] }

            fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
                assert_eq!(id, VECTOR_CONSTRUCTOR);
                Self::deserialize_bare(de)
            }
        }

        impl<T> BareSerialize for Vector<$det, T>
            where T: $det_ser,
        {
            fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
                ser.write_i32::<LittleEndian>(self.0.len() as i32)?;
                for item in &self.0 {
                    ser.$write_method(item)?;
                }
                Ok(())
            }
        }

        impl<T> BoxedSerialize for Vector<$det, T>
            where Self: BareSerialize,
        {
            fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
                (VECTOR_CONSTRUCTOR, self)
            }
        }

    }
}

impl_vector! { Bare, BareDeserialize, BareSerialize, read_bare, write_bare }
impl_vector! { Boxed, BoxedDeserialize, BoxedSerialize, read_boxed, write_boxed }

impl BareDeserialize for Vec<u8> {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        let len = de.read_u8()?;
        let (len, mut have_read) = if len != 254 {
            (len as usize, 1)
        } else {
            (de.read_u24::<LittleEndian>()? as usize, 4)
        };

        let mut buf = vec![0; len];
        de.read_exact(&mut buf)?;
        have_read += len;
        let remainder = have_read % 4;
        if remainder != 0 {
            let mut buf = [0u8; 4];
            de.read_exact(&mut buf[..4 - remainder])?;
        }
        Ok(buf)
    }
}

impl BareSerialize for [u8] {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        let len = self.len();
        let mut have_written = if len < 254 {
            ser.write_u8(len as u8)?;
            1
        } else {
            ser.write_u8(254)?;
            ser.write_u24::<LittleEndian>(len as u32)?;
            4
        };

        ser.write_all(self)?;
        have_written += len;
        let remainder = have_written % 4;
        if remainder != 0 {
            let buf = [0u8; 4];
            ser.write_all(&buf[..4 - remainder])?;
        }
        Ok(())
    }
}

macro_rules! impl_tl_primitive {
    ($tltype:ident, $ptype:ident, $read:ident, $write:ident) => {
        pub type $tltype = $ptype;

        impl BareDeserialize for $ptype {
            fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
                Ok(de.$read::<LittleEndian>()?)
            }
        }

        impl BareSerialize for $ptype {
            fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
                ser.$write::<LittleEndian>(*self)?;
                Ok(())
            }
        }
    }
}

impl_tl_primitive! { int, i32, read_i32, write_i32 }
impl_tl_primitive! { long, i64, read_i64, write_i64 }
impl_tl_primitive! { double, f64, read_f64, write_f64 }

pub type Flags = i32;
pub type lengthPrefixedTypedObject = LengthPrefixed<TypedObject>;
pub type string = String;
pub type TypedObject = TLObject;
