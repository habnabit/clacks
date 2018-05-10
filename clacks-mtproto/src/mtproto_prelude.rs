use ::{AnyBoxedSerialize, BareDeserialize, BareSerialize, BoxedDeserialize, BoxedSerialize, ConstructorNumber, Deserializer, Result, Serializer};
use extfmt::Hexlify;
use rand::{Rand, Rng};
use std::fmt;
use std::marker::PhantomData;

#[derive(Clone)]
pub struct bytes(pub Vec<u8>);

impl fmt::Debug for bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{} bytes>", self.0.len())
    }
}

impl ::std::ops::Deref for bytes {
    type Target = [u8];
    fn deref(&self) -> &[u8] { self.0.as_slice() }
}

impl BareDeserialize for bytes {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        let vec = de.read_bare::<Vector<Bare, u8>>()?;
        Ok(bytes(vec.0))
    }
}

impl BareSerialize for bytes {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

#[derive(Clone, Copy, Default)]
pub struct int128(pub [u8; 16]);

impl fmt::Debug for int128 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", Hexlify(&self.0))
    }
}

impl ::std::ops::Deref for int128 {
    type Target = [u8];
    fn deref(&self) -> &[u8] { &self.0 }
}

impl Rand for int128 {
    fn rand<R: Rng>(rng: &mut R) -> Self {
        let mut ret: Self = Default::default();
        rng.fill_bytes(&mut ret.0);
        ret
    }
}

impl BareDeserialize for int128 {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        use std::io::Read;
        let mut ret: Self = Default::default();
        de.read(&mut ret.0)?;
        Ok(ret)
    }
}

impl BareSerialize for int128 {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        use std::io::Write;
        ser.write(&self.0)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Default)]
pub struct int256(pub [u8; 32]);

impl fmt::Debug for int256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", Hexlify(&self.0))
    }
}

impl ::std::ops::Deref for int256 {
    type Target = [u8];
    fn deref(&self) -> &[u8] { &self.0 }
}

impl Rand for int256 {
    fn rand<R: Rng>(rng: &mut R) -> Self {
        let mut ret: Self = Default::default();
        rng.fill_bytes(&mut ret.0);
        ret
    }
}

impl BareDeserialize for int256 {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        use std::io::Read;
        let mut ret: Self = Default::default();
        de.read(&mut ret.0)?;
        Ok(ret)
    }
}

impl BareSerialize for int256 {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        use std::io::Write;
        ser.write(&self.0)?;
        Ok(())
    }
}

pub struct TLObject(Box<AnyBoxedSerialize>);

impl TLObject {
    pub fn new<I: AnyBoxedSerialize>(inner: I) -> Self {
        TLObject(Box::new(inner))
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
        unimplemented!()
    }

    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
        unimplemented!()
    }
}

impl BoxedSerialize for TLObject {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
        self.0.serialize_boxed()
    }
}

#[derive(Debug, Clone)]
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
        unimplemented!()
    }
}

impl<T> BareSerialize for LengthPrefixed<T>
    where T: BoxedSerialize,
{
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        use std::io::Write;
        let inner = self.0.boxed_serialized_bytes()?;
        ser.write_bare(&(inner.len() as i32))?;
        ser.write(&inner)?;
        Ok(())
    }
}

impl BareSerialize for () {
    fn serialize_bare(&self, _ser: &mut Serializer) -> Result<()> {
        Ok(())
    }
}

impl BoxedDeserialize for bool {
    fn possible_constructors() -> Vec<ConstructorNumber> {
        unimplemented!()
    }

    fn deserialize_boxed(id: ConstructorNumber, de: &mut Deserializer) -> Result<Self> {
        unimplemented!()
    }
}

impl BoxedSerialize for bool {
    fn serialize_boxed<'this>(&'this self) -> (ConstructorNumber, &'this BareSerialize) {
        unimplemented!()
    }
}

impl BareDeserialize for String {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        unimplemented!()
    }
}

impl BareSerialize for String {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
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
        write!(f, "Vector({:?})", self.0)
    }
}

const VECTOR_CONSTRUCTOR: ConstructorNumber = ConstructorNumber(0);

macro_rules! impl_vector {
    ($det:ident, $det_de:ident, $det_ser:ident, $read_method:ident, $write_method:ident) => {

        impl<T> From<Vec<T>> for Vector<$det, T> {
            fn from(obj: Vec<T>) -> Self {
                Vector(obj, PhantomData)
            }
        }

        impl<T> BareDeserialize for Vector<$det, T>
            where T: $det_de,
        {
            fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
                unimplemented!()
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
                unimplemented!()
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

impl BareDeserialize for Vector<Bare, u8> {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
        unimplemented!()
    }
}

impl BareSerialize for Vector<Bare, u8> {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

impl_vector! { Bare, BareDeserialize, BareSerialize, read_bare, write_bare }
impl_vector! { Boxed, BoxedDeserialize, BoxedSerialize, read_boxed, write_boxed }

macro_rules! impl_tl_primitive {
    ($tltype:ident, $ptype:ident, $read:ident, $write:ident) => {
        pub type $tltype = $ptype;

        impl BareDeserialize for $ptype {
            fn deserialize_bare(de: &mut Deserializer) -> Result<Self> {
                use byteorder::{LittleEndian, ReadBytesExt};
                Ok(de.$read::<LittleEndian>()?)
            }
        }

        impl BareSerialize for $ptype {
            fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
                use byteorder::{LittleEndian, WriteBytesExt};
                ser.$write::<LittleEndian>(*self)?;
                Ok(())
            }
        }
    }
}

impl_tl_primitive! { int, i32, read_i32, write_i32 }
impl_tl_primitive! { long, i64, read_i64, write_i64 }
impl_tl_primitive! { double, f64, read_f64, write_f64 }

pub type Bool = bool;
pub type Flags = i32;
pub type lengthPrefixedTypedObject = LengthPrefixed<TypedObject>;
pub type string = String;
pub type TypedObject = TLObject;
