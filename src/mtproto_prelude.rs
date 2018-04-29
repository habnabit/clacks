use ::{AnyBoxedSerialize, BareDeserialize, BareSerialize, Deserialize, Deserializer, Result, Serialize, Serializer};
use extfmt::Hexlify;
use std::fmt;
use std::marker::PhantomData;
use void::Void;

#[derive(Clone)]
pub struct bytes(pub Vec<u8>);

impl fmt::Debug for bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{} bytes>", self.0.len())
    }
}

impl BareDeserialize for bytes {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self>
        where Self: Sized
    {
        unimplemented!()
    }
}

impl BareSerialize for bytes {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

impl Deserialize for bytes {
    type Output = Self;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl Serialize for bytes {
    type Input = Self;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
pub struct Bytes8(pub [u8; 8]);
pub type int128 = Bytes8;

impl fmt::Debug for Bytes8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", Hexlify(&self.0))
    }
}

impl BareDeserialize for Bytes8 {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self>
        where Self: Sized
    {
        use std::io::Read;
        let mut buf = [0u8; 8];
        de.read(&mut buf)?;
        Ok(Bytes8(buf))
    }
}

impl BareSerialize for Bytes8 {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        use std::io::Write;
        ser.write(&self.0)?;
        Ok(())
    }
}

impl Deserialize for Bytes8 {
    type Output = Self;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl Serialize for Bytes8 {
    type Input = Self;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
pub struct Bytes16(pub [u8; 16]);
pub type int256 = Bytes16;

impl fmt::Debug for Bytes16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", Hexlify(&self.0))
    }
}

impl BareDeserialize for Bytes16 {
    fn deserialize_bare(de: &mut Deserializer) -> Result<Self>
        where Self: Sized
    {
        use std::io::Read;
        let mut buf = [0u8; 16];
        de.read(&mut buf)?;
        Ok(Bytes16(buf))
    }
}

impl BareSerialize for Bytes16 {
    fn serialize_bare(&self, ser: &mut Serializer) -> Result<()> {
        use std::io::Write;
        ser.write(&self.0)?;
        Ok(())
    }
}

impl Deserialize for Bytes16 {
    type Output = Self;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl Serialize for Bytes16 {
    type Input = Self;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

pub struct TLObject(Box<AnyBoxedSerialize>);

impl Clone for TLObject {
    fn clone(&self) -> Self {
        unimplemented!()
    }
}

impl fmt::Debug for TLObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(TLObject tl_id:{:?})", self.0.type_id())
    }
}

pub struct LengthPrefixed<T>(PhantomData<fn(&T) -> T>, Void);
pub enum TypedObject {}
pub type LengthPrefixedTypedObject = LengthPrefixed<TypedObject>;

impl<T: Deserialize> Deserialize for LengthPrefixed<T> {
    type Output = T::Output;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl<T: Serialize> Serialize for LengthPrefixed<T> {
    type Input = T::Input;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

impl Deserialize for TypedObject {
    type Output = TLObject;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl Serialize for TypedObject {
    type Input = TLObject;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

pub enum Bool {}

impl Deserialize for Bool {
    type Output = bool;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

pub enum string {}

impl Deserialize for string {
    type Output = String;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl Serialize for string {
    type Input = String;  // XXX: for now?
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

macro_rules! impl_tl_primitive {
    ($tltype:ident, $ptype:ident, $read:ident, $write:ident) => {
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

        pub enum $tltype {}

        impl Deserialize for $tltype {
            type Output = $ptype;
            fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
                de.read_bare::<$ptype>()
            }
        }

        impl Serialize for $tltype {
            type Input = $ptype;
            fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
                ser.write_bare::<$ptype>(obj)
            }
        }
    }
}

impl_tl_primitive! { int, i32, read_i32, write_i32 }
impl_tl_primitive! { long, i64, read_i64, write_i64 }
impl_tl_primitive! { double, f64, read_f64, write_f64 }

pub struct vector<T: ?Sized>(PhantomData<fn(&T) -> T>, Void);

impl<T: ?Sized + Deserialize> Deserialize for vector<T> {
    type Output = Vec<T::Output>;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl<T: Serialize> Serialize for vector<T>
    where T::Input: Sized,
{
    type Input = [T::Input];
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

pub struct Vector<T: ?Sized>(PhantomData<fn(&T) -> T>, Void);

impl<T: ?Sized + Deserialize> Deserialize for Vector<T> {
    type Output = Vec<T::Output>;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl<T: Serialize> Serialize for Vector<T>
    where T::Input: Sized,
{
    type Input = [T::Input];
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}

pub enum Flags {}

impl Deserialize for Flags {
    type Output = i32;
    fn deserialize(de: &mut Deserializer) -> Result<Self::Output> {
        unimplemented!()
    }
}

impl Serialize for Flags {
    type Input = i32;
    fn serialize(obj: &Self::Input, ser: &mut Serializer) -> Result<()> {
        unimplemented!()
    }
}
