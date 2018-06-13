#![deny(private_in_public, unused_extern_crates)]

#[macro_use] extern crate failure;
extern crate byteorder;
extern crate chrono;
extern crate clacks_mtproto;
extern crate openssl;
extern crate rand;

pub mod asymm;
pub mod symm;


pub type Result<T> = ::std::result::Result<T, ::failure::Error>;

enum Padding {
    Total255,
    Mod16,
}

pub fn sha1_bytes(parts: &[&[u8]]) -> Result<::openssl::hash::DigestBytes> {
    let mut hasher = ::openssl::hash::Hasher::new(::openssl::hash::MessageDigest::sha1())?;
    for part in parts {
        hasher.update(part)?;
    }
    Ok(hasher.finish()?)
}

fn sha1_and_or_pad(input: &[u8], prepend_sha1: bool, padding: Padding) -> Result<Vec<u8>> {
    let mut ret = if prepend_sha1 {
        sha1_bytes(&[input])?.to_vec()
    } else {
        vec![]
    };
    ret.extend(input);
    match padding {
        Padding::Total255 => {
            while ret.len() < 255 {
                ret.push(0);
            }
        },
        Padding::Mod16 if ret.len() % 16 != 0 => {
            for _ in 0..16 - (ret.len() % 16) {
                ret.push(0);
            }
        },
        _ => (),
    }
    Ok(ret)
}

pub struct CSRNG;

impl rand::Rng for CSRNG {
    fn next_u32(&mut self) -> u32 {
        let mut buf = [0u8; 4];
        self.fill_bytes(&mut buf);
        <byteorder::NativeEndian as byteorder::ByteOrder>::read_u32(&buf)
    }

    fn next_u64(&mut self) -> u64 {
        let mut buf = [0u8; 8];
        self.fill_bytes(&mut buf);
        <byteorder::NativeEndian as byteorder::ByteOrder>::read_u64(&buf)
    }

    fn fill_bytes(&mut self, dest: &mut [u8]) {
        ::openssl::rand::rand_bytes(dest).unwrap();
    }
}

pub fn csrng_gen<R: rand::Rand>() -> R { rand::Rand::rand(&mut CSRNG) }
