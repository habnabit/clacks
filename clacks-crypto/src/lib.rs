#![recursion_limit = "128"]

#[macro_use] extern crate error_chain;
extern crate byteorder;
extern crate clacks_mtproto;
extern crate openssl;
extern crate rand;

pub mod error {
    error_chain! {
        links {
            Mtproto(::clacks_mtproto::error::Error, ::clacks_mtproto::error::ErrorKind);
        }

        foreign_links {
            Io(::std::io::Error);
            Utf8(::std::str::Utf8Error);
            FromUtf8(::std::string::FromUtf8Error);
            Openssl(::openssl::error::ErrorStack);
        }

        errors {
            InvalidData {}
            BoxedAsBare {}
            ReceivedSendType {}
            UnsupportedLayer {}
            NoAuthKey {}
            NoSalts {}
            WrongAuthKey {}
            InvalidLength {}
            Unknown {}
            FactorizationFailure {}
            AuthenticationFailure {}
        }
    }
}

pub mod asymm;
pub mod symm;

enum Padding {
    Total255,
    Mod16,
}

fn sha1_bytes(parts: &[&[u8]]) -> ::error::Result<::openssl::hash::DigestBytes> {
    let mut hasher = ::openssl::hash::Hasher::new(::openssl::hash::MessageDigest::sha1())?;
    for part in parts {
        hasher.update(part)?;
    }
    Ok(hasher.finish()?)
}

fn sha1_and_or_pad(input: &[u8], prepend_sha1: bool, padding: Padding) -> ::error::Result<Vec<u8>> {
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