use byteorder::{LittleEndian, ByteOrder, WriteBytesExt};
use chrono::Duration;
use clacks_mtproto::{BareSerialize, Function, IntoBoxed, mtproto};
use clacks_mtproto::mtproto::wire::inbound_encrypted::InboundEncrypted;
use clacks_mtproto::mtproto::wire::outbound_encrypted::OutboundEncrypted;
use openssl::{aes, symm};
use std::fmt;
use std::io::{Cursor, Write};

use {Padding, Result, csrng_gen, sha1_and_or_pad, sha1_bytes};


#[derive(Debug, Fail)]
pub enum AuthenticationFailure {
    #[fail(display = "incorrect key received")]
    BadKey,
    #[fail(display = "incorrect length received")]
    BadLength,
    #[fail(display = "incorrect hash received")]
    BadHash,
}

#[derive(Default, Clone, Copy)]
pub struct AesParams {
    key: [u8; 32],
    iv: [u8; 32],
}

impl fmt::Debug for AesParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AesParams")
    }
}

#[derive(Debug, Fail)]
#[fail(display = "input AES key was invalid")]
pub struct BadAesKey;

impl AesParams {
    fn run_ige(mut self, input: &[u8], mode: symm::Mode) -> Result<Vec<u8>> {
        let key = match mode {
            symm::Mode::Encrypt => aes::AesKey::new_encrypt(&self.key),
            symm::Mode::Decrypt => aes::AesKey::new_decrypt(&self.key),
        }.map_err(|_: aes::KeyError| BadAesKey)?;
        let mut output = vec![0; input.len()];
        aes::aes_ige(input, &mut output, &key, &mut self.iv, mode);
        Ok(output)
    }

    pub fn ige_encrypt(self, decrypted: &[u8], prepend_sha1: bool) -> Result<Vec<u8>> {
        let input = sha1_and_or_pad(decrypted, prepend_sha1, Padding::Mod16)?;
        self.run_ige(&input, symm::Mode::Encrypt)
    }

    pub fn ige_decrypt(self, encrypted: &[u8]) -> Result<Vec<u8>> {
        self.run_ige(encrypted, symm::Mode::Decrypt)
    }

    pub fn from_pq_inner_data(data: &mtproto::PQInnerData) -> Result<AesParams> {
        let sha1_a = sha1_bytes(&[data.new_nonce(), data.server_nonce()])?;
        let sha1_b = sha1_bytes(&[data.server_nonce(), data.new_nonce()])?;
        let sha1_c = sha1_bytes(&[data.new_nonce(), data.new_nonce()])?;
        let mut ret: AesParams = Default::default();
        set_slice_parts(&mut ret.key, &[&sha1_a, &sha1_b[..12]]);
        set_slice_parts(&mut ret.iv, &[&sha1_b[12..], &sha1_c, &data.new_nonce()[..4]]);
        Ok(ret)
    }
}

fn set_slice_parts(result: &mut [u8], parts: &[&[u8]]) {
    let mut cursor = Cursor::new(result);
    for part in parts {
        cursor.write(part).unwrap();
    }
}

pub struct AuthKey {
    auth_key: [u8; AuthKey::SIZE],
    aux_hash: i64,
    fingerprint: i64,
}

impl Clone for AuthKey {
    fn clone(&self) -> AuthKey {
        AuthKey {
            auth_key: self.auth_key,
            aux_hash: self.aux_hash,
            fingerprint: self.fingerprint,
        }
    }
}

impl fmt::Debug for AuthKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AuthKey(#{:08x})", self.fingerprint)
    }
}

impl AuthKey {
    pub const SIZE: usize = 256;

    pub fn new(key_in: &[u8]) -> Result<AuthKey> {
        let mut key = [0u8; AuthKey::SIZE];
        let size_diff = (AuthKey::SIZE as isize) - (key_in.len() as isize);
        if size_diff > 0 {
            // key longer than key_in
            (&mut key[size_diff as usize..]).copy_from_slice(key_in);
        } else if size_diff < 0 {
            // key_in longer than key
            Err(format_err!("auth key too short"))?
        } else {
            key.copy_from_slice(key_in);
        }
        let sha1 = sha1_bytes(&[&key])?;
        let aux_hash = LittleEndian::read_i64(&sha1[0..8]);
        let fingerprint = LittleEndian::read_i64(&sha1[12..20]);
        Ok(AuthKey {
            auth_key: key,
            aux_hash: aux_hash,
            fingerprint: fingerprint,
        })
    }

    fn generate_message_aes_params(&self, msg_key: &[u8], mode: symm::Mode) -> Result<AesParams> {
        let mut pos = match mode {
            symm::Mode::Encrypt => 0,
            symm::Mode::Decrypt => 8,
        };
        let mut auth_key_take = |len| {
            let ret = &self.auth_key[pos..pos+len];
            pos += len;
            ret
        };
        let sha1_a = sha1_bytes(&[msg_key, auth_key_take(32)])?;
        let sha1_b = sha1_bytes(&[auth_key_take(16), msg_key, auth_key_take(16)])?;
        let sha1_c = sha1_bytes(&[auth_key_take(32), msg_key])?;
        let sha1_d = sha1_bytes(&[msg_key, auth_key_take(32)])?;

        let mut ret: AesParams = Default::default();
        set_slice_parts(&mut ret.key, &[&sha1_a[0..8], &sha1_b[8..20], &sha1_c[4..16]]);
        set_slice_parts(&mut ret.iv, &[&sha1_a[8..20], &sha1_b[0..8], &sha1_c[16..20], &sha1_d[0..8]]);
        Ok(ret)
    }

    pub fn new_nonce_hash(&self, which: u8, new_nonce: mtproto::int256) -> Result<mtproto::int128> {
        let mut input = [0u8; 41];
        {
            let mut cursor = Cursor::new(&mut input[..]);
            cursor.write(&new_nonce).unwrap();
            cursor.write_u8(which).unwrap();
            cursor.write_i64::<LittleEndian>(self.aux_hash).unwrap();
        }
        let sha1 = sha1_bytes(&[&input])?;
        let mut ret = [0u8; 16];
        ret.copy_from_slice(&sha1[4..20]);
        Ok(mtproto::int128(ret))
    }

    pub fn encrypt_message(&self, message: OutboundEncrypted) -> Result<Vec<u8>> {
        let message = message.bare_serialized_bytes()?;
        let message_hash = sha1_bytes(&[&message])?;
        let message_key = &message_hash[4..20];
        let aes = self.generate_message_aes_params(message_key, symm::Mode::Encrypt)?;
        let mut ret = vec![0u8; 8];
        LittleEndian::write_i64(&mut ret, self.fingerprint);
        ret.extend(message_key);
        ret.extend(aes.ige_encrypt(&message, false)?);
        Ok(ret)
    }

    fn decrypt_message(&self, message: &[u8]) -> Result<Vec<u8>> {
        if LittleEndian::read_i64(&message[..8]) != self.fingerprint {
            Err(AuthenticationFailure::BadKey)?
        }
        let message_key = &message[8..24];
        let aes = self.generate_message_aes_params(message_key, symm::Mode::Decrypt)?;
        aes.ige_decrypt(&message[24..])
    }

    pub fn decrypt_and_verify_message(&self, message: &[u8]) -> Result<(InboundEncrypted, Vec<u8>)> {
        let decrypted = self.decrypt_message(message)?;
        let (inbound, payload) = {
            let mut bytes = &decrypted[..];
            let inbound: InboundEncrypted = ::clacks_mtproto::Deserializer::new(&mut bytes).read_bare()?;
            (inbound, bytes)
        };
        let payload_len = inbound.payload_len as usize;
        let padding = match payload.len().checked_sub(payload_len) {
            Some(i) => i,
            None => Err(AuthenticationFailure::BadLength)?,
        };
        let payload = &payload[..payload_len];
        let computed_message_hash = sha1_bytes(&[&decrypted[..decrypted.len() - padding]])?;
        if &message[8..24] != &computed_message_hash[4..20] {
            Err(AuthenticationFailure::BadHash)?;
        }
        Ok((inbound, payload.into()))
    }

    pub fn into_inner(self) -> [u8; AuthKey::SIZE] {
        self.auth_key
    }

    pub fn downcast_bind_temp_auth_key(obj: mtproto::TLObject) -> ::std::result::Result<bool, mtproto::TLObject> {
        obj.downcast::<<mtproto::rpc::auth::BindTempAuthKey as Function>::Reply>()
            .map(Into::into)
    }

    pub fn bind_temp_auth_key(self, temp_key: &AuthKey, expires_in: Duration, message_id: i64)
                             -> Result<(i64, mtproto::rpc::auth::BindTempAuthKey)>
    {
        let expires_in = expires_in.num_seconds();
        assert!(expires_in < ::std::i32::MAX as i64);
        let expires_at = expires_in as i32;
        let nonce: i64 = csrng_gen();
        let temp_session_id: i64 = csrng_gen();
        let inner = mtproto::manual::bind_auth_key_inner::BindAuthKeyInner {
            nonce, expires_at, temp_session_id,
            temp_auth_key_id: temp_key.fingerprint,
            perm_auth_key_id: self.fingerprint,
        }.into_boxed();
        let inner = OutboundEncrypted {
            message_id,
            salt: csrng_gen(),
            session_id: csrng_gen(),
            seq_no: 0,
            payload: (mtproto::TLObject::new(inner)).into(),
        };
        Ok((temp_session_id, mtproto::rpc::auth::BindTempAuthKey {
            nonce, expires_at,
            perm_auth_key_id: self.fingerprint,
            encrypted_message: mtproto::bytes(self.encrypt_message(inner)?),
        }))
    }
}
