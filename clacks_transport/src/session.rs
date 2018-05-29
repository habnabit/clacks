use chrono::{DateTime, Duration, Utc, Timelike, TimeZone};
use clacks_crypto::CSRNG;
use clacks_crypto::symm::AuthKey;
use clacks_mtproto::{AnyBoxedSerialize, BareSerialize, BoxedSerialize, ConstructorNumber, IntoBoxed, mtproto};
use clacks_mtproto::mtproto::wire::outbound_encrypted::OutboundEncrypted;
use byteorder::{LittleEndian, ByteOrder, ReadBytesExt};
use rand::Rng;
use std::{cmp, io, mem};

use error::{ErrorKind, Result};


fn next_message_id() -> i64 {
    let time = Utc::now();
    let timestamp = time.timestamp() as i64;
    let nano = time.nanosecond() as i64;
    ((timestamp << 32) | (nano & 0x_7fff_fffc))
}

#[derive(Debug, Clone)]
pub struct AppId {
    pub api_id: i32,
    pub api_hash: String,
}

#[derive(Debug, Clone)]
struct Salt {
    valid_since: DateTime<Utc>,
    valid_until: DateTime<Utc>,
    salt: i64,
}

impl From<mtproto::FutureSalt> for Salt {
    fn from(fs: mtproto::FutureSalt) -> Self {
        Salt {
            valid_since: Utc.timestamp(*fs.valid_since() as i64, 0),
            valid_until: Utc.timestamp(*fs.valid_until() as i64, 0),
            salt: *fs.salt(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Session {
    session_id: i64,
    temp_session_id: Option<i64>,
    server_salts: Vec<Salt>,
    seq_no: i32,
    auth_key: Option<AuthKey>,
    to_ack: Vec<i64>,
    app_id: AppId,
}

#[derive(Debug)]
enum OutboundMessageWithoutId {
    Plain(mtproto::TLObject),
    Encrypted(mtproto::TLObject),
    BindPermKey { perm_key: AuthKey, expires_at: i32 },
}

#[derive(Debug)]
pub struct OutboundMessage {
    message_id: i64,
    inner: OutboundMessageWithoutId,
}

impl OutboundMessage {
    fn from_inner(inner: OutboundMessageWithoutId) -> Self {
        OutboundMessage { inner, message_id: CSRNG.gen() }
    }

    pub fn plain<P>(payload: P) -> Self
        where P: AnyBoxedSerialize,
    {
        Self::from_inner(OutboundMessageWithoutId::Plain(mtproto::TLObject::new(payload)))
    }

    pub fn encrypted<P>(payload: P) -> Self
        where P: AnyBoxedSerialize,
    {
        Self::from_inner(OutboundMessageWithoutId::Encrypted(mtproto::TLObject::new(payload)))
    }

    pub fn bind_temp_auth_key(perm_key: AuthKey, expires_at: i32) -> Self {
        Self::from_inner(OutboundMessageWithoutId::BindPermKey { perm_key, expires_at })
    }

    pub fn message_id(&self) -> i64 {
        self.message_id
    }

    pub fn constructor(&self) -> ConstructorNumber {
        use self::OutboundMessageWithoutId::*;
        match self.inner {
            Plain(ref o) |
            Encrypted(ref o) => o.serialize_boxed().0,
            BindPermKey {..} => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InboundMessage {
    pub message_id: i64,
    pub payload: Vec<u8>,
    pub was_encrypted: bool,
    pub seq_no: Option<i32>,
}

impl Session {
    pub fn new(app_id: AppId) -> Session {
        Session {
            app_id,
            session_id: CSRNG.gen(),
            temp_session_id: None,
            server_salts: vec![],
            seq_no: 0,
            auth_key: None,
            to_ack: vec![],
        }
    }

    fn next_content_seq_no(&mut self) -> i32 {
        let ret = self.seq_no | 1;
        self.seq_no += 2;
        ret
    }

    fn next_seq_no(&mut self, content_message: bool) -> i32 {
        if content_message {
            self.next_content_seq_no()
        } else {
            self.seq_no
        }
    }

    fn latest_server_salt(&mut self) -> Result<i64> {
        let time = {
            let last_salt = match self.server_salts.last() {
                Some(s) => s,
                None => return Err(ErrorKind::NoSalts.into()),
            };
            // Make sure at least one salt is retained.
            cmp::min(Utc::now(), last_salt.valid_until.clone())
        };
        self.server_salts.retain(|s| &s.valid_until >= &time);
        Ok(self.server_salts.first().unwrap().salt)
    }

    pub fn add_server_salts<I>(&mut self, salts: I)
        where I: IntoIterator<Item = mtproto::FutureSalt>,
    {
        self.server_salts.extend(salts.into_iter().map(Into::into));
        self.server_salts.sort_by(|a, b| a.valid_since.cmp(&b.valid_since));
    }

    pub fn adopt_key(&mut self, authorization_key: AuthKey) {
        self.auth_key = Some(authorization_key);
    }

    pub fn ack_id(&mut self, id: i64) {
        self.to_ack.push(id);
    }

    fn plain_payload(&mut self, payload: mtproto::TLObject, message_id: i64) -> Result<Vec<u8>> {
        Ok(mtproto::wire::outbound_raw::OutboundRaw {
            message_id,
            auth_key_id: 0,
            payload: payload.into(),
        }.bare_serialized_bytes()?)
    }

    fn pack_message_container<I>(&mut self, payloads: I) -> mtproto::manual::MessageContainer
        where I: IntoIterator<Item = (bool, mtproto::TLObject)>,
    {
        let messages: Vec<_> = payloads.into_iter()
            .map(|(content_message, payload)| {
                mtproto::manual::basic_message::BasicMessage {
                    msg_id: next_message_id(),
                    seqno: self.next_seq_no(content_message),
                    body: payload.into(),
                }
            })
            .collect();
        mtproto::manual::msg_container::MsgContainer {
            messages: messages.into(),
        }.into_boxed()
    }

    fn fresh_auth_key(&self) -> Result<AuthKey> {
        match self.auth_key {
            Some(ref key) => Ok(key.clone()),
            None => Err(ErrorKind::NoAuthKey.into()),
        }
    }

    fn encrypted_payload_inner(&mut self, payload: mtproto::TLObject, key: Option<AuthKey>, message_id: i64, content_message: bool) -> Result<Vec<u8>> {
        let key = key.map(Ok).unwrap_or_else(|| self.fresh_auth_key())?;
        let salt = self.latest_server_salt()?;
        let message = OutboundEncrypted {
            salt, message_id,
            session_id: self.session_id,
            seq_no: self.next_seq_no(content_message),
            payload: payload.into(),
        };
        Ok(key.encrypt_message(message)?)
    }

    fn pack_encrypted_payload_with_acks(&mut self, payload: mtproto::TLObject, message_id: i64) -> Result<Vec<u8>> {
        let acks = mtproto::TLObject::new(mtproto::msgs_ack::MsgsAck {
            msg_ids: mem::replace(&mut self.to_ack, vec![]).into(),
        }.into_boxed());
        let combined = self.pack_message_container(vec![(false, acks), (true, payload)]);
        self.encrypted_payload_inner(mtproto::TLObject::new(combined), None, message_id, false)
    }

    fn encrypted_payload(&mut self, payload: mtproto::TLObject, message_id: i64) -> Result<Vec<u8>> {
        if self.to_ack.is_empty() {
            self.encrypted_payload_inner(payload, None, message_id, true)
        } else {
            self.pack_encrypted_payload_with_acks(payload, message_id)
        }
    }

    fn bind_auth_key(&mut self, perm_key: AuthKey, expires_at: i32, message_id: i64) -> Result<Vec<u8>> {
        let temp_key = self.fresh_auth_key()?;
        let (temp_session_id, bind_message) = perm_key.bind_temp_auth_key(
            &temp_key, expires_at, message_id, &mut CSRNG)?;
        self.temp_session_id = Some(temp_session_id);
        self.encrypted_payload_inner(mtproto::TLObject::new(bind_message), Some(temp_key), message_id, true)
    }

    pub fn serialize_message(&mut self, message: OutboundMessage) -> Result<Vec<u8>> {
        use self::OutboundMessageWithoutId::*;
        let OutboundMessage { message_id, inner } = message;
        match inner {
            Plain(p) => self.plain_payload(p, message_id),
            Encrypted(p) => self.encrypted_payload(p, message_id),
            BindPermKey { perm_key, expires_at } => self.bind_auth_key(perm_key, expires_at, message_id),
        }
    }

    pub fn process_message(&self, message: &[u8]) -> Result<InboundMessage> {
        if message.len() == 4 {
            return Err(ErrorKind::ErrorCode(LittleEndian::read_i32(&message)).into());
        } else if message.len() < 8 {
            panic!("bad message");
        }

        let mut cursor = io::Cursor::new(message);
        let auth_key_id = cursor.read_i64::<LittleEndian>()?;
        if auth_key_id != 0 {
            cursor.into_inner();
            return self.decrypt_message(message);
        }

        let message_id = cursor.read_i64::<LittleEndian>()?;
        let len = cursor.read_i32::<LittleEndian>()? as usize;
        let pos = cursor.position() as usize;
        cursor.into_inner();
        if message.len() < pos + len {
            return Err(ErrorKind::AuthenticationFailure.into());
        }
        let payload = &message[pos..pos+len];
        Ok(InboundMessage {
            message_id: message_id,
            payload: payload.into(),
            was_encrypted: false,
            seq_no: None,
        })
    }

    fn decrypt_message(&self, message: &[u8]) -> Result<InboundMessage> {
        let (inbound, payload) = self.fresh_auth_key()?.decrypt_and_verify_message(message)?;
        if inbound.session_id != self.session_id && Some(inbound.session_id) != self.temp_session_id {
            return Err(ErrorKind::AuthenticationFailure.into());
        }
        if !self.server_salts.iter().any(|s| s.salt == inbound.salt) {
            println!("salt failure: {} not in {:#?}", inbound.salt, self.server_salts);
        }
        Ok(InboundMessage {
            payload,
            message_id: inbound.message_id,
            was_encrypted: true,
            seq_no: Some(inbound.seq_no),
        })
    }
}

pub fn future_salt_from_negotiated_salt(salt: i64) -> mtproto::FutureSalt {
    let time = Utc::now();
    mtproto::future_salt::FutureSalt {
        salt,
        valid_since: time.timestamp() as i32,
        valid_until: (time + Duration::minutes(10)).timestamp() as i32,
    }.into_boxed()
}
