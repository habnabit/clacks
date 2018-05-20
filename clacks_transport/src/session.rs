use chrono::{DateTime, Duration, Utc, Timelike, TimeZone};
use clacks_crypto::symm::AuthKey;
use clacks_mtproto::{AnyBoxedSerialize, BareSerialize, IntoBoxed, mtproto};
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
    pub app_id: AppId,
}

#[derive(Debug, Clone)]
pub struct OutboundMessage {
    pub message_id: i64,
    pub message: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct InboundMessage {
    pub message_id: i64,
    pub payload: Vec<u8>,
    pub was_encrypted: bool,
    pub seq_no: Option<i32>,
}

impl Session {
    pub fn new(session_id: i64, app_id: AppId) -> Session {
        Session {
            session_id, app_id,
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

    fn encrypted_payload_inner(&mut self, payload: mtproto::TLObject, content_message: bool) -> Result<OutboundMessage> {
        let key = self.fresh_auth_key()?;
        let salt = self.latest_server_salt()?;
        let message_id = next_message_id();
        let message = OutboundEncrypted {
            salt, message_id,
            session_id: self.session_id,
            seq_no: self.next_seq_no(content_message),
            payload: payload.into(),
        };
        Ok(OutboundMessage {
            message_id, message: key.encrypt_message(message)?,
        })
    }

    fn pack_encrypted_payload_with_acks(&mut self, payload: mtproto::TLObject) -> Result<OutboundMessage> {
        let acks = mtproto::TLObject::new(mtproto::msgs_ack::MsgsAck {
            msg_ids: mem::replace(&mut self.to_ack, vec![]).into(),
        }.into_boxed());
        let combined = self.pack_message_container(vec![(false, acks), (true, payload)]);
        // The message id of the interior message which was 'payload'.
        let message_id = combined.messages()[1].msg_id;
        let mut ret = self.encrypted_payload_inner(mtproto::TLObject::new(combined), false)?;
        ret.message_id = message_id;
        Ok(ret)
    }

    pub fn encrypted_payload<P>(&mut self, payload: P) -> Result<OutboundMessage>
        where P: AnyBoxedSerialize,
    {
        let payload = mtproto::TLObject::new(payload);
        if self.to_ack.is_empty() {
            self.encrypted_payload_inner(payload, true)
        } else {
            self.pack_encrypted_payload_with_acks(payload)
        }
    }

    pub fn plain_payload<P>(&mut self, payload: P) -> Result<OutboundMessage>
        where P: AnyBoxedSerialize,
    {
        let message_id = next_message_id();
        let message = mtproto::wire::outbound_raw::OutboundRaw {
            message_id,
            auth_key_id: 0,
            payload: mtproto::TLObject::new(payload).into(),
        }.bare_serialized_bytes()?;
        Ok(OutboundMessage { message_id, message })
    }

    pub fn assemble_payload<P>(&mut self, payload: P, encrypt: bool) -> Result<OutboundMessage>
        where P: AnyBoxedSerialize,
    {
        if encrypt {
            self.encrypted_payload(payload)
        } else {
            self.plain_payload(payload)
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

    pub fn bind_from_permanent_auth_key<R: Rng>(&mut self, perm_key: AuthKey, expires_at: i32, rng: &mut R)
                                                -> Result<OutboundMessage> {
        let temp_key = self.fresh_auth_key()?;
        let salt = self.latest_server_salt()?;
        let message_id = next_message_id();
        let (temp_session_id, bind_message) = perm_key.bind_temp_auth_key(&temp_key, expires_at, message_id, rng)?;
        let message = OutboundEncrypted {
            salt, message_id,
            session_id: temp_session_id,
            seq_no: self.next_seq_no(true),
            payload: mtproto::TLObject::new(bind_message).into(),
        };
        self.temp_session_id = Some(temp_session_id);
        Ok(OutboundMessage {
            message_id,
            message: temp_key.encrypt_message(message)?,
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
