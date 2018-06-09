use chrono::{DateTime, Duration, Utc, Timelike, TimeZone};
use clacks_crypto::CSRNG;
use clacks_crypto::symm::AuthKey;
use clacks_mtproto::{AnyBoxedSerialize, BareSerialize, BoxedSerialize, ConstructorNumber, IntoBoxed, mtproto};
use clacks_mtproto::mtproto::wire::outbound_encrypted::OutboundEncrypted;
use clacks_mtproto::mtproto::wire::outbound_raw::OutboundRaw;
use byteorder::{LittleEndian, ByteOrder, ReadBytesExt};
use either::Either;
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

#[derive(Debug, Default)]
pub struct PlainPayload {
    dummy: (),
}

#[derive(Debug, Default)]
pub struct EncryptedPayload {
    session_id: Option<i64>,
}

pub struct MessageBuilder<P> {
    message_id: i64,
    payload: mtproto::TLObject,
    payload_opts: P,
}

pub type EitherMessageBuilder = MessageBuilder<Either<PlainPayload, EncryptedPayload>>;

impl<PO: Default> MessageBuilder<PO> {
    fn with_message_id<P>(message_id: i64, payload: P) -> Self
        where P: AnyBoxedSerialize,
    {
        let payload = mtproto::TLObject::new(payload).into();
        MessageBuilder {
            message_id, payload,
            payload_opts: Default::default(),
        }
    }

    pub fn new<P>(payload: P) -> Self
        where P: AnyBoxedSerialize,
    {
        Self::with_message_id(next_message_id(), payload)
    }
}

impl<PO> MessageBuilder<PO> {
    pub fn message_id(&self) -> i64 {
        self.message_id
    }

    pub fn constructor(&self) -> ConstructorNumber {
        self.payload.serialize_boxed().0
    }

    fn seq_no_from<SNF>(&self, seq_no_func: SNF) -> i32
        where SNF: FnOnce(bool) -> i32
    {
        seq_no_func(is_content_message(self.constructor()))
    }

    pub fn into_basic_message<SNF>(self, seq_no_func: SNF) -> mtproto::manual::basic_message::BasicMessage
        where SNF: FnOnce(bool) -> i32,
    {
        mtproto::manual::basic_message::BasicMessage {
            msg_id: self.message_id,
            seqno: self.seq_no_from(seq_no_func),
            body: self.payload.into(),
        }
    }
}

impl MessageBuilder<PlainPayload> {
    pub fn into_outbound_raw(self) -> OutboundRaw {
        OutboundRaw {
            auth_key_id: 0,
            message_id: self.message_id,
            payload: self.payload.into(),
        }
    }

    pub fn lift(self) -> EitherMessageBuilder {
        MessageBuilder {
            message_id: self.message_id,
            payload: self.payload,
            payload_opts: Either::Left(self.payload_opts),
        }
    }
}

impl MessageBuilder<EncryptedPayload> {
    pub fn with_session_id(mut self, session_id: i64) -> Self {
        self.payload_opts.session_id = Some(session_id);
        self
    }

    pub fn into_outbound_encrypted<SNF>(self, salt: i64, session_id: i64, seq_no_func: SNF) -> OutboundEncrypted
        where SNF: FnOnce(bool) -> i32,
    {
        OutboundEncrypted {
            salt,
            session_id: self.payload_opts.session_id.unwrap_or(session_id),
            message_id: self.message_id,
            seq_no: self.seq_no_from(seq_no_func),
            payload: self.payload.into(),
        }
    }

    pub fn lift(self) -> EitherMessageBuilder {
        MessageBuilder {
            message_id: self.message_id,
            payload: self.payload,
            payload_opts: Either::Right(self.payload_opts),
        }
    }
}

impl EitherMessageBuilder {
    pub fn plain<P>(payload: P) -> Self
        where P: AnyBoxedSerialize,
    {
        MessageBuilder::<PlainPayload>::new(payload).lift()
    }

    pub fn encrypted<P>(payload: P) -> Self
        where P: AnyBoxedSerialize,
    {
        MessageBuilder::<EncryptedPayload>::new(payload).lift()
    }

    pub fn separate(self) -> Either<MessageBuilder<PlainPayload>, MessageBuilder<EncryptedPayload>> {
        let MessageBuilder { message_id, payload, payload_opts: e } = self;
        match e {
            Either::Left(payload_opts) => Either::Left(MessageBuilder { message_id, payload, payload_opts }),
            Either::Right(payload_opts) => Either::Right(MessageBuilder { message_id, payload, payload_opts }),
        }
    }
}

fn is_content_message(n: ConstructorNumber) -> bool {
    // XXX: there has to be a better way
    match n {
        ConstructorNumber(0x62d6b459) |
        ConstructorNumber(0x73f1f8dc) => false,
        _ => true,
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

    fn pack_message_container<PO, I>(&mut self, payloads: I) -> mtproto::manual::MessageContainer
        where I: IntoIterator<Item = MessageBuilder<PO>>,
    {
        let messages: Vec<_> = payloads.into_iter()
            .map(|m| m.into_basic_message(|c| self.next_seq_no(c)))
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

    fn pack_payload_with_acks<PO: Default>(&mut self, payload: MessageBuilder<PO>) -> MessageBuilder<PO> {
        if self.to_ack.is_empty() {
            return payload;
        };
        let acks = MessageBuilder::new(mtproto::msgs_ack::MsgsAck {
            msg_ids: mem::replace(&mut self.to_ack, vec![]).into(),
        }.into_boxed());
        MessageBuilder::new(self.pack_message_container(vec![payload, acks]))
    }

    pub fn serialize_plain_message(&mut self, message: MessageBuilder<PlainPayload>) -> Result<Vec<u8>> {
        Ok(message.into_outbound_raw().bare_serialized_bytes()?)
    }

    pub fn serialize_encrypted_message(&mut self, message: MessageBuilder<EncryptedPayload>) -> Result<Vec<u8>> {
        let key = self.fresh_auth_key()?;
        let message = self.pack_payload_with_acks(message)
            .into_outbound_encrypted(
                self.latest_server_salt()?, self.session_id,
                |c| self.next_seq_no(c));
        println!("---out---\n{}\n{:?}\n---", ::serde_json::to_string(&message).unwrap_or_default(), message);
        Ok(key.encrypt_message(message)?)
    }

    pub fn serialize_message(&mut self, message: EitherMessageBuilder) -> Result<Vec<u8>> {
        match message.separate() {
            Either::Left(m) => self.serialize_plain_message(m),
            Either::Right(m) => self.serialize_encrypted_message(m),
        }
    }

    pub fn bind_auth_key(&mut self, perm_key: AuthKey, expires_in: Duration) -> Result<MessageBuilder<EncryptedPayload>> {
        let temp_key = self.fresh_auth_key()?;
        let message_id = next_message_id();
        let (session_id, bind_message) = perm_key.bind_temp_auth_key(&temp_key, expires_in, message_id)?;
        self.temp_session_id = Some(session_id);
        Ok({
            MessageBuilder::with_message_id(message_id, bind_message)
                .with_session_id(session_id)
        })
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
