use failure::{self, Error};
use keyring;
use serde_json;
use serde;

type Result<T> = ::std::result::Result<T, Error>;

const KEYRING_SERVICE: &'static str = "clacks_agentd";


pub trait Kind {
    type Stored: serde::Serialize + for<'a> serde::Deserialize<'a>;
    fn username(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct Entry<K> {
    kind: K,
    username: String,
}

impl<K: Kind> Entry<K> {
    pub fn new(kind: K) -> Self {
        let username = kind.username();
        Entry { kind, username }
    }

    fn keyring(&self) -> keyring::Keyring {
        keyring::Keyring::new(KEYRING_SERVICE, &self.username)
    }

    pub fn get(&self) -> Result<Option<K::Stored>> {
        match self.keyring().get_password() {
            Ok(ref s) if !s.is_empty() => Ok(Some(serde_json::from_str(&s)?)),
            Ok(_) |
            Err(keyring::KeyringError::NoPasswordFound) => Ok(None),

            #[cfg(target_os = "macos")]
            Err(keyring::KeyringError::MacOsKeychainError(ref e)) if e.code() == -25300 => Ok(None),

            Err(e) => Err(e.into()),
        }
    }

    pub fn set(&self, value: &K::Stored) -> Result<()> {
        let serialized = serde_json::to_string(value)?;
        self.keyring().set_password(&serialized)?;
        Ok(())
    }
}

impl<K> ::std::ops::Deref for Entry<K> {
    type Target = K;
    fn deref(&self) -> &K { &self.kind }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppKeyV1 {
    pub api_id: i32,
    pub api_hash: String,
}

impl AppKeyV1 {
    pub fn as_app_id(&self) -> ::clacks_transport::AppId {
        ::clacks_transport::AppId {
            api_id: self.api_id,
            api_hash: self.api_hash.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AppKey;

impl Kind for AppKey {
    type Stored = AppKeyV1;
    fn username(&self) -> String { "_appkey".to_string() }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserAuthKeyV1 {
    pub auth_key: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct UserAuthKey {
    pub phone_number: String,
}

impl Kind for UserAuthKey {
    type Stored = UserAuthKeyV1;
    fn username(&self) -> String { format!("user+{}", self.phone_number) }
}
