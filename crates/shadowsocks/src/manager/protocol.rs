//! Shadowsocks server manager protocol

use std::{
    collections::HashMap,
    io::{self, ErrorKind},
    str,
    string::ToString,
};

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Abstract Manager Protocol
pub trait ManagerProtocol: Sized {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error>;
    fn to_bytes(&self) -> Result<Vec<u8>, Error>;
}

/// Server's configuration
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ServerConfig {
    pub server_port: u16,
    pub password: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub method: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub no_delay: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub plugin: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub plugin_opts: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
}

/// `add` request
pub type AddRequest = ServerConfig;

impl ManagerProtocol for AddRequest {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let mut nsplit = buf.splitn(2, |b| *b == b':');

        let cmd = nsplit.next().expect("first element shouldn't be None");
        let cmd = str::from_utf8(cmd)?.trim();
        if cmd != "add" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        match nsplit.next() {
            None => Err(Error::MissingParameter),
            Some(param) => {
                let req = serde_json::from_slice(param)?;
                Ok(req)
            }
        }
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf = b"add: ".to_vec();
        serde_json::to_writer(&mut buf, self)?;
        buf.push(b'\n');
        Ok(buf)
    }
}

/// `add` response
#[derive(Debug, Clone)]
pub struct AddResponse(pub String);

impl ManagerProtocol for AddResponse {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        Ok(AddResponse(str::from_utf8(buf)?.trim().to_owned()))
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut v = self.0.as_bytes().to_owned();
        v.push(b'\n');
        Ok(v)
    }
}

/// `remove` request
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RemoveRequest {
    pub server_port: u16,
}

impl ManagerProtocol for RemoveRequest {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let mut nsplit = buf.splitn(2, |b| *b == b':');

        let cmd = nsplit.next().expect("first element shouldn't be None");
        let cmd = str::from_utf8(cmd)?.trim();
        if cmd != "remove" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        match nsplit.next() {
            None => Err(Error::MissingParameter),
            Some(param) => {
                let req = serde_json::from_slice(param)?;
                Ok(req)
            }
        }
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf = b"remove: ".to_vec();
        serde_json::to_writer(&mut buf, self)?;
        buf.push(b'\n');
        Ok(buf)
    }
}

/// `remove` response
#[derive(Debug, Clone)]
pub struct RemoveResponse(pub String);

impl ManagerProtocol for RemoveResponse {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        Ok(RemoveResponse(str::from_utf8(buf)?.trim().to_owned()))
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut v = self.0.as_bytes().to_owned();
        v.push(b'\n');
        Ok(v)
    }
}

/// `list` request
#[derive(Debug, Clone)]
pub struct ListRequest;

impl ManagerProtocol for ListRequest {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let cmd = str::from_utf8(buf)?;
        if cmd != "list" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        Ok(ListRequest)
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        Ok(b"list\n".to_vec())
    }
}

/// `list` response
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(transparent)]
pub struct ListResponse {
    pub servers: Vec<ServerConfig>,
}

impl ManagerProtocol for ListResponse {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let req = serde_json::from_slice(buf)?;
        Ok(req)
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf = serde_json::to_vec(self)?;
        buf.push(b'\n');
        Ok(buf)
    }
}

/// `list` response part
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ListResponsePart {
    pub data: ListResponse,
    pub parts: u32,
    pub seq: u32,
}

impl ManagerProtocol for ListResponsePart {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let req = if buf.len() > 3 {
            serde_json::from_slice(&buf[3..])?
        } else {
            serde_json::from_slice(buf)?
        };
        Ok(req)
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf: Vec<u8> = vec![];
        let mut seq_byte_str = self.seq.to_string().as_bytes().to_vec();
        let mut parts_byte_str = self.parts.to_string().as_bytes().to_vec();

        buf.append(&mut seq_byte_str);
        buf.push(b'|');
        buf.append(&mut parts_byte_str);
        buf.push(b'|');

        let mut data = serde_json::to_vec(&self.data)?;

        buf.append(&mut data);
        buf.push(b'\n');

        Ok(buf)
    }
}

/// `ping` request
#[derive(Debug, Clone)]
pub struct PingRequest;

impl ManagerProtocol for PingRequest {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let cmd = str::from_utf8(buf)?;
        if cmd != "ping" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        Ok(PingRequest)
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        Ok(b"ping\n".to_vec())
    }
}

/// `ping` response
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(transparent)]
pub struct PingResponse {
    pub stat: HashMap<u16, u64>,
}

impl ManagerProtocol for PingResponse {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let mut nsplit = buf.splitn(2, |b| *b == b':');

        let cmd = nsplit.next().expect("first element shouldn't be None");
        let cmd = str::from_utf8(cmd)?.trim();
        if cmd != "stat" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        match nsplit.next() {
            None => Err(Error::MissingParameter),
            Some(param) => {
                let req = serde_json::from_slice(param)?;
                Ok(req)
            }
        }
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf = b"stat: ".to_vec();
        serde_json::to_writer(&mut buf, self)?;
        buf.push(b'\n');
        Ok(buf)
    }
}


/// `ping` response
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PingResponsePart {
    pub data: PingResponse,
    pub parts: u32,
    pub seq: u32,
}

impl ManagerProtocol for PingResponsePart {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        // todo needed for client?

        let mut nsplit = buf.splitn(2, |b| *b == b':');

        let cmd = nsplit.next().expect("first element shouldn't be None");
        let cmd = str::from_utf8(cmd)?.trim();
        if cmd != "stat" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        match nsplit.next() {
            None => Err(Error::MissingParameter),
            Some(param) => {
                let req = serde_json::from_slice(param)?;
                Ok(req)
            }
        }
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf: Vec<u8> = vec![];
        let mut seq_byte_str = self.seq.to_string().as_bytes().to_vec();
        let mut parts_byte_str = self.parts.to_string().as_bytes().to_vec();

        buf.append(&mut seq_byte_str);
        buf.push(b'|');
        buf.append(&mut parts_byte_str);
        buf.push(b'|');

        serde_json::to_writer(&mut buf, &self.data.stat)?;
        buf.push(b'\n');
        Ok(buf)
    }
}



/// `stat` request
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(transparent)]
pub struct StatRequest {
    pub stat: HashMap<u16, u64>,
}

impl ManagerProtocol for StatRequest {
    fn from_bytes(buf: &[u8]) -> Result<Self, Error> {
        let mut nsplit = buf.splitn(2, |b| *b == b':');

        let cmd = nsplit.next().expect("first element shouldn't be None");
        let cmd = str::from_utf8(cmd)?.trim();
        if cmd != "stat" {
            return Err(Error::UnrecognizedCommand(cmd.to_owned()));
        }

        match nsplit.next() {
            None => Err(Error::MissingParameter),
            Some(param) => {
                let req = serde_json::from_slice(param)?;
                Ok(req)
            }
        }
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buf = b"stat: ".to_vec();
        serde_json::to_writer(&mut buf, self)?;
        buf.push(b'\n');
        Ok(buf)
    }
}

/// Server's error message
#[derive(Debug, Clone)]
pub struct ErrorResponse<E: ToString>(pub E);

impl<E: ToString> ManagerProtocol for ErrorResponse<E> {
    fn from_bytes(_: &[u8]) -> Result<Self, Error> {
        panic!("ErrorResponse is only for sending errors from manager servers");
    }

    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut v = self.0.to_string().into_bytes();
        v.push(b'\n');
        Ok(v)
    }
}

/// Collections of Manager's request
#[derive(Debug, Clone)]
pub enum ManagerRequest {
    Add(AddRequest),
    Remove(RemoveRequest),
    List(ListRequest),
    Ping(PingRequest),
    Stat(StatRequest),
}

impl ManagerRequest {
    /// Command key
    pub fn command(&self) -> &'static str {
        match *self {
            ManagerRequest::Add(..) => "add",
            ManagerRequest::Remove(..) => "remove",
            ManagerRequest::List(..) => "list",
            ManagerRequest::Ping(..) => "ping",
            ManagerRequest::Stat(..) => "stat",
        }
    }
}

impl ManagerProtocol for ManagerRequest {
    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        match *self {
            ManagerRequest::Add(ref req) => req.to_bytes(),
            ManagerRequest::Remove(ref req) => req.to_bytes(),
            ManagerRequest::List(ref req) => req.to_bytes(),
            ManagerRequest::Ping(ref req) => req.to_bytes(),
            ManagerRequest::Stat(ref req) => req.to_bytes(),
        }
    }

    fn from_bytes(buf: &[u8]) -> Result<ManagerRequest, Error> {
        let mut nsplit = buf.splitn(2, |b| *b == b':');

        let cmd = nsplit.next().expect("first element shouldn't be None");
        match str::from_utf8(cmd)?.trim() {
            "add" => match nsplit.next() {
                None => Err(Error::MissingParameter),
                Some(param) => {
                    let req = serde_json::from_slice(param)?;
                    Ok(ManagerRequest::Add(req))
                }
            },
            "remove" => match nsplit.next() {
                None => Err(Error::MissingParameter),
                Some(param) => {
                    let req = serde_json::from_slice(param)?;
                    Ok(ManagerRequest::Remove(req))
                }
            },
            "list" => {
                if nsplit.next().is_some() {
                    return Err(Error::RedundantParameter);
                }
                Ok(ManagerRequest::List(ListRequest))
            }
            "ping" => {
                if nsplit.next().is_some() {
                    return Err(Error::RedundantParameter);
                }
                Ok(ManagerRequest::Ping(PingRequest))
            }
            "stat" => match nsplit.next() {
                None => Err(Error::MissingParameter),
                Some(param) => {
                    let req = serde_json::from_slice(param)?;
                    Ok(ManagerRequest::Stat(req))
                }
            },
            cmd => Err(Error::UnrecognizedCommand(cmd.to_owned())),
        }
    }
}

/// Manager's Error
#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}")]
    JsonError(#[from] serde_json::Error),
    #[error("{0}")]
    FromUtf8Error(#[from] std::str::Utf8Error),
    #[error("missing parameter")]
    MissingParameter,
    #[error("redundant parameter")]
    RedundantParameter,
    #[error("unrecognized command \"{0}\"")]
    UnrecognizedCommand(String),
}

impl From<Error> for io::Error {
    fn from(err: Error) -> io::Error {
        io::Error::new(ErrorKind::Other, err.to_string())
    }
}
