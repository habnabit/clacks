use byteorder::{ByteOrder, LittleEndian};
use bytes::{BufMut, BytesMut};
use std::{io, mem};
use tokio_io::codec::{Decoder, Encoder};

enum LengthState {
    ReadingLength,
    ReadingBytes { remaining: usize, buf: Vec<u8> },
}

pub struct TelegramCodec {
    sent_leading_byte: bool,
    state: LengthState,
}

impl TelegramCodec {
    pub fn new() -> TelegramCodec {
        TelegramCodec {
            sent_leading_byte: false,
            state: LengthState::ReadingLength,
        }
    }
}

impl Decoder for TelegramCodec {
    type Item = Vec<u8>;
    type Error = io::Error;

    fn decode(&mut self, input: &mut BytesMut) -> io::Result<Option<Vec<u8>>> {
        use self::LengthState::*;
        loop {
            let available = input.len();
            match &mut self.state {
                &mut ReadingLength if available < 4 => return Ok(None),
                state @ &mut ReadingLength => {
                    let length = if input[0] == 0x7f {
                        ((LittleEndian::read_u32(&input.split_to(4)) & !0xff) >> 6) as usize
                    } else {
                        (input.split_to(1)[0] as usize) << 2
                    };
                    *state = ReadingBytes {
                        remaining: length,
                        buf: Vec::with_capacity(length),
                    };
                },
                &mut ReadingBytes { ref mut remaining, ref mut buf } if available < *remaining => {
                    buf.extend(input.split_to(available));
                    *remaining -= available;
                    return Ok(None);
                },
                state => {
                    if let ReadingBytes { remaining, mut buf } = mem::replace(state, ReadingLength) {
                        buf.extend(input.split_to(remaining));
                        return Ok(Some(buf));
                    } else {
                        unreachable!()
                    }
                },
            }
        }
    }
}

impl Encoder for TelegramCodec {
    type Item = Vec<u8>;
    type Error = io::Error;

    fn encode(&mut self, bytes: Vec<u8>, buf: &mut BytesMut) -> io::Result<()> {
        assert!(bytes.len() % 4 == 0);
        buf.reserve(bytes.len() + 5);
        if !self.sent_leading_byte {
            buf.put_u8(0xef);
            self.sent_leading_byte = true;
        }
        let compact_size = (bytes.len() as u32) / 4;
        if compact_size < 0x7f {
            buf.put_u8(compact_size as u8);
        } else {
            let u32_size = compact_size << 8 | 0x7f;
            buf.put_u32_le(u32_size);
        }
        buf.put(bytes);
        Ok(())
    }
}
