use std::io::{BufReader, Read};
use std::{error::Error, fs::File};

use byteorder::{BigEndian, ReadBytesExt};

// sign extends i to 16 bits using the digit at msb_pos
pub fn sext(i: u16, msb_pos: u8) -> u16 {
    let mut digit = 0;
    let n = (1 << msb_pos & i) >> msb_pos;
    let num_iter = 16 - 1 - msb_pos;
    for j in 0..num_iter {
        let old = n << (j + msb_pos + 1);
        digit += old;
    }
    digit + i
}

// takes bits 0 - 4 of the instruction and sign extends to 16 bits
pub fn imm5(i: u16) -> u16 {
    let result = bit_slice(i, 0, 5);
    sext(result, 4)
}

pub fn offset6(i: u16) -> u16 {
    let result = bit_slice(i, 0, 6);
    sext(result, 5)
}

pub fn offset9(i: u16) -> u16 {
    let result = bit_slice(i, 0, 9);
    sext(result, 8)
}

pub fn offset11(i: u16) -> u16 {
    let result = bit_slice(i, 0, 11);
    sext(result, 10)
}

// extracts the portion [start..end) from the bit slice
// this is returned adjusted to 0
// ie, bit_slice(111111, 4, 5) == 1 and not 1 << 4
pub fn bit_slice(bits: u16, start: u16, end: u16) -> u16 {
    let mut result = 0;
    let bits_copy = bits >> start;
    for idx in 0..(end - start) {
        result += bits_copy & (1 << idx);
    }
    result
}

pub fn bit_at(bits: u16, idx: usize) -> bool {
    (bits >> idx & 1) != 0
}

// NOTE: This might bug when the msb is the 15th bit (range is [0, 15])
pub fn zext(bits: u16, msb: u16) -> u16 {
    // MSB denotes the most significant bit
    // We set the bit to the left of that to 0 and pass this into sext
    let flipped = bits & (0 << msb + 1);
    sext(flipped, msb as u8 + 1)
}

pub fn read_u16<T: Read>(mut reader: BufReader<T>) -> Result<Vec<u16>, Box<dyn Error>> {
    let mut buffer: Vec<u16> = Vec::new();
    loop {
        match reader.read_u16::<BigEndian>() {
            Ok(i) => buffer.push(i),
            Err(e) => {
                if e.kind() == std::io::ErrorKind::UnexpectedEof {
                    return Ok(buffer);
                } else {
                    return Err(Box::new(e));
                }
            }
        }
    }
}
