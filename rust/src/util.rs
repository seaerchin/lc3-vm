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
    (bits >> idx & 1) == 1
}
