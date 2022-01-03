use super::util;
use super::vm;
use crate::util::{bit_at, bit_slice, imm5, offset11, offset6, offset9};

pub enum Instruction {
    Add(vm::dr, vm::sr, vm::sr),
    AddImm(vm::dr, vm::sr, vm::imm),
    And(vm::dr, vm::sr, vm::sr),
    AndImm(vm::dr, vm::sr, vm::imm),
    Br(vm::CondReg, vm::sr),
    Jmp(vm::sr),
    Ret,
    Jsr(vm::sr),
    Jsrr(vm::sr),
    Ld(vm::dr, u16),
    Ldi(vm::dr, u16),
    Ldr(vm::dr, vm::sr, u16),
    Lea(vm::dr, u16),
    Not(vm::dr, vm::sr),
    St(vm::sr, u16),
    Sti(vm::sr, u16),
    Str(vm::sr, vm::dr, u16),
    Trap(u16),
}

pub fn parse(raw: u16) -> Instruction {
    // take only the high 4 bits
    let high_bits = (raw & (0xF000)) >> 12;
    let low = raw & 0x0FFFF;
    match high_bits {
        0b0001 => parse_add(low),
        0b0101 => parse_and(low),
        0b0000 => parse_br(low),
        0b1100 => parse_jmp(low), // also parses for RET
        0b0100 => parse_jsr(low),
        0b0010 => parse_ld(low),
        0b1010 => parse_ldi(low),
        0b0110 => parse_ldr(low),
        0b1110 => parse_lea(low),
        0b1001 => parse_not(low),
        0b1000 => parse_rti(low),
        0b0011 => parse_st(low),
        0b1011 => parse_sti(low),
        0b0111 => parse_str(low),
        0b1111 => parse_trap(low),
        x => panic!("received unknown instruction code: {:#b}", x),
    }
}

fn parse_add(low: u16) -> Instruction {
    let is_immediate = bit_at(low, 5);
    let dr = bit_slice(low, 9, 12);
    let sr = bit_slice(low, 6, 9);
    if is_immediate {
        let imm = imm5(low);
        return Instruction::AddImm(dr, sr, imm);
    }
    let sr2 = bit_slice(low, 0, 3);
    return Instruction::Add(dr, sr, sr2);
}

fn parse_and(low: u16) -> Instruction {
    let is_immediate = bit_at(low, 5);
    let dr = bit_slice(low, 9, 12);
    let sr = bit_slice(low, 6, 9);
    if is_immediate {
        let imm = imm5(low);
        return Instruction::AndImm(dr, sr, imm);
    }
    let sr2 = bit_slice(low, 0, 3);
    return Instruction::And(dr, sr, sr2);
}

fn parse_br(low: u16) -> Instruction {
    let n = bit_at(low, 11);
    let z = bit_at(low, 10);
    let p = bit_at(low, 9);
    let cond_reg = vm::CondReg::from_bits(n, z, p);
    let sr = offset9(low);
    Instruction::Br(cond_reg, sr)
}

fn parse_jmp(low: u16) -> Instruction {
    let base_reg = bit_slice(low, 6, 9);
    Instruction::Jmp(base_reg)
}

fn parse_jsr(low: u16) -> Instruction {
    let is_jsr = bit_at(low, 11);
    if is_jsr {
        return Instruction::Jsr(offset11(low));
    }
    let base_reg = bit_slice(low, 6, 9);
    Instruction::Jsrr(base_reg)
}

fn parse_ld(low: u16) -> Instruction {
    let dr = bit_slice(low, 9, 12);
    let offset = offset9(low);
    Instruction::Ld(dr, offset)
}

fn parse_ldi(low: u16) -> Instruction {
    let dr = bit_slice(low, 9, 12);
    let offset = offset9(low);
    Instruction::Ldi(dr, offset)
}

fn parse_ldr(low: u16) -> Instruction {
    let dr = bit_slice(low, 9, 12);
    let base = bit_slice(low, 6, 9);
    let offset = offset6(low);
    Instruction::Ldr(dr, base, offset)
}

fn parse_lea(low: u16) -> Instruction {
    let dr = bit_slice(low, 9, 12);
    let offset = offset9(low);
    Instruction::Lea(dr, offset)
}

fn parse_not(low: u16) -> Instruction {
    let dr = bit_slice(low, 9, 12);
    let sr = bit_slice(low, 6, 9);
    Instruction::Not(dr, sr)
}

// this should not be in here
// this instruction is not implemented/unused for our obj files hence left empty
fn parse_rti(_: u16) -> Instruction {
    todo!()
}

fn parse_st(low: u16) -> Instruction {
    let sr = bit_slice(low, 9, 12);
    let offset = offset9(low);
    Instruction::St(sr, offset)
}

fn parse_sti(low: u16) -> Instruction {
    let sr = bit_slice(low, 9, 12);
    let offset = offset9(low);
    Instruction::Sti(sr, offset)
}

fn parse_str(low: u16) -> Instruction {
    let sr = bit_slice(low, 9, 12);
    let base = bit_slice(low, 6, 9);
    let offset = offset6(low);
    Instruction::Str(sr, base, offset)
}

fn parse_trap(low: u16) -> Instruction {
    let trap_vec = bit_slice(low, 0, 8);
    Instruction::Trap(trap_vec)
}
