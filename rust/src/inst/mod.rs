use super::vm;

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
    let low_bits = raw & 0x0FFFF;
    match high_bits {
        0b0001 => parse_add(low_bits),
        0b0101 => parse_and(low_bits),
        0b0000 => parse_br(low_bits),
        0b1100 => parse_jmp(low_bits), // also parses for RET
        0b0100 => parse_jsr(low_bits),
        0b0010 => parse_ld(low_bits),
        0b1010 => parse_ldi(low_bits),
        0b0110 => parse_ldr(low_bits),
        0b1110 => parse_lea(low_bits),
        0b1001 => parse_not(low_bits),
        0b1000 => parse_rti(low_bits),
        0b0011 => parse_st(low_bits),
        0b1011 => parse_sti(low_bits),
        0b0111 => parse_str(low_bits),
        0b1111 => parse_trap(low_bits),
        x => panic!("received unknown instruction code: {:#b}", x),
    }
}

fn parse_add(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_and(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_br(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_jmp(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_jsr(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_ld(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_ldi(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_ldr(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_lea(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_not(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_rti(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_st(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_sti(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_str(low_bits: u16) -> Instruction {
    todo!()
}

fn parse_trap(low_bits: u16) -> Instruction {
    todo!()
}
