use std::error::Error;

use crate::util::offset9;
use crossterm::Result;
use crossterm::{
    event::{self, read, KeyEvent},
    terminal,
};

use super::{inst::Instruction, util};

// These type aliases are exported for clarity when referring to instructions
pub type dr = u16;
pub type sr = u16;
pub type imm = u16;

pub struct cond_reg {
    n: bool,
    z: bool,
    p: bool,
}

struct vm {
    mem: [u16; 1 << 16],
    pc: u16,
    reg: [u16; 1 << 3],
    cond_reg: cond_reg,
}

impl cond_reg {
    fn new() -> cond_reg {
        cond_reg {
            n: false,
            z: false,
            p: false,
        }
    }

    fn set_n(mut self) {
        self.n = true;
        self.z = false;
        self.p = false
    }

    fn set_z(mut self) {
        self.z = true;
        self.p = false;
        self.n = false
    }

    fn set_p(mut self) {
        self.p = true;
        self.z = false;
        self.n = false
    }
}

impl vm {
    pub fn new() -> vm {
        return vm {
            mem: [0; 1 << 16],
            pc: 0,
            reg: [0; 1 << 3],
            cond_reg: cond_reg::new(),
        };
    }

    pub fn set_cc(self, result: u16) {
        if result > 0 {
            self.cond_reg.set_p();
        } else if result == 0 {
            self.cond_reg.set_z();
        } else {
            self.cond_reg.set_n();
        }
    }

    pub fn handle_inst(mut self, inst: Instruction) {
        // we increment the pc first before executing any instructions
        self.pc += 1;
        match inst {
            Instruction::Add(dest, src1, src2) => handle_add(self, dest, src1, src2),
            Instruction::AddImm(dest, src, imm) => handle_add_imm(self, dest, src, imm),
            Instruction::And(dest, src1, src2) => handle_and(self, dest, src1, src2),
            Instruction::AndImm(dest, src, imm) => handle_and_imm(self, dest, src, imm),
            Instruction::Br(cond, offset) => handle_br(self, cond, offset),
            Instruction::Jmp(reg_idx) => handle_jmp(self, reg_idx),
            Instruction::Ret => handle_ret(self),
            Instruction::Jsr(offset) => handle_jsr(self, offset),
            Instruction::Jsrr(base) => handle_jsrr(self, base),
            Instruction::Ld(dr, offset) => handle_ld(self, dr, offset),
            Instruction::Ldi(dr, offset) => handle_ldi(self, dr, offset),
            Instruction::Ldr(dr, base, offset) => handle_ldr(self, dr, base, offset),
            Instruction::Lea(dr, offset) => handle_lea(self, dr, offset),
            Instruction::Not(dr, sr) => handle_not(self, dr, sr),
            Instruction::St(sr, offset) => handle_st(self, sr, offset),
            Instruction::Sti(sr, offset) => handle_sti(self, sr, offset),
            Instruction::Str(sr, base_r, offset) => handle_str(self, sr, base_r, offset),
            Instruction::Trap(trap) => handle_trap(self, trap),
        }
    }
}

fn handle_add(mut vm: vm, dest: u16, source1: u16, source2: u16) {
    vm.reg[dest as usize] = vm.reg[source1 as usize] + vm.reg[source2 as usize]
}

fn handle_add_imm(mut vm: vm, dest: u16, src: u16, imm: u16) {
    vm.reg[dest as usize] = vm.reg[src as usize] + util::imm5(imm);
}

fn handle_and(mut vm: vm, dest: u16, src1: u16, src2: u16) {
    vm.reg[dest as usize] = vm.reg[src1 as usize] & vm.reg[src2 as usize];
}

fn handle_and_imm(mut vm: vm, dest: u16, src: u16, imm: u16) {
    vm.reg[dest as usize] = vm.reg[src as usize] & util::imm5(imm);
}

fn handle_br(mut vm: vm, cond_reg { n, z, p }: cond_reg, offset: u16) {
    if n && vm.cond_reg.n || z && vm.cond_reg.z || p && vm.cond_reg.p {
        vm.pc += util::offset9(offset)
    }
}

fn handle_jmp(mut vm: vm, reg_idx: u16) {
    vm.pc = vm.reg[reg_idx as usize]
}

fn handle_ret(mut vm: vm) {
    vm.pc = vm.reg[7];
}

fn handle_jsr(mut vm: vm, offset: u16) {
    vm.reg[7] = vm.pc;
    vm.pc = vm.pc + util::offset11(offset);
}

fn handle_jsrr(mut vm: vm, base: u16) {
    vm.reg[7] = vm.pc;
    vm.pc = vm.reg[base as usize];
}

fn handle_ld(mut vm: vm, dr: u16, offset: u16) {
    let addr = vm.pc + offset9(offset);
    let value = vm.mem[addr as usize];
    vm.reg[dr as usize] = value;
    vm.set_cc(value)
}

fn handle_ldi(mut vm: vm, dr: u16, offset: u16) {
    let base: usize = (vm.pc + offset9(offset)).into();
    let initial_addr: usize = vm.mem[base].into();
    let value = vm.mem[initial_addr];
    vm.reg[dr as usize] = value;
    vm.set_cc(value)
}

fn handle_ldr(mut vm: vm, dr: u16, base: u16, offset: u16) {
    let base_addr = vm.reg[base as usize];
    let value = vm.mem[(base_addr + util::offset6(offset)) as usize];
    vm.reg[dr as usize] = value;
    vm.set_cc(value);
}

fn handle_lea(mut vm: vm, dr: u16, offset: u16) {
    let value = vm.pc + offset9(offset);
    vm.reg[dr as usize] = value;
    vm.set_cc(value)
}

fn handle_not(mut vm: vm, dr: u16, sr: u16) {
    vm.reg[dr as usize] = !vm.reg[sr as usize];
}

fn handle_st(mut vm: vm, sr: u16, offset: u16) {
    let base_addr: usize = (vm.pc + offset9(offset)).into();
    vm.mem[base_addr] = vm.reg[sr as usize];
}

fn handle_sti(mut vm: vm, sr: u16, offset: u16) {
    let base_addr: usize = (vm.pc + offset9(offset)).into();
    let base_contents: usize = vm.mem[base_addr].into();
    vm.mem[base_contents] = vm.reg[sr as usize];
}

fn handle_str(mut vm: vm, sr: u16, base_r: u16, offset: u16) {
    let base: usize = (vm.reg[base_r as usize] + util::offset6(offset)).into();
    vm.mem[base] = vm.reg[sr as usize];
}

fn handle_trap(mut vm: vm, trap: u16) {
    vm.reg[7] = vm.pc;
    match trap {
        0x20 => getc(vm),
        0x21 => out(),
        0x22 => puts(),
        0x23 => inn(),
        0x24 => putsp(),
        0x25 => halt(),
        _ => panic!("this trap instruction doesn't exist"),
    }
}

// we enable raw mode to prevent line buffering and read from stdin immediately
// upon exit, we disable raw mode
fn getc(mut vm: vm) {
    match terminal::enable_raw_mode() {
        Ok(_) => {
            if let Ok(char) = read_kbd_event() {
                let mut buffer = [0; 1];
                let encoded_char = char.encode_utf8(&mut buffer);
                vm.reg[0] = encoded_char[0]
            }
        }
        // abort early; don't retry as the error might be something on the user's part
        Err(e) => println!("Unable to read character from input due to error: {}", e),
    }
}

// recursively read until we get a keyboard event or we error
fn read_kbd_event() -> Result<char> {
    match read() {
        Ok(e) => match e {
            event::Event::Key(k) => match k.code {
                event::KeyCode::Char(c) => Ok(c),
                _ => read_kbd_event(),
            },
            event::Event::Mouse(_) => read_kbd_event(),
            event::Event::Resize(_, _) => read_kbd_event(),
        },
        Err(e) => Err(e),
    }
}

fn out(mut vm: vm) {
    let base = vm.reg[0];
    let new = (base & 0xFF) as u8 as char;
    println!("{}", new)
}
