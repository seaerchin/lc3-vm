use crate::util::overflowing_add;
use core::time;
use std::fmt::{Debug, Formatter};
use std::io::{self, Read, Write};
use std::result;
use std::{fmt, thread};

use super::inst;
use crossterm::Result;
use crossterm::{
    event::{self, read},
    terminal,
};

use super::inst::Instruction;

// These type aliases are exported for clarity when referring to instructions
pub type dr = u16;
pub type sr = u16;
pub type imm = u16;

const MR_KBSR: u16 = 0xFE00;
const MR_KBDR: u16 = 0xFE02;

pub struct CondReg {
    n: bool,
    z: bool,
    p: bool,
}

impl Debug for CondReg {
    fn fmt(&self, _: &mut Formatter<'_>) -> result::Result<(), fmt::Error> {
        Ok(println!("{}, {}, {}", self.n, self.z, self.p))
    }
}

pub struct vm {
    mem: [u16; 1 << 16],
    pc: u16,
    reg: [u16; 1 << 3],
    cond_reg: CondReg,
}

impl CondReg {
    fn new() -> CondReg {
        CondReg {
            n: false,
            z: false,
            p: false,
        }
    }

    pub fn from_bits(n: bool, z: bool, p: bool) -> CondReg {
        CondReg { n, z, p }
    }

    fn set_n(&mut self) {
        self.n = true;
        self.z = false;
        self.p = false
    }

    fn set_z(&mut self) {
        self.z = true;
        self.p = false;
        self.n = false
    }

    fn set_p(&mut self) {
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
            cond_reg: CondReg::new(),
        };
    }

    pub fn run(&mut self) {
        let cur_inst = self.mem_read(self.pc);
        self.handle_inst(inst::parse(cur_inst));
    }

    pub fn from_file(&mut self, insts: Vec<u16>, origin: u16) {
        self.pc = origin;
        for (idx, inst) in insts.iter().enumerate() {
            self.mem[overflowing_add(origin, idx as u16) as usize] = *inst
        }
    }

    pub fn set_cc(&mut self, result: u16) {
        if result > 0 {
            self.cond_reg.set_p();
        } else if result == 0 {
            self.cond_reg.set_z();
        } else {
            self.cond_reg.set_n();
        }
    }

    pub fn mem_write(&mut self) {
        todo!()
    }

    fn handle_keyboard(&mut self) {
        match read_kbd_event() {
            Some(c) => {
                println!("CHAR: {}", c);
                self.mem[MR_KBSR as usize] = 1 << 15;
                self.mem[MR_KBDR as usize] = c as u16;
                println!(
                    "KBSR: {}, KBDR: {:#?}",
                    self.mem[MR_KBSR as usize], self.mem[MR_KBDR as usize]
                );
            }
            None => self.mem[MR_KBSR as usize] = 0,
        }
    }

    pub fn mem_read(&mut self, addr: u16) -> u16 {
        if addr == MR_KBSR {
            println!("I AM HERE");
            self.handle_keyboard();
        }
        self.mem[addr as usize]
    }

    fn handle_inst(&mut self, inst: Instruction) {
        // we increment the pc first before executing any instructions
        let (new, _) = self.pc.overflowing_add(1);
        self.pc = new;
        println!("INSTRUCTION: {:?}", inst);
        thread::sleep(time::Duration::from_secs(1));
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

fn handle_add(vm: &mut vm, dest: u16, source1: u16, source2: u16) {
    vm.reg[dest as usize] =
        (vm.reg[source1 as usize] as u32 + vm.reg[source2 as usize] as u32) as u16
}

fn handle_add_imm(mut vm: &mut vm, dest: u16, src: u16, imm: u16) {
    vm.reg[dest as usize] = (vm.reg[src as usize] as u32 + imm as u32) as u16;
}

fn handle_and(mut vm: &mut vm, dest: u16, src1: u16, src2: u16) {
    vm.reg[dest as usize] = vm.reg[src1 as usize] & vm.reg[src2 as usize];
}

fn handle_and_imm(mut vm: &mut vm, dest: u16, src: u16, imm: u16) {
    vm.reg[dest as usize] = vm.reg[src as usize] & imm;
}

fn handle_br(mut vm: &mut vm, CondReg { n, z, p }: CondReg, offset: u16) {
    if n && vm.cond_reg.n || z && vm.cond_reg.z || p && vm.cond_reg.p {
        vm.pc = overflowing_add(vm.pc, offset);
    }
}

fn handle_jmp(mut vm: &mut vm, reg_idx: u16) {
    vm.pc = vm.reg[reg_idx as usize]
}

fn handle_ret(mut vm: &mut vm) {
    vm.pc = vm.reg[7];
}

fn handle_jsr(mut vm: &mut vm, offset: u16) {
    vm.reg[7] = vm.pc;
    vm.pc = overflowing_add(vm.pc, offset);
}

fn handle_jsrr(mut vm: &mut vm, base: u16) {
    vm.reg[7] = vm.pc;
    vm.pc = vm.reg[base as usize];
}

fn handle_ld(vm: &mut vm, dr: u16, offset: u16) {
    let addr = overflowing_add(vm.pc, offset);
    let value = vm.mem_read(addr);
    vm.reg[dr as usize] = value;
    vm.set_cc(value)
}

fn handle_ldi(mut vm: &mut vm, dr: u16, offset: u16) {
    let base = overflowing_add(vm.pc, offset);
    let initial_addr = vm.mem_read(base);
    let value = vm.mem_read(initial_addr);
    vm.reg[dr as usize] = value;
    vm.set_cc(value)
}

fn handle_ldr(mut vm: &mut vm, dr: u16, base: u16, offset: u16) {
    let base_addr = vm.reg[base as usize];
    let value = vm.mem_read(overflowing_add(base_addr, offset));
    vm.reg[dr as usize] = value;
    vm.set_cc(value);
}

fn handle_lea(mut vm: &mut vm, dr: u16, offset: u16) {
    let value = overflowing_add(vm.pc, offset);
    vm.reg[dr as usize] = value;
    vm.set_cc(value)
}

fn handle_not(mut vm: &mut vm, dr: u16, sr: u16) {
    vm.reg[dr as usize] = !vm.reg[sr as usize];
}

fn handle_st(mut vm: &mut vm, sr: u16, offset: u16) {
    let base_addr: usize = overflowing_add(vm.pc, offset).into();
    vm.mem[base_addr] = vm.reg[sr as usize];
}

fn handle_sti(mut vm: &mut vm, sr: u16, offset: u16) {
    let base_addr = overflowing_add(vm.pc, offset);
    let base_contents = vm.mem_read(base_addr);
    vm.mem[base_contents as usize] = vm.reg[sr as usize];
}

fn handle_str(mut vm: &mut vm, sr: u16, base_r: u16, offset: u16) {
    let base: u32 = vm.reg[base_r as usize] as u32 + offset as u32;
    vm.mem[base as u16 as usize] = vm.reg[sr as usize];
}

fn handle_trap(mut vm: &mut vm, trap: u16) {
    vm.reg[7] = vm.pc;
    match trap {
        0x20 => getc(vm),
        0x21 => out(vm),
        0x22 => puts(vm),
        0x23 => inn(vm),
        0x24 => putsp(vm),
        0x25 => halt(),
        _ => panic!("this trap instruction doesn't exist"),
    }
}

// we enable raw mode to prevent line buffering and read from stdin immediately
// upon exit, we disable raw mode
fn getc(vm: &mut vm) {
    match terminal::enable_raw_mode() {
        Ok(_) => {
            if let Some(char) = read_kbd_event() {
                let mut buffer = [0; 1];
                let encoded_char = char.encode_utf8(&mut buffer);
                vm.reg[0] = encoded_char.chars().nth(0).unwrap() as u16;
            }
            terminal::disable_raw_mode().unwrap();
        }
        // abort early; don't retry as the error might be something on the user's part
        Err(e) => println!("Unable to read character from input due to error: {}", e),
    }
}

// recursively read until we get a keyboard event or we error
fn read_kbd_event() -> Option<char> {
    terminal::enable_raw_mode().unwrap();
    let c = std::io::stdin()
        .bytes()
        .next()
        .and_then(|result| result.ok())
        .map(|byte| byte as char);
    terminal::disable_raw_mode().unwrap();
    c
}

fn out(vm: &vm) {
    let base = vm.reg[0];
    let new = (base & 0xFF) as u8 as char;
    println!("{}", new)
}

fn puts(vm: &mut vm) {
    let mut addr = vm.reg[0];
    while vm.mem_read(addr) != 0x0000 {
        print!("{}", vm.mem_read(addr) as u8 as char);
        addr = overflowing_add(addr, 1);
    }
    io::stdout().flush().expect("failed to flush");
}

fn inn(mut vm: &mut vm) {
    println!("please enter a single character");
    match read_kbd_event() {
        Some(c) => {
            println!("{}", c);
            vm.reg[0] = c as u16;
        }
        None => panic!("we dun goofed"),
    }
}

fn putsp(vm: &mut vm) {
    let mut addr = vm.reg[0];
    while vm.mem_read(addr) != 0x0000 {
        let lower = (addr & 0x00FF) as u8;
        let upper = (addr & 0xFF00) as u8;
        print!("{}", lower as char);
        // if odd number of characters, the character at bits [15, 8] will be 0x00
        if upper != 0x00 {
            print!("{}", upper as char);
        }
        addr = overflowing_add(addr, 1);
    }
    io::stdout().flush().expect("failed to flush");
}

fn halt() {
    println!("program halted! exiting now...");
    std::process::exit(0)
}
