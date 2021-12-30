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
    mem: [i32; 1 << 16],
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

    pub fn set_cc(mut self, result: i32) {
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
            Instruction::Jsr(_) => todo!(),
            Instruction::Jsrr(_) => todo!(),
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
