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
    // match on [12 .. 15]
    todo!("add impl")
}
