use super::vm;

pub enum Instruction {
    Add(vm::dr, vm::sr, vm::sr),
    AddImm(vm::dr, vm::sr, vm::imm),
    And(vm::dr, vm::sr, vm::sr),
    AndImm(vm::dr, vm::sr, vm::imm),
    Br(vm::cond_reg, vm::sr),
    Jmp(vm::sr),
    Ret,
    Jsr(vm::sr),
    Jsrr(vm::sr),
}

pub fn parse(raw: u16) -> Instruction {
    // match on [12 .. 15]
    todo!("add impl")
}
