use crate::common::{Int, Key, List, Reg};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Val {
    Int(Int),
    Reg(Reg),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Target {
    Lbl(Key),
    Reg(Reg),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Key,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Exit,
    Lbl {
        name: Key,
    },
    Copy {
        to: Reg,
        from: Reg,
    },
    Jump {
        target: Target,
    },
    JumpEz {
        val: Val,
        target: Key,
    },
    JumpNz {
        val: Val,
        target: Key,
    },
    JumpLt {
        lhs: Val,
        rhs: Val,
        target: Key,
    },
    JumpLe {
        lhs: Val,
        rhs: Val,
        target: Key,
    },
    JumpGt {
        lhs: Val,
        rhs: Val,
        target: Key,
    },
    JumpGe {
        lhs: Val,
        rhs: Val,
        target: Key,
    },
    JumpEq {
        lhs: Val,
        rhs: Val,
        target: Key,
    },
    JumpNe {
        lhs: Val,
        rhs: Val,
        target: Key,
    },
    Call {
        to: Reg,
        target: Target,
        args: List<Reg>,
    },
    Addr {
        to: Reg,
        of: Key,
    },
    Ret {
        val: Val,
    },
    Int {
        to: Reg,
        int: Int,
    },
    Str {
        to: Reg,
        str: Key,
    },
    Add {
        to: Reg,
        lhs: Val,
        rhs: Val,
    },
    Sub {
        to: Reg,
        lhs: Val,
        rhs: Val,
    },
    Mul {
        to: Reg,
        lhs: Val,
        rhs: Val,
    },
    Div {
        to: Reg,
        lhs: Val,
        rhs: Val,
    },
    Mod {
        to: Reg,
        lhs: Val,
        rhs: Val,
    },
    Neg {
        to: Reg,
        rhs: Val,
    },
    Incr {
        reg: Reg,
    },
    Decr {
        reg: Reg,
    },
    Arr {
        to: Reg,
        len: Val,
    },
    Get {
        to: Reg,
        arr: Reg,
        idx: Val,
    },
    Set {
        arr: Reg,
        idx: Val,
        val: Val,
    },
    Len {
        to: Reg,
        arr: Reg,
    },
    Type {
        to: Reg,
        obj: Reg,
    },
    Putc {
        val: Val,
    },
}
