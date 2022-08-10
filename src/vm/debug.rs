use crate::{
    common::{self, BytecodeReader},
    vm::Opcode,
};
use std::fmt::{self, Write};

enum Arg {
    Addr(common::Addr),
    Reg(common::Reg),
    Int(common::Int),
    Value(crate::vm::Value),
    Char(u32),
}

impl fmt::Debug for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Addr(addr) => write!(f, "@{addr}"),
            Self::Reg(reg) => write!(f, "r{reg}"),
            Self::Int(int) => write!(f, "{int}"),
            Self::Value(val) => write!(f, "{:?}", val),
            Self::Char(ch) => write!(f, "{:?}", char::from_u32(*ch).unwrap()),
        }
    }
}

pub fn disassemble_instruction(code: &mut BytecodeReader, buf: &mut String) {
    use Opcode::*;

    macro_rules! args {
        ($($ty:ident),* $(,)?) => {{
            let args: Vec<Arg> = vec![$(
                Arg::$ty(code.take()),
            )*];
            writeln!(buf, " {args:?}").unwrap();
        }};
    }

    let offset = code.offset();
    let opcode = code.take::<Opcode>();
    let as_str = opcode.as_str();
    let padding = " ".repeat(9 - as_str.len());
    write!(buf, "{offset:>04} | {as_str}{padding}").unwrap();

    match opcode {
        Exit => args!(),
        Func => args!(Addr, Reg),
        RegA => args!(Addr, Reg),
        RegR => args!(Reg, Reg),
        BranchNz => args!(Reg, Addr, Addr),
        BranchEqRR | BranchLtRR => args!(Reg, Reg, Addr, Addr),
        BranchLtRI => args!(Reg, Value, Addr, Addr),
        BranchEqIR | BranchLtIR => args!(Value, Reg, Addr, Addr),
        Jump => args!(Addr),
        JumpEz | JumpNz => args!(Reg, Addr),
        JumpLtRR | JumpLeRR | JumpEqRR | JumpNeRR => args!(Reg, Reg, Addr),
        JumpLtRI | JumpLeRI => args!(Reg, Value, Addr),
        JumpLtIR | JumpLeIR | JumpEqIR | JumpNeIR => args!(Value, Reg, Addr),
        Call0 => args!(Addr, Reg, Reg),
        Call1 => args!(Addr, Reg, Reg, Reg),
        Call2 => args!(Addr, Reg, Reg, Reg, Reg),
        Call3 => args!(Addr, Reg, Reg, Reg, Reg, Reg),
        Call4 => args!(Addr, Reg, Reg, Reg, Reg, Reg, Reg),
        Call5 => args!(Addr, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        Call6 => args!(Addr, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        Call7 => args!(Addr, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        Call8 => args!(Addr, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        CallL => {
            let addr = Arg::Addr(code.take());
            let num_args = code.take::<u32>();
            write!(buf, " [{addr:?}, {num_args}").unwrap();
            for _ in 0..num_args + 2 {
                write!(buf, ", {:?}", Arg::Reg(code.take())).unwrap();
            }
            // writeln!(buf, ", {:?}]", Arg::Reg(code.take().unwrap())).unwrap();
        }
        DJump => args!(Reg, Reg),
        DCall0 => args!(Reg, Reg, Reg),
        DCall1 => args!(Reg, Reg, Reg, Reg),
        DCall2 => args!(Reg, Reg, Reg, Reg, Reg),
        DCall3 => args!(Reg, Reg, Reg, Reg, Reg, Reg),
        DCall4 => args!(Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        DCall5 => args!(Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        DCall6 => args!(Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        DCall7 => args!(Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        DCall8 => args!(Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg, Reg),
        DCallL => {
            let addr = Arg::Reg(code.take());
            let num_args = code.take::<u32>();
            write!(buf, " [{addr:?}, {num_args}").unwrap();
            for _ in 0..num_args + 2 {
                write!(buf, ", {:?}", Arg::Reg(code.take())).unwrap();
            }
            // writeln!(buf, ", {:?}]", Arg::Reg(code.take().unwrap())).unwrap();
        }
        RetR => args!(Reg),
        RetV => args!(Value),
        Value => args!(Value, Reg),
        AddRR | SubRR | MulRR | DivRR | ModRR => args!(Reg, Reg, Reg),
        AddIR | SubIR | MulIR | DivIR | ModIR => args!(Value, Reg, Reg),
        SubRI | DivRI | ModRI => args!(Reg, Value, Reg),
        Neg => args!(Reg, Reg),
        Incr => args!(Reg),
        Decr => args!(Reg),
        ArrR => args!(Reg, Reg),
        ArrI => args!(Int, Reg),
        GetR => args!(Reg, Reg, Reg),
        GetI => args!(Reg, Int, Reg),
        SetII => args!(Reg, Int, Value),
        SetRR => args!(Reg, Reg, Reg),
        SetRI => args!(Reg, Reg, Value),
        SetIR => args!(Reg, Int, Reg),
        Len => args!(Reg, Reg),
        Type => args!(Reg, Reg),
        PutcR => args!(Reg),
        PutcI => args!(Char),
        __MAX => unimplemented!(),
    }
}

#[must_use]
pub fn disassemble(mut code: BytecodeReader) -> String {
    let mut buf = String::new();

    while !code.is_at_end() {
        disassemble_instruction(&mut code, &mut buf);
    }

    buf.pop();
    buf
}
