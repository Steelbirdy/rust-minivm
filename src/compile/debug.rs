use crate::{
    common::{self, ByteReader, Read},
    compile::bytecode::Bytecode,
};
use std::fmt::{self, Write};

enum Arg {
    Addr(common::Addr),
    Reg(common::Reg),
    Int(common::Int),
    Char(common::Int),
}

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
impl fmt::Debug for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Addr(addr) => write!(f, "@{addr}"),
            Self::Reg(reg) => write!(f, "r{reg}"),
            Self::Int(int) => write!(f, "{int}"),
            Self::Char(ch) => write!(f, "{:?}", char::from_u32(*ch as _).unwrap()),
        }
    }
}

pub fn disassemble(code: ByteReader) -> String {
    use Bytecode::*;
    let mut buf = String::new();

    macro_rules! args {
        ($($ty:ident),* $(,)?) => {{
            let args: Vec<Arg> = vec![$(
                Arg::$ty(code.take().unwrap()),
            )*];
            writeln!(buf, " {args:?}").unwrap();
        }};
    }

    while !code.is_at_end() {
        let offset = code.offset();
        let opcode = code.take::<Bytecode>().unwrap();
        let as_str = opcode.as_str();
        let padding = " ".repeat(9 - as_str.len());
        write!(buf, "{offset:>04} | {as_str}{padding}").unwrap();

        match opcode {
            Missing => args!(),
            Exit => args!(),
            Func => args!(Addr, Reg),
            Copy => args!(Reg, Reg),
            Jump => args!(Addr),
            JumpEz | JumpNz => args!(Reg, Addr),
            JumpLtRR | JumpLeRR | JumpEqRR | JumpNeRR => args!(Reg, Reg, Addr),
            JumpLtRI | JumpLeRI => args!(Reg, Int, Addr),
            JumpLtIR | JumpLeIR | JumpEqIR | JumpNeIR => args!(Int, Reg, Addr),
            Call => {
                let addr = Arg::Addr(code.take().unwrap());
                let num_args = code.take::<common::Int>().unwrap();
                write!(buf, " [{addr:?}, {num_args}").unwrap();
                for _ in 0..num_args {
                    write!(buf, ", {:?}", Arg::Reg(code.take().unwrap())).unwrap();
                }
                writeln!(buf, ", {:?}]", Arg::Reg(code.take().unwrap())).unwrap();
            }
            Addr => args!(Addr, Reg),
            DJump => args!(Reg),
            DCall => {
                let addr = Arg::Reg(code.take().unwrap());
                let num_args = code.take::<common::Int>().unwrap();
                write!(buf, " [{addr:?}, {num_args}").unwrap();
                for _ in 0..num_args {
                    write!(buf, ", {:?}", Arg::Reg(code.take().unwrap())).unwrap();
                }
                writeln!(buf, ", {:?}]", Arg::Reg(code.take().unwrap())).unwrap();
            }
            RetR => args!(Reg),
            RetI => args!(Int),
            Int => args!(Int, Reg),
            Str => {
                let len = code.take::<common::Int>().unwrap();
                write!(buf, " [{len}").unwrap();
                for _ in 0..len {
                    write!(buf, ", {:?}", Arg::Char(code.take().unwrap())).unwrap();
                }
                writeln!(buf, ", {:?}]", Arg::Reg(code.take().unwrap())).unwrap();
            }
            AddRR | SubRR | MulRR | DivRR | ModRR => args!(Reg, Reg, Reg),
            AddIR | SubIR | MulIR | DivIR | ModIR => args!(Int, Reg, Reg),
            SubRI | DivRI | ModRI => args!(Reg, Int, Reg),
            Neg => args!(Reg, Reg),
            Incr => args!(Reg),
            Decr => args!(Reg),
            ArrR => args!(Reg, Reg),
            ArrI => args!(Int, Reg),
            GetR => args!(Reg, Reg, Reg),
            GetI => args!(Reg, Int, Reg),
            SetII => args!(Reg, Int, Int),
            SetRR => args!(Reg, Reg, Reg),
            SetRI => args!(Reg, Reg, Int),
            SetIR => args!(Reg, Int, Reg),
            Len => args!(Reg, Reg),
            Type => args!(Reg, Reg),
            PutcR => args!(Reg),
            PutcI => args!(Int),
            __MAX => unimplemented!(),
        }
    }

    code.reset();
    buf.pop();
    buf
}
