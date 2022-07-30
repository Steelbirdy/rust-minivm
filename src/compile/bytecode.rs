use crate::common::{FromBytes, Read, ToBytes, Write};

impl FromBytes for Bytecode {
    fn from_bytes<R: Read>(reader: &R) -> Option<Self> {
        reader.read_byte().and_then(Self::from_raw)
    }

    unsafe fn from_bytes_unchecked<R: Read>(reader: &R) -> Self {
        Self::from_raw_unchecked(reader.read_byte_unchecked())
    }
}

impl ToBytes for Bytecode {
    fn to_bytes<W: Write>(self, writer: &mut W) -> Option<usize> {
        writer.write(self as u8)
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum Bytecode {
    Exit = 0,
    Func,
    Copy,
    Jump,
    JumpEz,
    JumpNz,
    JumpLtRR,
    JumpLtRI,
    JumpLtIR,
    JumpLeRR,
    JumpLeRI,
    JumpLeIR,
    JumpEqRR,
    JumpEqIR,
    JumpNeRR,
    JumpNeIR,
    Call0,
    Call1,
    Call2,
    Call3,
    Call4,
    Call5,
    Call6,
    Call7,
    Call8,
    CallV,
    Addr,
    DJump,
    DCall0,
    DCall1,
    DCall2,
    DCall3,
    DCall4,
    DCall5,
    DCall6,
    DCall7,
    DCall8,
    DCallV,
    RetR,
    RetI,
    Int,
    Str,
    AddRR,
    AddIR,
    SubRR,
    SubRI,
    SubIR,
    MulRR,
    MulIR,
    DivRR,
    DivRI,
    DivIR,
    ModRR,
    ModRI,
    ModIR,
    Neg,
    Incr,
    Decr,
    ArrR,
    ArrI,
    GetR,
    GetI,
    SetII,
    SetRR,
    SetRI,
    SetIR,
    Len,
    Type,
    PutcR,
    PutcI,
    __MAX,
}

impl Bytecode {
    pub const MAX: u8 = Self::__MAX as u8;
    pub const MAX_INLINE_ARGS: usize = 8;

    pub fn from_raw(raw: u8) -> Option<Self> {
        if raw > Self::MAX {
            return None;
        }
        Some(unsafe { Self::from_raw_unchecked(raw) })
    }

    pub unsafe fn from_raw_unchecked(raw: u8) -> Self {
        std::mem::transmute(raw)
    }

    pub fn call(num_args: usize) -> Self {
        const BASE: Bytecode = Bytecode::Call0;

        if num_args <= Self::MAX_INLINE_ARGS {
            let num_args: u8 = num_args.try_into().unwrap();
            unsafe { std::mem::transmute(BASE as u8 + num_args) }
        } else {
            Self::CallV
        }
    }

    pub fn dcall(num_args: usize) -> Self {
        const BASE: Bytecode = Bytecode::DCall0;

        if num_args <= Self::MAX_INLINE_ARGS {
            let num_args: u8 = num_args.try_into().unwrap();
            unsafe { std::mem::transmute(BASE as u8 + num_args) }
        } else {
            Self::DCallV
        }
    }

    pub fn as_str(&self) -> &'static str {
        use Bytecode::*;

        match self {
            Exit => "exit",
            Func => "func",
            Copy => "copy",
            Jump => "jump",
            JumpEz => "jumpez",
            JumpNz => "jumpnz",
            JumpLtRR => "jumplt_rr",
            JumpLtRI => "jumplt_ri",
            JumpLtIR => "jumplt_ir",
            JumpLeRR => "jumple_rr",
            JumpLeRI => "jumple_ri",
            JumpLeIR => "jumple_ir",
            JumpEqRR => "jumpeq_rr",
            JumpEqIR => "jumpeq_ir",
            JumpNeRR => "jumpne_rr",
            JumpNeIR => "jumpne_ir",
            Call0 => "call0",
            Call1 => "call1",
            Call2 => "call2",
            Call3 => "call3",
            Call4 => "call4",
            Call5 => "call5",
            Call6 => "call6",
            Call7 => "call7",
            Call8 => "call8",
            CallV => "callv",
            Addr => "addr",
            DJump => "djump",
            DCall0 => "dcall0",
            DCall1 => "dcall1",
            DCall2 => "dcall2",
            DCall3 => "dcall3",
            DCall4 => "dcall4",
            DCall5 => "dcall5",
            DCall6 => "dcall6",
            DCall7 => "dcall7",
            DCall8 => "dcall8",
            DCallV => "dcallv",
            RetR => "ret_r",
            RetI => "ret_i",
            Int => "int",
            Str => "str",
            AddRR => "add_rr",
            AddIR => "add_ir",
            SubRR => "sub_rr",
            SubRI => "sub_ri",
            SubIR => "sub_ir",
            MulRR => "mul_rr",
            MulIR => "mul_ir",
            DivRR => "div_rr",
            DivRI => "div_ri",
            DivIR => "div_ir",
            ModRR => "mod_rr",
            ModRI => "mod_ri",
            ModIR => "mod_ir",
            Neg => "neg",
            Incr => "incr",
            Decr => "decr",
            ArrR => "arr_r",
            ArrI => "arr_i",
            GetR => "get_r",
            GetI => "get_i",
            SetII => "set_ii",
            SetRR => "set_rr",
            SetRI => "set_ri",
            SetIR => "set_ir",
            Len => "len",
            Type => "type",
            PutcR => "putc_r",
            PutcI => "putc_i",
            __MAX => unimplemented!(),
        }
    }
}
