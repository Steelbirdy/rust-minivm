use crate::common::{FromBytes, Read, ToBytes, Write};

impl FromBytes for Bytecode {
    fn from_bytes<R: Read>(reader: &R) -> Option<Self> {
        reader.read_byte().and_then(Self::from_raw)
    }

    #[cfg(feature = "unsafe")]
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
    Missing = 0,
    Exit,
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
    Call,
    Addr,
    DJump,
    DCall,
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
    #[allow(unsafe_code)]
    pub fn from_raw(raw: u8) -> Option<Self> {
        if raw > Self::__MAX as u8 {
            return None;
        }
        // SAFETY:
        // * All discriminants in the range 0..(Self::__MAX as u8) are valid Bytecode variants.
        // * We just checked that `raw` is in this range.
        // * Hence `raw` is a valid Bytecode variant.
        // qed
        Some(unsafe { Self::from_raw_unchecked(raw) })
    }

    #[allow(unsafe_code)]
    unsafe fn from_raw_unchecked(raw: u8) -> Self {
        std::mem::transmute(raw)
    }

    pub fn as_str(self) -> &'static str {
        use Bytecode::*;

        match self {
            Missing => "<missing>",
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
            Call => "call",
            Addr => "addr",
            DJump => "djump",
            DCall => "dcall",
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
