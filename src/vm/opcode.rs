use crate::common::AsBytes;

impl AsBytes for Opcode {
    fn to_bytes(self, buf: &mut [u8]) {
        buf[0] = self as u8;
    }
}

// TODO: Use a tagged u8 to represent opcodes. The layout would be:
// bbbbbbbb
//       ^^ argument type flags. 0 is register, 1 is value
// ^^^^^^   discriminant

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    // <op>
    Exit = 0,
    // <op> <end:Addr> <nregs:Reg>
    Func,
    // <op> <addr:Addr> <to:Reg>
    RegA,
    // <op> <from:Reg> <to:Reg>
    RegR,
    // <op> <cond:Reg> <addr_f:Addr> <addr_t:Addr>
    BranchNz,
    // <op> <lhs:Reg> <rhs:Reg> <addr_f:Addr> <addr_t:Addr>
    BranchEqRR,
    // <op> <lhs:Int> <rhs:Reg> <addr_f:Addr> <addr_t:Addr>
    BranchEqIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr_f:Addr> <addr_t:Addr>
    BranchLtRR,
    // <op> <lhs:Int> <rhs:Reg> <addr_f:Addr> <addr_t:Addr>
    BranchLtRI,
    // <op> <lhs:Int> <rhs:Reg> <addr_f:Addr> <addr_t:Addr>
    BranchLtIR,
    // <op> <addr:Addr>
    Jump,
    // <op> <cond:Reg> <addr:Addr>
    JumpEz,
    // <op> <cond:Reg> <addr:Addr>
    JumpNz,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpLtRR,
    // <op> <lhs:Reg> <rhs:Value> <addr:Addr>
    JumpLtRI,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpLtIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpLeRR,
    // <op> <lhs:Reg> <rhs:Value> <addr:Addr>
    JumpLeRI,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpLeIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpEqRR,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpEqIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpNeRR,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpNeIR,
    // <op> <addr:Addr> <nregs:Reg> <to:Reg>
    Call0,
    // <op> <addr:Addr> <args:[Reg; 1]> <nregs:Reg> <to:Reg>
    Call1,
    // <op> <addr:Addr> <args:[Reg; 2]> <nregs:Reg> <to:Reg>
    Call2,
    // <op> <addr:Addr> <args:[Reg; 3]> <nregs:Reg> <to:Reg>
    Call3,
    // <op> <addr:Addr> <args:[Reg; 4]> <nregs:Reg> <to:Reg>
    Call4,
    // <op> <addr:Addr> <args:[Reg; 5]> <nregs:Reg> <to:Reg>
    Call5,
    // <op> <addr:Addr> <args:[Reg; 6]> <nregs:Reg> <to:Reg>
    Call6,
    // <op> <addr:Addr> <args:[Reg; 7]> <nregs:Reg> <to:Reg>
    Call7,
    // <op> <addr:Addr> <args:[Reg; 8]> <nregs:Reg> <to:Reg>
    Call8,
    // <op> <addr:Addr> <nargs:Int> <args:[Reg; nargs]> <nregs:Reg> <to:Reg>
    CallL,
    // <op> <addr:Reg>
    DJump,
    // <op> <addr:Reg> <nregs:Reg> <to:Reg>
    DCall0,
    // <op> <addr:Reg> <args:[Reg; 1]> <nregs:Reg> <to:Reg>
    DCall1,
    // <op> <addr:Reg> <args:[Reg; 2]> <nregs:Reg> <to:Reg>
    DCall2,
    // <op> <addr:Reg> <args:[Reg; 3]> <nregs:Reg> <to:Reg>
    DCall3,
    // <op> <addr:Reg> <args:[Reg; 4]> <nregs:Reg> <to:Reg>
    DCall4,
    // <op> <addr:Reg> <args:[Reg; 5]> <nregs:Reg> <to:Reg>
    DCall5,
    // <op> <addr:Reg> <args:[Reg; 6]> <nregs:Reg> <to:Reg>
    DCall6,
    // <op> <addr:Reg> <args:[Reg; 7]> <nregs:Reg> <to:Reg>
    DCall7,
    // <op> <addr:Reg> <args:[Reg; 8]> <nregs:Reg> <to:Reg>
    DCall8,
    // <op> <addr:Reg> <nargs:Int> <args:[Reg; nargs]> <nregs:Reg> <to:Reg>
    DCallL,
    // <op> <reg:Reg>
    RetR,
    // <op> <val:Value>
    RetV,
    // <op> <val:Value> <to:Reg>
    Value,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    AddRR,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    AddIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    SubRR,
    // <op> <lhs:Reg> <rhs:Value> <to:Reg>
    SubRI,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    SubIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    MulRR,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    MulIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    DivRR,
    // <op> <lhs:Reg> <rhs:Value> <to:Reg>
    DivRI,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    DivIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    ModRR,
    // <op> <lhs:Reg> <rhs:Value> <to:Reg>
    ModRI,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    ModIR,
    // <op> <reg:Reg> <to:Reg>
    Neg,
    // <op> <reg:Reg>
    Incr,
    // <op> <reg:Reg>
    Decr,
    // <op> <len:Reg> <to:Reg>
    ArrR,
    // <op> <len:Int> <to:Reg>
    ArrI,
    // <op> <arr:Reg> <idx:Reg> <to:Reg>
    GetR,
    // <op> <arr:Reg> <idx:Int> <to:Reg>
    GetI,
    // <op> <arr:Reg> <idx:Int> <val:Value>
    SetII,
    // <op> <arr:Reg> <idx:Reg> <val:Reg>
    SetRR,
    // <op> <arr:Reg> <idx:Reg> <val:Value>
    SetRI,
    // <op> <arr:Reg> <idx:Int> <val:Reg>
    SetIR,
    // <op> <arr:Reg> <to:Reg>
    Len,
    // <op> <obj:Reg> <to:Reg>
    Type,
    // <op> <ch:Reg>
    PutcR,
    // <op> <ch:u32>
    PutcI,
    __MAX,
}

impl Opcode {
    pub fn as_str(self) -> &'static str {
        use Opcode::*;

        match self {
            Exit => "exit",
            Func => "func",
            RegA => "reg_a",
            RegR => "reg_r",
            BranchNz => "bb",
            BranchEqRR => "beq_rr",
            BranchEqIR => "beq_ir",
            BranchLtRR => "blt_rr",
            BranchLtRI => "blt_ri",
            BranchLtIR => "blt_ir",
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
            CallL => "callL",
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
            DCallL => "dcallL",
            RetR => "ret_r",
            RetV => "ret_i",
            Value => "value",
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
