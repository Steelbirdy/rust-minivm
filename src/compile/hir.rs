use crate::common::{Addr, Int, Key, List, list, Reg};

pub struct Function {
    name: Key,
    instructions: List<Instruction>,
}

#[derive(Default)]
pub struct RegisterUse {
    initialized: List<Reg>,
    used: List<Reg>,
}

impl RegisterUse {
    pub fn new<const N: usize, const M: usize>(init: [Reg; N], used: [Reg; M]) -> Self {
        Self {
            initialized: List::from_iter(init),
            used: List::from_iter(used),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Exit,
    Label { name: Key },
    Reg { to: Reg, from: Reg },
    BranchNz { cond: Reg, lbl_false: Key, lbl_true: Key },
    BranchEqRR { lhs: Reg, rhs: Reg, lbl_false: Key, lbl_true: Key },
    BranchEqIR { lhs: Int, rhs: Reg, lbl_false: Key, lbl_true: Key },
    BranchLtRR { lhs: Reg, rhs: Reg, lbl_false: Key, lbl_true: Key },
    BranchLtRI { lhs: Reg, rhs: Int, lbl_false: Key, lbl_true: Key },
    BranchLtIR { lhs: Int, rhs: Reg, lbl_false: Key, lbl_true: Key },
    Jump { lbl: Key },
    JumpEz { cond: Reg, lbl: Key },
    JumpNz { cond: Reg, lbl: Key },
    JumpLtRR { lhs: Reg, rhs: Reg, lbl: Key },
    JumpLtRI { lhs: Reg, rhs: Int, lbl: Key },
    JumpLtIR { lhs: Int, rhs: Reg, lbl: Key },
    JumpLeRR { lhs: Reg, rhs: Reg, lbl: Key },
    JumpLeRI { lhs: Reg, rhs: Int, lbl: Key },
    JumpLeIR { lhs: Int, rhs: Reg, lbl: Key },
    JumpEqRR { lhs: Reg, rhs: Reg, lbl: Key },
    JumpEqIR { lhs: Int, rhs: Reg, lbl: Key },
    JumpNeRR { lhs: Reg, rhs: Reg, lbl: Key },
    JumpNeIR { lhs: Int, rhs: Reg, lbl: Key },
    Call { to: Reg, func: Key, args: List<Reg> },
    Addr { to: Reg, lbl: Key },
    DJump { lbl: Reg },
    DCall { to: Reg, func: Reg, args: List<Reg> },
    RetR { reg: Reg },
    RetI { int: Int },
    Int { to: Reg, int: Int },
    Str { to: Reg, str: Key },
    AddRR { to: Reg, lhs: Reg, rhs: Reg },
    AddIR { to: Reg, lhs: Int, rhs: Reg },
    SubRR { to: Reg, lhs: Reg, rhs: Reg },
    SubRI { to: Reg, lhs: Reg, rhs: Int },
    SubIR { to: Reg, lhs: Int, rhs: Reg },
    MulRR { to: Reg, lhs: Reg, rhs: Reg },
    MulIR { to: Reg, lhs: Int, rhs: Reg },
    DivRR { to: Reg, lhs: Reg, rhs: Reg },
    DivRI { to: Reg, lhs: Reg, rhs: Int },
    DivIR { to: Reg, lhs: Int, rhs: Reg },
    ModRR { to: Reg, lhs: Reg, rhs: Reg },
    ModRI { to: Reg, lhs: Reg, rhs: Int },
    ModIR { to: Reg, lhs: Int, rhs: Reg },
    Neg { to: Reg, rhs: Reg },
    Incr { reg: Reg },
    Decr { reg: Reg },
    ArrR { to: Reg, len: Reg },
    ArrI { to: Reg, len: Int },
    GetR { to: Reg, arr: Reg, idx: Reg },
    GetI { to: Reg, arr: Reg, idx: Int },
    SetII { arr: Reg, idx: Int, val: Int },
    SetRR { arr: Reg, idx: Reg, val: Reg },
    SetRI { arr: Reg, idx: Reg, val: Int },
    SetIR { arr: Reg, idx: Int, val: Reg },
    Len { to: Reg, arr: Reg },
    Type { to: Reg, obj: Reg },
    PutcR { char: Reg },
    PutcI { char: Int },
}

impl Instruction {
    pub fn register_use(&self) -> RegisterUse {
        use Instruction::*;

        match self.clone() {
            Exit => RegisterUse::default(),
            Func  { .. } => RegisterUse::default(),
            End => RegisterUse::default(),
            Label { .. } => RegisterUse::default(),
            Reg  { to, from } => RegisterUse::new([to], [from]),
            BranchNz { cond, .. } => RegisterUse::new([], [cond]),
            BranchEqRR { lhs, rhs, .. }
            | BranchLtRR { lhs, rhs, .. } => RegisterUse::new([], [lhs, rhs]),
            BranchEqIR { rhs, .. }
            | BranchLtIR { rhs, .. } => RegisterUse::new([], [rhs]),
            BranchLtRI { lhs, .. } => RegisterUse::new([], [lhs]),
            Jump { .. } => RegisterUse::default(),
            JumpEz { cond, .. }
            | JumpNz { cond, .. } => RegisterUse::new([], [cond]),
            JumpLtRR { lhs, rhs, .. }
            | JumpLeRR { lhs, rhs, .. }
            | JumpEqRR { lhs, rhs, .. }
            | JumpNeRR { lhs, rhs, .. } => RegisterUse::new([], [lhs, rhs]),
            JumpLtIR { rhs, .. }
            | JumpLeIR { rhs, .. }
            | JumpEqIR { rhs, .. }
            | JumpNeIR { rhs, .. } => RegisterUse::new([], [rhs]),
            JumpLtRI { lhs, .. }
            | JumpLeRI { lhs, .. } => RegisterUse::new([], [lhs]),
            Call { to, args, .. } => RegisterUse { initialized: list![to], used: args },
            Addr { to, .. } => RegisterUse::new([to], []),
            DJump { lbl } => RegisterUse::new([], [lbl]),
            DCall { to, func, mut args } => { 
                args.push(func); 
                RegisterUse { initialized: list![to], used: args } 
            }
            RetR { reg } => RegisterUse::new([], [reg]),
            RetI { .. } => RegisterUse::default(),
            Int { to, .. } => RegisterUse::new([to], []),
            Str { to, .. } => RegisterUse::new([to], []),
            AddRR { to, lhs, rhs }
            | SubRR { to, lhs, rhs }
            | MulRR { to, lhs, rhs } 
            | DivRR { to, lhs, rhs }
            | ModRR { to, lhs, rhs } => RegisterUse::new([to], [lhs, rhs]),
            AddIR { to, rhs, .. }
            | SubIR { to, rhs, .. }
            | MulIR { to, rhs, .. } 
            | DivIR { to, rhs, .. }
            | ModIR { to, rhs, .. }
            | Neg { to, rhs } => RegisterUse::new([to], [rhs]),
            SubRI { to, lhs, .. } 
            | DivRI { to, lhs, .. }
            | ModRI { to, lhs, .. } => RegisterUse::new([to], [lhs]),
            Incr { reg }
            | Decr { reg } => RegisterUse::new([], [reg]),
            ArrR { to, len } => RegisterUse::new([to], [len]),
            ArrI { to, .. } => RegisterUse::new([to], []),
            GetR { to, arr, idx } => RegisterUse::new([to], [arr, idx]),
            GetI { to, arr, .. } => RegisterUse::new([to], [arr]),
            SetII { arr, .. } => RegisterUse::new([], [arr]),
            SetRR { arr, idx, val } => RegisterUse::new([], [arr, idx, val]),
            SetRI { arr, idx, .. } => RegisterUse::new([], [arr, idx]),
            SetIR { arr, val, .. } => RegisterUse::new([], [arr, val]),
            Len { to, arr } => RegisterUse::new([to], [arr]),
            Type { to, obj } => RegisterUse::new([to], [obj]),
            PutcR { char } => RegisterUse::new([], [char]),
            PutcI { .. } => RegisterUse::default(),
        }
    }

    pub fn opcode_name(&self) -> &'static str {
        use Instruction::*;

        match self {
            Exit => "exit",
            Func  { .. } => "func",
            End => "end",
            Label { .. } => "label",
            Reg  { .. } => "reg",
            BranchNz { .. } => "bb",
            BranchEqRR { .. } => "beq_rr",
            BranchEqIR { .. } => "beq_ir",
            BranchLtRR { .. } => "blt_rr",
            BranchLtRI { .. } => "blt_ri",
            BranchLtIR { .. } => "blt_ir",
            Jump { .. } => "jump",
            JumpEz { .. } => "jumpez",
            JumpNz { .. } => "jumpnz",
            JumpLtRR { .. } => "jumplt_rr",
            JumpLtRI { .. } => "jumplt_ri",
            JumpLtIR { .. } => "jumplt_ir",
            JumpLeRR { .. } => "jumple_rr",
            JumpLeRI { .. } => "jumple_ri",
            JumpLeIR { .. } => "jumple_ir",
            JumpEqRR { .. } => "jumpeq_rr",
            JumpEqIR { .. } => "jumpeq_ir",
            JumpNeRR { .. } => "jumpne_rr",
            JumpNeIR { .. } => "jumpne_ir",
            Call { .. } => "call",
            Addr { .. } => "addr",
            DJump { .. } => "djump",
            DCall { .. } => "dcall",
            RetR { .. } => "ret_r",
            RetI { .. } => "ret_i",
            Int { .. } => "int",
            Str { .. } => "str",
            AddRR { .. } => "add_rr",
            AddIR { .. } => "add_ir",
            SubRR { .. } => "sub_rr",
            SubRI { .. } => "sub_ri",
            SubIR { .. } => "sub_ir",
            MulRR { .. } => "mul_rr",
            MulIR { .. } => "mul_ir",
            DivRR { .. } => "div_rr",
            DivRI { .. } => "div_ri",
            DivIR { .. } => "div_ir",
            ModRR { .. } => "mod_rr",
            ModRI { .. } => "mod_ri",
            ModIR { .. } => "mod_ir",
            Neg { .. } => "neg",
            Incr { .. } => "incr",
            Decr { .. } => "decr",
            ArrR { .. } => "arr_r",
            ArrI { .. } => "arr_i",
            GetR { .. } => "get_r",
            GetI { .. } => "get_i",
            SetII { .. } => "set_ii",
            SetRR { .. } => "set_rr",
            SetRI { .. } => "set_ri",
            SetIR { .. } => "set_ir",
            Len { .. } => "len",
            Type { .. } => "type",
            PutcR { .. } => "putc_r",
            PutcI { .. } => "putc_i",
        }
    }
}