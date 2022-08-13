use crate::{
    common::{list, Int, Key, List, Reg},
    hir::JumpSet,
};
use rustc_hash::FxHashMap;
use std::cell::Cell;
use text_size::TextRange;

#[derive(Debug)]
pub struct Program {
    pub functions: FxHashMap<Key, Function>,
    pub labels: FxHashMap<Key, LabelInfo>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LabelInfo {
    pub range: TextRange,
    pub func: Key,
    pub index: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct KeyWithRange {
    pub key: Key,
    pub range: TextRange,
}

#[derive(Debug)]
pub struct Function {
    pub name: KeyWithRange,
    pub instructions: Vec<InstructionWithRange>,
    pub jumps: Vec<Cell<JumpSet>>,
    pub regs_used: Reg,
}

impl Function {
    pub fn instructions_and_jumps(&self) -> impl Iterator<Item = (&InstructionWithRange, JumpSet)> {
        self.instructions
            .iter()
            .zip(self.jumps.iter().map(Cell::get))
    }
}

#[derive(Default)]
pub struct RegisterUsage {
    pub initialized: List<Reg>,
    pub used: List<Reg>,
}

impl RegisterUsage {
    pub fn new<const N: usize, const M: usize>(init: [Reg; N], used: [Reg; M]) -> Self {
        Self {
            initialized: List::from_iter(init),
            used: List::from_iter(used),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionWithRange {
    pub inner: Instruction,
    pub range: TextRange,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Instruction {
    Exit,
    Label {
        name: KeyWithRange,
    },
    Reg {
        to: Reg,
        from: Reg,
    },
    Branch {
        kind: BranchKind,
        lbl_false: KeyWithRange,
        lbl_true: KeyWithRange,
    },
    Jump {
        kind: JumpKind,
        lbl: KeyWithRange,
    },
    Call {
        to: Reg,
        func: KeyWithRange,
        args: List<Reg>,
    },
    Addr {
        to: Reg,
        lbl: KeyWithRange,
    },
    DJump {
        kind: JumpKind,
        lbl: Reg,
    },
    DCall {
        to: Reg,
        func: Reg,
        args: List<Reg>,
    },
    Ret {
        arg: UnaryArg,
    },
    Int {
        to: Reg,
        int: Int,
    },
    Str {
        to: Reg,
        str: KeyWithRange,
    },
    Binary {
        op: BinaryOp,
        to: Reg,
        args: BinaryArgs,
    },
    Unary {
        op: UnaryOp,
        to: Reg,
        arg: Reg,
    },
    Incr {
        reg: Reg,
    },
    Decr {
        reg: Reg,
    },
    Arr {
        to: Reg,
        len: UnaryArg,
    },
    Get {
        to: Reg,
        arr: Reg,
        idx: UnaryArg,
    },
    Set {
        arr: Reg,
        idx_and_val: SetArgs,
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
        arg: UnaryArg,
    },
}

impl Instruction {
    pub fn register_use(&self) -> RegisterUsage {
        use Instruction::*;

        match self.clone() {
            Exit | Label { .. } => RegisterUsage::default(),
            Reg { to, from } => RegisterUsage::new([to], [from]),
            Branch { kind, .. } => kind.register_use(),
            Jump { kind, .. } => kind.register_use(None),
            Call { to, args, .. } => RegisterUsage {
                initialized: list![to],
                used: args,
            },
            Addr { to, .. } => RegisterUsage::new([to], []),
            DJump { kind, lbl } => kind.register_use(Some(lbl)),
            DCall { to, func, mut args } => {
                args.push(func);
                RegisterUsage {
                    initialized: list![to],
                    used: args,
                }
            }
            Ret { arg } => arg.register_use([]),
            Int { to, .. } | Str { to, .. } => RegisterUsage::new([to], []),
            Binary { to, args, .. } => args.register_use([to]),
            Unary { to, arg, .. } => RegisterUsage::new([to], [arg]),
            Incr { reg } | Decr { reg } => RegisterUsage::new([], [reg]),
            Arr { to, len } => len.register_use([to]),
            Get { to, arr, idx } => {
                let mut ret = idx.register_use([to]);
                ret.used.push(arr);
                ret
            }
            Set { arr, idx_and_val } => idx_and_val.register_use(arr),
            Len { to, arr: obj } | Type { to, obj } => RegisterUsage::new([to], [obj]),
            Putc { arg } => arg.register_use([]),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryArg {
    R(Reg),
    I(Int),
}

impl UnaryArg {
    fn register_use<const N: usize>(self, init: [Reg; N]) -> RegisterUsage {
        match self {
            Self::R(reg) => RegisterUsage::new(init, [reg]),
            Self::I(_) => RegisterUsage::new(init, []),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryArgs {
    RR(Reg, Reg),
    RI(Reg, Int),
    IR(Int, Reg),
}

impl BinaryArgs {
    fn register_use<const N: usize>(self, init: [Reg; N]) -> RegisterUsage {
        match self {
            Self::RR(lhs, rhs) => RegisterUsage::new(init, [lhs, rhs]),
            Self::RI(lhs, _) => RegisterUsage::new(init, [lhs]),
            Self::IR(_, rhs) => RegisterUsage::new(init, [rhs]),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BranchKind {
    Nz { cond: Reg },
    Eq { args: BinaryArgs },
    Lt { args: BinaryArgs },
}

impl BranchKind {
    fn register_use(self) -> RegisterUsage {
        match self {
            Self::Nz { cond } => RegisterUsage::new([], [cond]),
            Self::Eq { args } | Self::Lt { args } => args.register_use([]),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum JumpKind {
    Unconditional,
    Ez { cond: Reg },
    Nz { cond: Reg },
    Eq { args: BinaryArgs },
    Ne { args: BinaryArgs },
    Lt { args: BinaryArgs },
    Le { args: BinaryArgs },
}

impl JumpKind {
    fn register_use(self, lbl: Option<Reg>) -> RegisterUsage {
        let mut ret = match self {
            Self::Unconditional => RegisterUsage::default(),
            Self::Ez { cond } | Self::Nz { cond } => RegisterUsage::new([], [cond]),
            Self::Eq { args } | Self::Ne { args } | Self::Lt { args } | Self::Le { args } => {
                args.register_use([])
            }
        };
        ret.used.extend(lbl);
        ret
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SetArgs {
    RR(Reg, Reg),
    RI(Reg, Int),
    IR(Int, Reg),
    II(Int, Int),
}

impl SetArgs {
    fn register_use(self, arr: Reg) -> RegisterUsage {
        match self {
            Self::RR(lhs, rhs) => RegisterUsage::new([], [arr, lhs, rhs]),
            Self::RI(lhs, _) => RegisterUsage::new([], [arr, lhs]),
            Self::IR(_, rhs) => RegisterUsage::new([], [arr, rhs]),
            Self::II(_, _) => RegisterUsage::new([], [arr]),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOp {
    Neg,
}
