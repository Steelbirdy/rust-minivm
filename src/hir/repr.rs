use crate::common::{list, Int, Key, List, Reg};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug)]
pub struct Program {
    pub functions: FxHashMap<Key, Function>,
    pub labels: FxHashMap<Key, TextRange>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct KeyWithRange {
    pub key: Key,
    pub range: TextRange,
}

#[derive(Debug)]
pub struct Function {
    pub name: KeyWithRange,
    pub instructions: List<InstructionWithRange>,
}

#[derive(Default)]
pub struct RegisterUse {
    pub initialized: List<Reg>,
    pub used: List<Reg>,
}

impl RegisterUse {
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
    pub fn register_use(&self) -> RegisterUse {
        use Instruction::*;

        match self.clone() {
            Exit | Label { .. } => RegisterUse::default(),
            Reg { to, from } => RegisterUse::new([to], [from]),
            Branch { kind, .. } => kind.register_use(),
            Jump { kind, .. } => kind.register_use(None),
            Call { to, args, .. } => RegisterUse {
                initialized: list![to],
                used: args,
            },
            Addr { to, .. } => RegisterUse::new([to], []),
            DJump { kind, lbl } => kind.register_use(Some(lbl)),
            DCall { to, func, mut args } => {
                args.push(func);
                RegisterUse {
                    initialized: list![to],
                    used: args,
                }
            }
            Ret { arg } => arg.register_use([]),
            Int { to, .. } | Str { to, .. } => RegisterUse::new([to], []),
            Binary { to, args, .. } => args.register_use([to]),
            Unary { to, arg, .. } => RegisterUse::new([to], [arg]),
            Incr { reg } | Decr { reg } => RegisterUse::new([], [reg]),
            Arr { to, len } => len.register_use([to]),
            Get { to, arr, idx } => {
                let mut ret = idx.register_use([to]);
                ret.used.push(arr);
                ret
            }
            Set { arr, idx_and_val } => idx_and_val.register_use(arr),
            Len { to, arr: obj } | Type { to, obj } => RegisterUse::new([to], [obj]),
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
    fn register_use<const N: usize>(self, init: [Reg; N]) -> RegisterUse {
        match self {
            Self::R(reg) => RegisterUse::new(init, [reg]),
            Self::I(_) => RegisterUse::new(init, []),
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
    fn register_use<const N: usize>(self, init: [Reg; N]) -> RegisterUse {
        match self {
            Self::RR(lhs, rhs) => RegisterUse::new(init, [lhs, rhs]),
            Self::RI(lhs, _) => RegisterUse::new(init, [lhs]),
            Self::IR(_, rhs) => RegisterUse::new(init, [rhs]),
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
    fn register_use(self) -> RegisterUse {
        match self {
            Self::Nz { cond } => RegisterUse::new([], [cond]),
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
    fn register_use(self, lbl: Option<Reg>) -> RegisterUse {
        let mut ret = match self {
            Self::Unconditional => RegisterUse::default(),
            Self::Ez { cond } | Self::Nz { cond } => RegisterUse::new([], [cond]),
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
    fn register_use(self, arr: Reg) -> RegisterUse {
        match self {
            Self::RR(lhs, rhs) => RegisterUse::new([], [arr, lhs, rhs]),
            Self::RI(lhs, _) => RegisterUse::new([], [arr, lhs]),
            Self::IR(_, rhs) => RegisterUse::new([], [arr, rhs]),
            Self::II(_, _) => RegisterUse::new([], [arr]),
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
