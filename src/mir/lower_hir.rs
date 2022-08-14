use crate::{
    common::{Interner, Key, List, Reg},
    hir::{self, JumpSet},
    mir::Instruction,
    vm::{Gc, Value},
};
use rustc_hash::FxHashMap;

enum UnaryArg {
    R(Reg),
    V(Value),
}

enum BinaryArgs {
    RR(Reg, Reg),
    RV(Reg, Value),
    VR(Value, Reg),
    VV(Value, Value),
}

pub(crate) struct LoweringContext<'a> {
    gc: &'a mut Gc,
    interner: &'a Interner,
    out: Vec<Instruction>,
    label_locs: FxHashMap<Key, usize>,
    label_refs: FxHashMap<usize, List<Key>>,
    regs_used: Reg,
}

macro_rules! lower_binary {
    ($this:ident, $to:ident, $args:expr => ($rr:ident, $rv:ident, $vr:ident) |$lhs:ident, $rhs:ident| $vv:expr) => {{
        let args = $this.binary_args($args);
        if let BinaryArgs::VV($lhs, $rhs) = args {
            return $vv;
        }
        match args {
            BinaryArgs::RR(lhs, rhs) => write!($this; Instruction::$rr { lhs, rhs, to: $to }),
            BinaryArgs::RV(lhs, rhs) => write!($this; Instruction::$rv { lhs, rhs, to: $to }),
            BinaryArgs::VR(lhs, rhs) => write!($this; Instruction::$vr { lhs, rhs, to: $to }),
            BinaryArgs::VV($lhs, $rhs) => unreachable!(),
        }
    }};
    ($this:ident, $to:ident, $args:expr => ($rr:ident, $vr:ident) |$lhs:ident, $rhs:ident| $vv:expr) => {{
        let args = $this.binary_args($args);
        if let BinaryArgs::VV($lhs, $rhs) = args {
            return $vv;
        }
        match args {
            BinaryArgs::RR(lhs, rhs) => write!($this; Instruction::$rr { lhs, rhs, to: $to }),
            BinaryArgs::RV(lhs, rhs) => write!($this; Instruction::$vr { lhs: rhs, rhs: lhs, to: $to }),
            BinaryArgs::VR(lhs, rhs) => write!($this; Instruction::$vr { lhs, rhs, to: $to }),
            BinaryArgs::VV($lhs, $rhs) => unreachable!(),
        }
    }};
}

macro_rules! write {
    ($this:ident; $instr:expr) => {
        $this.out.push($instr)
    };
}

impl<'a> LoweringContext<'a> {
    fn new(gc: &'a mut Gc, interner: &'a Interner) -> Self {
        Self {
            gc,
            interner,
            out: Vec::new(),
            label_locs: FxHashMap::default(),
            label_refs: FxHashMap::default(),
            regs_used: Reg::MAX,
        }
    }

    fn addr(&mut self, key: Key) -> usize {
        self.label_refs.entry(self.out.len()).or_default().push(key);
        0
    }

    fn unary_arg(&mut self, arg: hir::UnaryArg) -> UnaryArg {
        match arg {
            hir::UnaryArg::R(reg) => UnaryArg::R(reg),
            hir::UnaryArg::I(int) => UnaryArg::V(Value::int(int)),
        }
    }

    fn binary_args(&mut self, args: hir::BinaryArgs) -> BinaryArgs {
        match args {
            hir::BinaryArgs::RR(lhs, rhs) => BinaryArgs::RR(lhs, rhs),
            hir::BinaryArgs::RI(lhs, rhs) => BinaryArgs::RV(lhs, Value::int(rhs)),
            hir::BinaryArgs::IR(lhs, rhs) => BinaryArgs::VR(Value::int(lhs), rhs),
        }
    }

    fn set_args(&mut self, args: hir::SetArgs) -> BinaryArgs {
        match args {
            hir::SetArgs::RR(lhs, rhs) => BinaryArgs::RR(lhs, rhs),
            hir::SetArgs::RI(lhs, rhs) => BinaryArgs::RV(lhs, Value::int(rhs)),
            hir::SetArgs::IR(lhs, rhs) => BinaryArgs::VR(Value::int(lhs), rhs),
            hir::SetArgs::II(lhs, rhs) => BinaryArgs::VV(Value::int(lhs), Value::int(rhs)),
        }
    }

    fn lower_function(&mut self, func: &hir::Function) {
        self.regs_used = func.regs_used;

        let start_idx = self.out.len();
        // TODO: Track the number of registers used by each function
        self.out.push(Instruction::Func {
            end: 0,
            num_regs: func.regs_used,
        });
        self.label_locs.insert(func.name.value, self.out.len());

        for (instr, jumps) in func.instructions_and_jumps() {
            self.lower_instruction(instr, jumps);
        }

        let end_idx = self.out.len();
        match &mut self.out[start_idx] {
            Instruction::Func { end, .. } => {
                *end = end_idx;
            }
            _ => unreachable!(),
        }
    }

    fn lower_instruction(&mut self, instr: &hir::InstructionWithRange, _jumps: JumpSet) {
        use hir::Instruction::*;

        match instr.value.clone() {
            Exit => {
                write!(self; Instruction::Exit);
            }
            Label { name } => {
                self.label_locs.insert(name.value, self.out.len());
            }
            Reg { to, from } => {
                write!(self; Instruction::RegR { from, to });
            }
            Branch {
                kind,
                lbl_false,
                lbl_true,
            } => {
                self.lower_branch(kind, lbl_false, lbl_true);
            }
            Jump { kind, lbl } => {
                self.lower_jump(kind, lbl);
            }
            Call { to, func, args } => {
                self.lower_call(to, func, args);
            }
            Addr { to, lbl } => {
                let addr = self.addr(lbl.value);
                write!(self; Instruction::RegA { addr, to });
            }
            DJump { kind, lbl } => {
                self.lower_djump(kind, lbl);
            }
            DCall { to, func, args } => {
                self.lower_dcall(to, func, args);
            }
            Ret { arg } => match self.unary_arg(arg) {
                UnaryArg::R(reg) => write!(self; Instruction::RetR { reg }),
                UnaryArg::V(val) => write!(self; Instruction::RetV { val }),
            },
            Int { to, int } => {
                write!(self; Instruction::Value { val: Value::int(int), to });
            }
            Str { to, str } => {
                let text = self.interner.lookup(str.value);
                let num_chars = text.len();
                let ptr = self.gc.alloc(num_chars.try_into().unwrap());
                for (i, char) in text.chars().enumerate() {
                    *self.gc.get_mut(ptr, i) = Value::int(u32::from(char).into());
                }
                write!(self; Instruction::Value { val: Value::ptr(ptr), to });
            }
            Binary { to, op, args } => {
                self.lower_binary(to, op, args);
            }
            Unary { to, op, arg } => {
                self.lower_unary(to, op, arg);
            }
            Incr { reg } => {
                write!(self; Instruction::Incr { reg });
            }
            Decr { reg } => {
                write!(self; Instruction::Decr { reg });
            }
            Arr { to, len } => match self.unary_arg(len) {
                UnaryArg::R(len) => write!(self; Instruction::ArrR { len, to }),
                UnaryArg::V(len) => write!(self; Instruction::ArrI { len: len.as_int(), to }),
            },
            Get { to, arr, idx } => match self.unary_arg(idx) {
                UnaryArg::R(idx) => write!(self; Instruction::GetR { arr, idx, to }),
                UnaryArg::V(idx) => {
                    write!(self; Instruction::GetI { arr, idx: idx.as_int(), to });
                }
            },
            Set { arr, idx_and_val } => {
                self.lower_set(arr, idx_and_val);
            }
            Len { to, arr } => {
                write!(self; Instruction::Len { arr, to });
            }
            Type { to, obj } => {
                write!(self; Instruction::Type { obj, to });
            }
            Putc { arg } => match self.unary_arg(arg) {
                UnaryArg::R(reg) => write!(self; Instruction::PutcR { ch: reg }),
                UnaryArg::V(val) => {
                    let ch = u32::try_from(val.as_int()).unwrap();
                    write!(self; Instruction::PutcI { ch });
                }
            },
        }
    }

    fn lower_branch(
        &mut self,
        kind: hir::BranchKind,
        lbl_false: hir::KeyWithRange,
        lbl_true: hir::KeyWithRange,
    ) {
        macro_rules! instr {
            ($kind:ident, $($cond:ident),+) => {{
                let addr_f = self.addr(lbl_false.value);
                let addr_t = self.addr(lbl_true.value);
                write!(self; Instruction::$kind { $($cond,)+ addr_f, addr_t })
            }};
        }

        match kind {
            hir::BranchKind::Nz { cond } => {
                instr!(BranchNz, cond);
            }
            hir::BranchKind::Eq { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(BranchEqRR, lhs, rhs),
                BinaryArgs::RV(rhs, lhs) | BinaryArgs::VR(lhs, rhs) => {
                    let lhs = lhs.as_int();
                    instr!(BranchEqIR, lhs, rhs);
                }
                BinaryArgs::VV(lhs, rhs) => {
                    let lbl = if lhs == rhs { lbl_true } else { lbl_false };
                    let addr = self.addr(lbl.value);
                    write!(self; Instruction::Jump { addr });
                }
            },
            hir::BranchKind::Lt { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(BranchLtRR, lhs, rhs),
                BinaryArgs::RV(lhs, rhs) => {
                    let rhs = rhs.as_int();
                    instr!(BranchLtRI, lhs, rhs);
                }
                BinaryArgs::VR(lhs, rhs) => {
                    let lhs = lhs.as_int();
                    instr!(BranchLtIR, lhs, rhs);
                }
                BinaryArgs::VV(lhs, rhs) => {
                    let lbl = if lhs < rhs { lbl_true } else { lbl_false };
                    let addr = self.addr(lbl.value);
                    write!(self; Instruction::Jump { addr });
                }
            },
        }
    }

    fn lower_jump(&mut self, kind: hir::JumpKind, lbl: hir::KeyWithRange) {
        macro_rules! instr {
            ($kind:ident $(, $cond:ident)*) => {{
                let addr = self.addr(lbl.value);
                write!(self; Instruction::$kind { $($cond,)* addr })
            }};
        }

        match kind {
            hir::JumpKind::Unconditional => instr!(Jump),
            hir::JumpKind::Ez { cond } => {
                instr!(JumpEz, cond);
            }
            hir::JumpKind::Nz { cond } => {
                instr!(JumpNz, cond);
            }
            hir::JumpKind::Eq { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(JumpEqRR, lhs, rhs),
                BinaryArgs::RV(rhs, lhs) | BinaryArgs::VR(lhs, rhs) => instr!(JumpEqIR, lhs, rhs),
                BinaryArgs::VV(lhs, rhs) => {
                    if lhs == rhs {
                        instr!(Jump);
                    }
                }
            },
            hir::JumpKind::Ne { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(JumpNeRR, lhs, rhs),
                BinaryArgs::RV(rhs, lhs) | BinaryArgs::VR(lhs, rhs) => instr!(JumpNeIR, lhs, rhs),
                BinaryArgs::VV(lhs, rhs) => {
                    if lhs != rhs {
                        instr!(Jump);
                    }
                }
            },
            hir::JumpKind::Lt { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(JumpLtRR, lhs, rhs),
                BinaryArgs::RV(lhs, rhs) => instr!(JumpLtRI, lhs, rhs),
                BinaryArgs::VR(lhs, rhs) => instr!(JumpLtIR, lhs, rhs),
                BinaryArgs::VV(lhs, rhs) => {
                    if lhs < rhs {
                        instr!(Jump);
                    }
                }
            },
            hir::JumpKind::Le { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(JumpLeRR, lhs, rhs),
                BinaryArgs::RV(lhs, rhs) => instr!(JumpLeRI, lhs, rhs),
                BinaryArgs::VR(lhs, rhs) => instr!(JumpLeIR, lhs, rhs),
                BinaryArgs::VV(lhs, rhs) => {
                    if lhs <= rhs {
                        instr!(Jump);
                    }
                }
            },
        }
    }

    fn lower_call(&mut self, to: Reg, func: hir::KeyWithRange, args: List<Reg>) {
        let addr = self.addr(func.value);
        write!(self; Instruction::call(addr, args, self.regs_used, to));
    }

    fn lower_djump(&mut self, kind: hir::JumpKind, lbl: Reg) {
        match kind {
            hir::JumpKind::Unconditional => write!(self; Instruction::DJump { addr: lbl }),
            _ => panic!("currently only `jump` can be used with a dynamic jump target"),
        }
    }

    fn lower_dcall(&mut self, to: Reg, func: Reg, args: List<Reg>) {
        write!(self; Instruction::dcall(func, args, self.regs_used, to));
    }

    fn lower_binary(&mut self, to: Reg, op: hir::BinaryOp, args: hir::BinaryArgs) {
        match op {
            hir::BinaryOp::Add => lower_binary!(self, to, args => (AddRR, AddIR) |lhs, rhs| {
                assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                write!(self; Instruction::Value { val: lhs + rhs, to });
            }),
            hir::BinaryOp::Sub => {
                lower_binary!(self, to, args => (SubRR, SubRI, SubIR) |lhs, rhs| {
                    assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                    write!(self; Instruction::Value { val: lhs - rhs, to });
                });
            }
            hir::BinaryOp::Mul => lower_binary!(self, to, args => (MulRR, MulIR) |lhs, rhs| {
                assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                write!(self; Instruction::Value { val: lhs * rhs, to });
            }),
            hir::BinaryOp::Div => {
                lower_binary!(self, to, args => (DivRR, DivRI, DivIR) |lhs, rhs| {
                    assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                    write!(self; Instruction::Value { val: lhs / rhs, to });
                });
            }
            hir::BinaryOp::Mod => {
                lower_binary!(self, to, args => (ModRR, ModRI, ModIR) |lhs, rhs| {
                    assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                    write!(self; Instruction::Value { val: lhs % rhs, to });
                });
            }
        }
    }

    fn lower_unary(&mut self, to: Reg, op: hir::UnaryOp, arg: Reg) {
        match op {
            hir::UnaryOp::Neg => write!(self; Instruction::Neg { reg: arg, to }),
        }
    }

    fn lower_set(&mut self, arr: Reg, idx_and_val: hir::SetArgs) {
        match self.set_args(idx_and_val) {
            BinaryArgs::RR(idx, val) => write!(self; Instruction::SetRR { arr, idx, val }),
            BinaryArgs::RV(idx, val) => write!(self; Instruction::SetRI { arr, idx, val }),
            BinaryArgs::VR(idx, val) => {
                write!(self; Instruction::SetIR { arr, idx: idx.as_int(), val });
            }
            BinaryArgs::VV(idx, val) => {
                write!(self; Instruction::SetII { arr, idx: idx.as_int(), val });
            }
        }
    }

    fn patch_addrs(&mut self) {
        for (ref_index, lbl) in &self.label_refs {
            use Instruction::*;

            let mut i = 0;
            #[allow(non_snake_case)]
            macro_rules! L {
                ($($index:literal > $field:ident),+) => {{
                    $(
                    i += 1;
                    *$field = self.label_locs[&lbl[$index]];
                    )+
                }};
            }

            match &mut self.out[*ref_index] {
                Func { end, .. } => L!(0 > end),
                RegA { addr, .. } => L!(0 > addr),
                BranchNz { addr_f, addr_t, .. }
                | BranchEqRR { addr_f, addr_t, .. }
                | BranchEqIR { addr_f, addr_t, .. }
                | BranchLtRR { addr_f, addr_t, .. }
                | BranchLtRI { addr_f, addr_t, .. }
                | BranchLtIR { addr_f, addr_t, .. } => L!(0 > addr_f, 1 > addr_t),
                Jump { addr }
                | JumpEz { addr, .. }
                | JumpNz { addr, .. }
                | JumpLtRR { addr, .. }
                | JumpLtRI { addr, .. }
                | JumpLtIR { addr, .. }
                | JumpLeRR { addr, .. }
                | JumpLeRI { addr, .. }
                | JumpLeIR { addr, .. }
                | JumpEqRR { addr, .. }
                | JumpEqIR { addr, .. }
                | JumpNeRR { addr, .. }
                | JumpNeIR { addr, .. }
                | Call0 { addr, .. }
                | Call1 { addr, .. }
                | Call2 { addr, .. }
                | Call3 { addr, .. }
                | Call4 { addr, .. }
                | Call5 { addr, .. }
                | Call6 { addr, .. }
                | Call7 { addr, .. }
                | Call8 { addr, .. }
                | CallL { addr, .. } => L!(0 > addr),
                x => unreachable!("{x:?}"),
            }

            assert_eq!(i, lbl.len(), "{:?}", self.out[*ref_index]);
        }
    }
}

pub fn lower_hir(program: &hir::Program, gc: &mut Gc, interner: &Interner) -> Vec<Instruction> {
    let mut ctx = LoweringContext::new(gc, interner);

    let addr = ctx.addr(Key::entry_point());
    write!(ctx; Instruction::Jump { addr });

    let mut functions: List<_> = program.functions.values().collect();
    functions.sort_unstable_by_key(|func| func.name.range.start());

    for func in functions {
        ctx.lower_function(func);
    }

    ctx.patch_addrs();

    ctx.out
}
