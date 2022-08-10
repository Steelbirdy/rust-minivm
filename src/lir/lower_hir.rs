use crate::{
    common::{Interner, Key, List, Reg},
    hir,
    lir::Instruction,
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

struct Registers {
    regs: Vec<RegState>,
}

impl Registers {
    fn new() -> Self {
        Self {
            regs: Vec::with_capacity(16),
        }
    }

    fn grow(&mut self, len: usize) {
        if len <= self.regs.len() {
            return;
        }
        self.regs.resize(len, RegState::default());
    }

    fn get(&mut self, reg: Reg) -> &mut RegState {
        let reg = usize::from(reg);
        self.grow(reg + 1);
        &mut self.regs[reg]
    }

    fn reset(&mut self) {
        for reg in &mut self.regs {
            reg.reset();
        }
    }
}

#[derive(Default, Copy, Clone)]
struct RegState {
    value: Option<Value>,
    in_scope: bool,
}

impl RegState {
    fn is_named(&self) -> bool {
        self.value.is_some()
    }

    fn name(&mut self, value: Value) {
        self.value = Some(value);
        self.in_scope = false;
    }

    fn reset(&mut self) {
        self.value = None;
        self.in_scope = false;
    }
}

pub(crate) struct LoweringContext<'a> {
    gc: &'a mut Gc,
    interner: &'a Interner,
    out: Vec<Instruction>,
    label_locs: FxHashMap<Key, usize>,
    label_refs: FxHashMap<usize, List<Key>>,
    regs: Registers,
}

macro_rules! lower_binary {
    ($this:ident, $to:ident, $args:expr => ($rr:ident, $rv:ident, $vr:ident) |$lhs:ident, $rhs:ident| $vv:expr) => {
        match $this.binary_args($args) {
            BinaryArgs::RR(lhs, rhs) => write!($this; Instruction::$rr { lhs, rhs, to: $to }),
            BinaryArgs::RV(lhs, rhs) => write!($this; Instruction::$rv { lhs, rhs, to: $to }),
            BinaryArgs::VR(lhs, rhs) => write!($this; Instruction::$vr { lhs, rhs, to: $to }),
            BinaryArgs::VV($lhs, $rhs) => $vv,
        }
    };
    ($this:ident, $to:ident, $args:expr => ($rr:ident, $vr:ident) |$lhs:ident, $rhs:ident| $vv:expr) => {
        match $this.binary_args($args) {
            BinaryArgs::RR(lhs, rhs) => write!($this; Instruction::$rr { lhs, rhs, to: $to }),
            BinaryArgs::RV(lhs, rhs) => write!($this; Instruction::$vr { lhs: rhs, rhs: lhs, to: $to }),
            BinaryArgs::VR(lhs, rhs) => write!($this; Instruction::$vr { lhs, rhs, to: $to }),
            BinaryArgs::VV($lhs, $rhs) => $vv,
        }
    };
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
            regs: Registers::new(),
        }
    }

    fn name(&mut self, reg: Reg, value: Value) {
        self.regs.get(reg).name(value);
    }

    fn named(&mut self, reg: Reg) -> Option<Value> {
        self.regs.get(reg).value
    }

    fn clear_named(&mut self, reg: Reg) {
        self.regs.get(reg).reset();
    }

    fn assign_if_named(&mut self, reg: Reg) {
        let state = self.regs.get(reg);
        if state.in_scope {
            return;
        }
        if let Some(val) = state.value {
            state.in_scope = true;
            self.out.push(Instruction::Value { val, to: reg });
        }
    }

    fn addr(&mut self, key: Key) -> usize {
        self.label_refs.entry(self.out.len()).or_default().push(key);
        0
    }

    fn unary_arg(&mut self, arg: hir::UnaryArg) -> UnaryArg {
        match arg {
            hir::UnaryArg::R(reg) => {
                if let Some(value) = self.named(reg) {
                    UnaryArg::V(value)
                } else {
                    UnaryArg::R(reg)
                }
            }
            hir::UnaryArg::I(int) => UnaryArg::V(Value::int(int)),
        }
    }

    fn binary_args(&mut self, args: hir::BinaryArgs) -> BinaryArgs {
        match args {
            hir::BinaryArgs::RR(lhs, rhs) => match (self.named(lhs), self.named(rhs)) {
                (Some(lhs), Some(rhs)) => BinaryArgs::VV(lhs, rhs),
                (Some(lhs), None) => BinaryArgs::VR(lhs, rhs),
                (None, Some(rhs)) => BinaryArgs::RV(lhs, rhs),
                (None, None) => BinaryArgs::RR(lhs, rhs),
            },
            hir::BinaryArgs::RI(lhs, rhs) => {
                let rhs = Value::int(rhs);
                match self.named(lhs) {
                    Some(lhs) => BinaryArgs::VV(lhs, rhs),
                    None => BinaryArgs::RV(lhs, rhs),
                }
            }
            hir::BinaryArgs::IR(lhs, rhs) => {
                let lhs = Value::int(lhs);
                match self.named(rhs) {
                    Some(rhs) => BinaryArgs::VV(lhs, rhs),
                    None => BinaryArgs::VR(lhs, rhs),
                }
            }
        }
    }

    fn set_args(&mut self, args: hir::SetArgs) -> BinaryArgs {
        match args {
            hir::SetArgs::RR(lhs, rhs) => match (self.named(lhs), self.named(rhs)) {
                (Some(lhs), Some(rhs)) => BinaryArgs::VV(lhs, rhs),
                (Some(lhs), None) => BinaryArgs::VR(lhs, rhs),
                (None, Some(rhs)) => BinaryArgs::RV(lhs, rhs),
                (None, None) => BinaryArgs::RR(lhs, rhs),
            },
            hir::SetArgs::RI(lhs, rhs) => {
                let rhs = Value::int(rhs);
                match self.named(lhs) {
                    Some(lhs) => BinaryArgs::VV(lhs, rhs),
                    None => BinaryArgs::RV(lhs, rhs),
                }
            }
            hir::SetArgs::IR(lhs, rhs) => {
                let lhs = Value::int(lhs);
                match self.named(rhs) {
                    Some(rhs) => BinaryArgs::VV(lhs, rhs),
                    None => BinaryArgs::VR(lhs, rhs),
                }
            }
            hir::SetArgs::II(lhs, rhs) => BinaryArgs::VV(Value::int(lhs), Value::int(rhs)),
        }
    }

    fn lower_function(&mut self, func: &hir::Function) {
        self.regs.reset();

        let start_idx = self.out.len();
        // TODO: Track the number of registers used by each function
        self.out.push(Instruction::Func {
            end: 0,
            num_regs: 0,
        });
        self.label_locs.insert(func.name.key, self.out.len());

        for instr in &func.instructions {
            self.lower_instruction(instr);
        }

        let end_idx = self.out.len();
        match &mut self.out[start_idx] {
            Instruction::Func { end, .. } => *end = end_idx,
            _ => unreachable!(),
        }
    }

    fn lower_instruction(&mut self, instr: &hir::InstructionWithRange) {
        use hir::Instruction::*;

        let reg_use = instr.inner.register_use();
        for reg in reg_use.used {
            self.assign_if_named(reg);
        }

        match instr.inner.clone() {
            Exit => {
                write!(self; Instruction::Exit);
            }
            Label { name } => {
                self.label_locs.insert(name.key, self.out.len());
            }
            Reg { to, from } => {
                if let Some(value) = self.named(from) {
                    self.name(to, value);
                } else {
                    write!(self; Instruction::RegR { from, to });
                }
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
                let addr = self.addr(lbl.key);
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
                self.name(to, Value::int(int));
            }
            Str { to, str } => {
                let text = self.interner.lookup(str.key);
                let num_chars = text.len();
                let ptr = self.gc.alloc(num_chars.try_into().unwrap());
                for (i, char) in text.chars().enumerate() {
                    *self.gc.get_mut(ptr, i) = Value::int(u32::from(char).into());
                }
                self.name(to, Value::ptr(ptr));
            }
            Binary { to, op, args } => {
                self.lower_binary(to, op, args);
            }
            Unary { to, op, arg } => {
                self.lower_unary(to, op, arg);
            }
            Incr { reg } => {
                if let Some(value) = self.named(reg) {
                    self.name(reg, Value::int(value.as_int() + 1));
                } else {
                    write!(self; Instruction::Incr { reg });
                }
            }
            Decr { reg } => {
                if let Some(value) = self.named(reg) {
                    self.name(reg, Value::int(value.as_int() - 1));
                } else {
                    write!(self; Instruction::Incr { reg });
                }
            }
            Arr { to, len } => {
                self.clear_named(to);
                match self.unary_arg(len) {
                    UnaryArg::R(len) => write!(self; Instruction::ArrR { len, to }),
                    UnaryArg::V(len) => write!(self; Instruction::ArrI { len: len.as_int(), to }),
                }
            }
            Get { to, arr, idx } => {
                self.clear_named(to);
                match self.unary_arg(idx) {
                    UnaryArg::R(idx) => write!(self; Instruction::GetR { arr, idx, to }),
                    UnaryArg::V(idx) => {
                        write!(self; Instruction::GetI { arr, idx: idx.as_int(), to });
                    }
                }
            }
            Set { arr, idx_and_val } => {
                self.lower_set(arr, idx_and_val);
            }
            Len { to, arr } => {
                self.clear_named(to);
                write!(self; Instruction::Len { arr, to });
            }
            Type { to, obj } => {
                self.clear_named(to);
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
                let addr_f = self.addr(lbl_false.key);
                let addr_t = self.addr(lbl_true.key);
                write!(self; Instruction::$kind { $($cond,)+ addr_f, addr_t })
            }};
        }

        match kind {
            hir::BranchKind::Nz { cond } => {
                if let Some(cond) = self.named(cond) {
                    let lbl = if cond == 0 { lbl_false } else { lbl_true };
                    let addr = self.addr(lbl.key);
                    write!(self; Instruction::Jump { addr });
                } else {
                    instr!(BranchNz, cond);
                }
            }
            hir::BranchKind::Eq { args } => match self.binary_args(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(BranchEqRR, lhs, rhs),
                BinaryArgs::RV(rhs, lhs) | BinaryArgs::VR(lhs, rhs) => {
                    let lhs = lhs.as_int();
                    instr!(BranchEqIR, lhs, rhs);
                }
                BinaryArgs::VV(lhs, rhs) => {
                    let lbl = if lhs == rhs { lbl_true } else { lbl_false };
                    let addr = self.addr(lbl.key);
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
                    let addr = self.addr(lbl.key);
                    write!(self; Instruction::Jump { addr });
                }
            },
        }
    }

    fn lower_jump(&mut self, kind: hir::JumpKind, lbl: hir::KeyWithRange) {
        macro_rules! instr {
            ($kind:ident $(, $cond:ident)*) => {{
                let addr = self.addr(lbl.key);
                write!(self; Instruction::$kind { $($cond,)* addr })
            }};
        }

        match kind {
            hir::JumpKind::Unconditional => instr!(Jump),
            hir::JumpKind::Ez { cond } => {
                if let Some(cond) = self.named(cond) {
                    if cond != 0 {
                        return;
                    }
                    instr!(Jump);
                } else {
                    instr!(JumpEz, cond);
                }
            }
            hir::JumpKind::Nz { cond } => {
                if let Some(cond) = self.named(cond) {
                    if cond == 0 {
                        return;
                    }
                    instr!(Jump);
                } else {
                    instr!(JumpNz, cond);
                }
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
        self.clear_named(to);
        for &arg in &args {
            self.assign_if_named(arg);
        }
        let addr = self.addr(func.key);
        write!(self; Instruction::call(addr, args, 16, to));
    }

    fn lower_djump(&mut self, kind: hir::JumpKind, lbl: Reg) {
        match kind {
            hir::JumpKind::Unconditional => write!(self; Instruction::DJump { addr: lbl }),
            _ => panic!("currently only `jump` can be used with a dynamic jump target"),
        }
    }

    fn lower_dcall(&mut self, to: Reg, func: Reg, args: List<Reg>) {
        self.clear_named(to);
        for &arg in &args {
            self.assign_if_named(arg);
        }
        write!(self; Instruction::dcall(func, args, 16, to));
    }

    fn lower_binary(&mut self, to: Reg, op: hir::BinaryOp, args: hir::BinaryArgs) {
        match op {
            hir::BinaryOp::Add => lower_binary!(self, to, args => (AddRR, AddIR) |lhs, rhs| {
                assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                self.name(to, lhs + rhs);
            }),
            hir::BinaryOp::Sub => {
                lower_binary!(self, to, args => (SubRR, SubRI, SubIR) |lhs, rhs| {
                    assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                    self.name(to, lhs - rhs);
                });
            }
            hir::BinaryOp::Mul => lower_binary!(self, to, args => (MulRR, MulIR) |lhs, rhs| {
                assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                self.name(to, lhs * rhs);
            }),
            hir::BinaryOp::Div => {
                lower_binary!(self, to, args => (DivRR, DivRI, DivIR) |lhs, rhs| {
                    assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                    self.name(to, lhs / rhs);
                });
            }
            hir::BinaryOp::Mod => {
                lower_binary!(self, to, args => (ModRR, ModRI, ModIR) |lhs, rhs| {
                    assert!(lhs.is_int() && rhs.is_int(), "pointers cannot be used in math operations");
                    self.name(to, lhs % rhs);
                });
            }
        }
    }

    fn lower_unary(&mut self, to: Reg, op: hir::UnaryOp, arg: Reg) {
        if let Some(arg) = self.named(arg) {
            assert!(arg.is_int(), "pointers cannot be used in math operations");
            self.name(to, -arg);
            return;
        }

        self.clear_named(to);
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
