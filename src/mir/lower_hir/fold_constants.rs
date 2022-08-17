use crate::{
    common::{Int, Reg},
    hir::{self, BinaryOp, BranchKind, Function, JumpFlags, JumpKind, KeyWithRange, UnaryOp},
    mir::lower_hir::{registers::Registers, BinaryArgs, LoweringContext, UnaryArg},
    vm::{TaggedValue, Value},
};

pub enum FoldResult<T> {
    Keep,
    None,
    Some(T),
}

pub(super) struct FoldConstants<'ctx, 'a> {
    ctx: &'a mut LoweringContext<'ctx>,
    registers: Registers,
    out: Vec<hir::InstructionKind>,
    delayed_label: Option<KeyWithRange>,
}

impl<'ctx, 'a> FoldConstants<'ctx, 'a> {
    pub fn new(ctx: &'a mut LoweringContext<'ctx>) -> Self {
        Self {
            ctx,
            registers: Registers::default(),
            out: Vec::new(),
            delayed_label: None,
        }
    }

    fn reg(&self, reg: Reg) -> Option<Value> {
        self.registers[reg].get()
    }

    pub fn fold(&mut self, func: &Function) -> Vec<hir::InstructionKind> {
        self.registers.reset();

        for (i, instr) in func.instructions.iter().enumerate() {
            let jumps = instr.jumps;
            if jumps.intersects(JumpFlags::IN | JumpFlags::OUT) {
                self.dump_registers(func, i + 1);
            }
            if jumps.intersects(JumpFlags::IN) {
                self.registers.reset();
            }

            /* This fixes a bug that occurs (as an example) with the following assembly code:
             ```vasm
             ... # Inputs: r1 is an array
             r2 <- len r1
             r0 <- int 0
             @loop
             r3 <- get r1 r0
             putchar r3
             incr r0
             jumpne r0 r2 loop
             ...
             ```
             Without delaying adding the label, the code is (roughly) compiled as:
             ```
             0000 | len_rr    [r1, r2]
             0005 | value     [0, r0]
             0016 | get_rr    [r1, r0, r3]
             0023 | putc_r    [r3]
             0026 | incr      [r0]
             0029 | jumpne_rr [r0, r2, @5]
             ```
             which is incorrect since it resets `r0` every iteration.
            */
            if let Some(name) = self.delayed_label.take() {
                self.out.push(hir::InstructionKind::Label { name });
            }

            match hir::Visit::visit(self, &instr.kind) {
                FoldResult::Keep => self.out.push(instr.kind.clone()),
                FoldResult::None => {}
                FoldResult::Some(kind) => self.out.push(kind),
            }

            if jumps.contains(JumpFlags::OUT) {
                self.registers.reset();
            }
        }

        std::mem::take(&mut self.out)
    }

    fn push_value(&mut self, to: Reg, val: Value) {
        self.out.push(match val.tagged() {
            TaggedValue::Int(int) => hir::InstructionKind::Int { to, int },
            TaggedValue::Ptr(ptr) => hir::InstructionKind::Ptr { to, ptr },
        });
    }

    fn dump_registers(&mut self, func: &Function, start: usize) {
        for (reg, constant) in self.registers.iter_mut() {
            if !constant.is_in_scope() && hir::register_is_used(self.ctx.program, func, reg, start)
            {
                let val = constant.get();
                self.out.push(match val.tagged() {
                    TaggedValue::Int(int) => hir::InstructionKind::Int { to: reg, int },
                    TaggedValue::Ptr(ptr) => hir::InstructionKind::Ptr { to: reg, ptr },
                });
            }
        }
    }

    fn assign_reg(&mut self, reg: Reg) {
        let state = &mut self.registers[reg];
        if !state.is_in_scope() {
            state.put_in_scope();
            let val = state.get().unwrap();
            self.push_value(reg, val);
        }
    }

    fn fold_binary_args(&mut self, args: BinaryArgs) -> BinaryArgs {
        match BinaryArgs::from(args) {
            BinaryArgs::RR(lhs, rhs) => {
                match (self.registers[lhs].get(), self.registers[rhs].get()) {
                    (Some(lhs), Some(rhs)) => BinaryArgs::VV(lhs, rhs),
                    (Some(lhs), None) => BinaryArgs::VR(lhs, rhs),
                    (None, Some(rhs)) => BinaryArgs::RV(lhs, rhs),
                    (None, None) => BinaryArgs::RR(lhs, rhs),
                }
            }
            BinaryArgs::VR(lhs, rhs) => match self.registers[rhs].get() {
                Some(rhs) => BinaryArgs::VV(lhs, rhs),
                None => BinaryArgs::VR(lhs, rhs),
            },
            BinaryArgs::RV(lhs, rhs) => match self.registers[lhs].get() {
                Some(lhs) => BinaryArgs::VV(lhs, rhs),
                None => BinaryArgs::RV(lhs, rhs),
            },
            _ => unreachable!(),
        }
    }

    fn fold_unary_arg(&mut self, arg: UnaryArg) -> UnaryArg {
        match arg {
            UnaryArg::R(arg) => match self.registers[arg].get() {
                Some(arg) => UnaryArg::V(arg),
                None => UnaryArg::R(arg),
            },
            UnaryArg::V(arg) => UnaryArg::V(arg),
        }
    }
}

impl hir::Visit for FoldConstants<'_, '_> {
    type Output = FoldResult<hir::InstructionKind>;

    fn unimplemented(&mut self) -> Self::Output {
        FoldResult::Keep
    }

    fn visit_label(&mut self, name: KeyWithRange) -> Self::Output {
        self.delayed_label = Some(name);
        FoldResult::None
    }

    fn visit_reg(&mut self, to: Reg, from: Reg) -> Self::Output {
        if let Some(value) = self.reg(from) {
            self.registers[to].set(value);
            FoldResult::None
        } else {
            self.registers[to].reset();
            FoldResult::Keep
        }
    }

    fn visit_branch(
        &mut self,
        kind: BranchKind,
        lbl_false: KeyWithRange,
        lbl_true: KeyWithRange,
    ) -> Self::Output {
        macro_rules! jump {
            ($cond:expr) => {{
                let lbl = if $cond { lbl_true } else { lbl_false };
                FoldResult::Some(hir::InstructionKind::Jump {
                    kind: JumpKind::Unconditional,
                    lbl,
                })
            }};
        }

        match kind {
            BranchKind::Nz { cond } => match self.reg(cond) {
                Some(value) => jump!(value != 0),
                None => FoldResult::Keep,
            },
            BranchKind::Eq { args } => match self.fold_binary_args(args.into()) {
                BinaryArgs::VV(lhs, rhs) => jump!(lhs == rhs),
                args => FoldResult::Some(hir::InstructionKind::Branch {
                    kind: BranchKind::Eq {
                        args: args.try_into().unwrap(),
                    },
                    lbl_false,
                    lbl_true,
                }),
            },
            BranchKind::Lt { args } => match self.fold_binary_args(args.into()) {
                BinaryArgs::VV(lhs, rhs) => jump!(lhs < rhs),
                args => FoldResult::Some(hir::InstructionKind::Branch {
                    kind: BranchKind::Lt {
                        args: args.try_into().unwrap(),
                    },
                    lbl_false,
                    lbl_true,
                }),
            },
        }
    }

    fn visit_jump(&mut self, kind: JumpKind, lbl: KeyWithRange) -> Self::Output {
        macro_rules! jump_if {
            (un $cond:ident, $expr:expr) => {
                match self.reg($cond) {
                    Some($cond) => jump_if!($expr),
                    None => FoldResult::Keep,
                }
            };
            (bin $kind:ident $args:ident, |$lhs:ident, $rhs:ident| $expr:expr) => {
                match self.fold_binary_args($args.into()) {
                    BinaryArgs::VV($lhs, $rhs) => jump_if!($expr),
                    args => FoldResult::Some(hir::InstructionKind::Jump {
                        kind: JumpKind::$kind {
                            args: args.try_into().unwrap(),
                        },
                        lbl,
                    }),
                }
            };
            ($cond:expr) => {
                if $cond {
                    FoldResult::Some(hir::InstructionKind::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    FoldResult::None
                }
            };
        }

        match kind {
            JumpKind::Unconditional => FoldResult::Keep,
            JumpKind::Ez { cond } => jump_if!(un cond, cond == 0),
            JumpKind::Nz { cond } => jump_if!(un cond, cond != 0),
            JumpKind::Lt { args } => jump_if!(bin Lt args, |lhs, rhs| lhs < rhs),
            JumpKind::Le { args } => jump_if!(bin Le args, |lhs, rhs| lhs <= rhs),
            JumpKind::Eq { args } => jump_if!(bin Eq args, |lhs, rhs| lhs == rhs),
            JumpKind::Ne { args } => jump_if!(bin Ne args, |lhs, rhs| lhs != rhs),
        }
    }

    fn visit_call(&mut self, to: Reg, _func: KeyWithRange, args: &[Reg]) -> Self::Output {
        for &arg in args {
            self.assign_reg(arg);
        }
        self.registers[to].reset();
        FoldResult::Keep
    }

    fn visit_addr(&mut self, to: Reg, _lbl: KeyWithRange) -> Self::Output {
        self.registers[to].reset();
        FoldResult::Keep
    }

    fn visit_djump(&mut self, kind: JumpKind, lbl: Reg) -> Self::Output {
        macro_rules! jump_if {
            (un $cond:ident, $expr:expr) => {
                match self.reg($cond) {
                    Some($cond) => jump_if!($expr),
                    None => FoldResult::Keep,
                }
            };
            (bin $kind:ident $args:ident, |$lhs:ident, $rhs:ident| $expr:expr) => {
                match self.fold_binary_args($args.into()) {
                    BinaryArgs::VV($lhs, $rhs) => jump_if!($expr),
                    args => FoldResult::Some(hir::InstructionKind::DJump {
                        kind: JumpKind::$kind {
                            args: args.try_into().unwrap(),
                        },
                        lbl,
                    }),
                }
            };
            ($cond:expr) => {
                if $cond {
                    FoldResult::Some(hir::InstructionKind::DJump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    FoldResult::None
                }
            };
        }

        match kind {
            JumpKind::Unconditional => FoldResult::Keep,
            JumpKind::Ez { cond } => jump_if!(un cond, cond == 0),
            JumpKind::Nz { cond } => jump_if!(un cond, cond != 0),
            JumpKind::Lt { args } => jump_if!(bin Lt args, |lhs, rhs| lhs < rhs),
            JumpKind::Le { args } => jump_if!(bin Le args, |lhs, rhs| lhs <= rhs),
            JumpKind::Eq { args } => jump_if!(bin Eq args, |lhs, rhs| lhs == rhs),
            JumpKind::Ne { args } => jump_if!(bin Ne args, |lhs, rhs| lhs != rhs),
        }
    }

    fn visit_dcall(&mut self, to: Reg, _func: Reg, args: &[Reg]) -> Self::Output {
        for &arg in args {
            self.assign_reg(arg);
        }
        self.registers[to].reset();
        FoldResult::Keep
    }

    fn visit_ret(&mut self, arg: hir::UnaryArg) -> Self::Output {
        let arg = self.fold_unary_arg(arg.into()).into();
        FoldResult::Some(hir::InstructionKind::Ret { arg })
    }

    fn visit_int(&mut self, to: Reg, int: Int) -> Self::Output {
        self.registers[to].set(Value::int(int));
        FoldResult::None
    }

    fn visit_str(&mut self, to: Reg, str: KeyWithRange) -> Self::Output {
        let text = self.ctx.interner.lookup(str.value);
        let num_chars = text.chars().count();

        let gc = &mut self.ctx.gc;
        let ptr = gc.alloc(num_chars.try_into().unwrap());
        for (i, ch) in text.chars().enumerate() {
            *gc.get_mut(ptr, i) = Value::int(u32::from(ch).into());
        }
        self.registers[to].set(Value::ptr(ptr));
        FoldResult::None
    }

    fn visit_binary(&mut self, op: BinaryOp, to: Reg, args: hir::BinaryArgs) -> Self::Output {
        match self.fold_binary_args(args.into()) {
            BinaryArgs::VV(lhs, rhs) => {
                assert!(
                    lhs.is_int() && rhs.is_int(),
                    "pointers cannot be used in arithmetic"
                );
                self.registers[to].set(match op {
                    BinaryOp::Add => lhs + rhs,
                    BinaryOp::Sub => lhs - rhs,
                    BinaryOp::Mul => lhs * rhs,
                    BinaryOp::Div => lhs / rhs,
                    BinaryOp::Mod => lhs % rhs,
                });
                FoldResult::None
            }
            args => {
                self.registers[to].reset();
                FoldResult::Some(hir::InstructionKind::Binary {
                    op,
                    to,
                    args: args.try_into().unwrap(),
                })
            }
        }
    }

    fn visit_unary(&mut self, op: UnaryOp, to: Reg, arg: Reg) -> Self::Output {
        if let Some(value) = self.registers[arg].get() {
            assert!(value.is_int(), "pointers cannot be used in arithmetic");
            self.registers[to].set(match op {
                UnaryOp::Neg => -value,
            });
            FoldResult::None
        } else {
            self.registers[to].reset();
            FoldResult::Keep
        }
    }

    fn visit_incr(&mut self, reg: Reg) -> Self::Output {
        if let Some(value) = self.registers[reg].get() {
            assert!(value.is_int(), "pointers cannot be used in arithmetic");
            self.registers[reg].set(value + 1);
            FoldResult::None
        } else {
            FoldResult::Keep
        }
    }

    fn visit_decr(&mut self, reg: Reg) -> Self::Output {
        if let Some(value) = self.registers[reg].get() {
            assert!(value.is_int(), "pointers cannot be used in arithmetic");
            self.registers[reg].set(value - 1);
            FoldResult::None
        } else {
            FoldResult::Keep
        }
    }

    fn visit_arr(&mut self, to: Reg, len: hir::UnaryArg) -> Self::Output {
        let len = self.fold_unary_arg(len.into()).into();
        self.registers[to].reset();
        FoldResult::Some(hir::InstructionKind::Arr { to, len })
    }

    fn visit_get(&mut self, to: Reg, arr: Reg, idx: hir::UnaryArg) -> Self::Output {
        let idx = self.fold_unary_arg(idx.into()).into();
        self.registers[to].reset();
        FoldResult::Some(hir::InstructionKind::Get { to, arr, idx })
    }

    fn visit_set(&mut self, arr: Reg, idx_and_val: hir::SetArgs) -> Self::Output {
        FoldResult::Some(hir::InstructionKind::Set {
            arr,
            idx_and_val: self.fold_binary_args(idx_and_val.into()).into(),
        })
    }

    fn visit_len(&mut self, to: Reg, _arr: Reg) -> Self::Output {
        self.registers[to].reset();
        FoldResult::Keep
    }

    fn visit_type(&mut self, to: Reg, obj: Reg) -> Self::Output {
        if let Some(val) = self.registers[obj].get() {
            let int = (val.kind() as u8).into();
            self.registers[to].set(Value::int(int));
            FoldResult::None
        } else {
            self.registers[to].reset();
            FoldResult::Keep
        }
    }

    fn visit_putc(&mut self, arg: hir::UnaryArg) -> Self::Output {
        FoldResult::Some(hir::InstructionKind::Putc {
            arg: self.fold_unary_arg(arg.into()).into(),
        })
    }
}
