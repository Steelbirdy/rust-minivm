mod fold_constants;
mod patch_addrs;
mod registers;

use fold_constants::FoldConstants;
use patch_addrs::PatchAddrs;

use crate::{
    common::{Int, Interner, Key, List, Reg},
    hir::{self, BinaryOp, BranchKind, Function, JumpKind, KeyWithRange, Program, UnaryOp},
    mir::Instruction,
    vm::{Gc, Ptr, Value},
};
use rustc_hash::FxHashMap;

pub fn lower_hir(program: &Program, interner: &Interner, gc: &mut Gc) -> Vec<Instruction> {
    let mut ctx = LoweringContext::new(program, interner, gc);
    ctx.finish();
    ctx.out
}

enum UnaryArg {
    R(Reg),
    V(Value),
}

impl From<hir::UnaryArg> for UnaryArg {
    fn from(arg: hir::UnaryArg) -> Self {
        match arg {
            hir::UnaryArg::R(reg) => Self::R(reg),
            hir::UnaryArg::I(int) => Self::V(Value::int_unchecked(int)),
        }
    }
}

impl From<UnaryArg> for hir::UnaryArg {
    fn from(arg: UnaryArg) -> Self {
        match arg {
            UnaryArg::R(reg) => Self::R(reg),
            UnaryArg::V(val) => Self::I(val.as_int()),
        }
    }
}

enum BinaryArgs {
    RR(Reg, Reg),
    RV(Reg, Value),
    VR(Value, Reg),
    VV(Value, Value),
}

impl From<hir::BinaryArgs> for BinaryArgs {
    fn from(args: hir::BinaryArgs) -> Self {
        match args {
            hir::BinaryArgs::RR(lhs, rhs) => Self::RR(lhs, rhs),
            hir::BinaryArgs::RI(lhs, rhs) => Self::RV(lhs, Value::int_unchecked(rhs)),
            hir::BinaryArgs::IR(lhs, rhs) => Self::VR(Value::int_unchecked(lhs), rhs),
        }
    }
}

impl From<hir::SetArgs> for BinaryArgs {
    fn from(args: hir::SetArgs) -> Self {
        match args {
            hir::SetArgs::RR(lhs, rhs) => Self::RR(lhs, rhs),
            hir::SetArgs::RI(lhs, rhs) => Self::RV(lhs, Value::int_unchecked(rhs)),
            hir::SetArgs::IR(lhs, rhs) => Self::VR(Value::int_unchecked(lhs), rhs),
            hir::SetArgs::II(lhs, rhs) => {
                Self::VV(Value::int_unchecked(lhs), Value::int_unchecked(rhs))
            }
        }
    }
}

impl From<BinaryArgs> for hir::SetArgs {
    fn from(args: BinaryArgs) -> Self {
        match args {
            BinaryArgs::RR(lhs, rhs) => Self::RR(lhs, rhs),
            BinaryArgs::RV(lhs, rhs) => Self::RI(lhs, rhs.as_int()),
            BinaryArgs::VR(lhs, rhs) => Self::IR(lhs.as_int(), rhs),
            BinaryArgs::VV(lhs, rhs) => Self::II(lhs.as_int(), rhs.as_int()),
        }
    }
}

impl TryFrom<BinaryArgs> for hir::BinaryArgs {
    type Error = ();

    fn try_from(args: BinaryArgs) -> Result<Self, Self::Error> {
        Ok(match args {
            BinaryArgs::RR(lhs, rhs) => Self::RR(lhs, rhs),
            BinaryArgs::RV(lhs, rhs) => Self::RI(lhs, rhs.as_int()),
            BinaryArgs::VR(lhs, rhs) => Self::IR(lhs.as_int(), rhs),
            BinaryArgs::VV(..) => return Err(()),
        })
    }
}

pub struct LoweringContext<'a> {
    program: &'a Program,
    interner: &'a Interner,
    gc: &'a mut Gc,
    out: Vec<Instruction>,
    label_locs: FxHashMap<Key, usize>,
    label_refs: FxHashMap<usize, List<Key>>,
}

struct LowerInstruction<'ctx, 'a> {
    ctx: &'a mut LoweringContext<'ctx>,
    func: &'a Function,
}

impl<'a> LoweringContext<'a> {
    pub(in crate::mir) fn new(
        program: &'a Program,
        interner: &'a Interner,
        gc: &'a mut Gc,
    ) -> Self {
        Self {
            program,
            interner,
            gc,
            out: Vec::new(),
            label_locs: FxHashMap::default(),
            label_refs: FxHashMap::default(),
        }
    }

    fn finish(&mut self) {
        let addr = self.addr(Key::entry_point());
        self.push(Instruction::Jump { addr });

        let mut functions: List<_> = self.program.functions.values().collect();
        functions.sort_unstable_by_key(|func| func.name.range.start());

        for func in functions {
            self.lower_function(func);
        }

        self.patch_addrs();
    }

    fn lower_function(&mut self, func: &Function) {
        let start_idx = self.out.len();
        self.out.push(Instruction::Func {
            end: 0,
            num_regs: func.regs_used,
        });
        self.label_locs.insert(func.name.value, self.out.len());

        let instructions = self.fold_constants(func);

        let mut lower_instr = LowerInstruction::new(self, func);
        for instr in instructions {
            lower_instr.lower(&instr);
        }

        let end_idx = self.out.len();
        match &mut self.out[start_idx] {
            Instruction::Func { end, .. } => {
                *end = end_idx;
            }
            _ => unreachable!(),
        }
    }

    fn fold_constants(&mut self, func: &Function) -> Vec<hir::InstructionKind> {
        FoldConstants::new(self).fold(func)
    }

    fn patch_addrs(&mut self) {
        let label_refs: Vec<_> = self.label_refs.drain().collect();
        let mut patcher = PatchAddrs::new(self);
        for (ref_idx, lbls) in label_refs.iter() {
            patcher.patch(*ref_idx, lbls);
        }
    }

    fn addr(&mut self, key: Key) -> usize {
        self.label_refs.entry(self.out.len()).or_default().push(key);
        usize::MAX
    }

    fn push(&mut self, instr: Instruction) {
        self.out.push(instr);
    }
}

impl<'ctx, 'a> LowerInstruction<'ctx, 'a> {
    fn new(ctx: &'a mut LoweringContext<'ctx>, func: &'a Function) -> Self {
        Self { ctx, func }
    }

    fn lower(&mut self, instr: &hir::InstructionKind) {
        hir::Visit::visit(self, &instr);
    }

    fn addr(&mut self, key: Key) -> usize {
        self.ctx.addr(key)
    }

    fn push(&mut self, instr: Instruction) {
        self.ctx.push(instr);
    }
}

impl hir::Visit for LowerInstruction<'_, '_> {
    type Output = ();

    fn unimplemented(&mut self) -> Self::Output {
        ()
    }

    fn visit_exit(&mut self) -> Self::Output {
        self.push(Instruction::Exit);
    }

    fn visit_label(&mut self, name: KeyWithRange) -> Self::Output {
        self.ctx.label_locs.insert(name.value, self.ctx.out.len());
    }

    fn visit_reg(&mut self, to: Reg, from: Reg) -> Self::Output {
        self.push(Instruction::RegR { from, to });
    }

    fn visit_branch(
        &mut self,
        kind: BranchKind,
        lbl_false: KeyWithRange,
        lbl_true: KeyWithRange,
    ) -> Self::Output {
        macro_rules! instr {
            ($kind:ident, $($cond:ident),+) => {{
                let addr_f = self.addr(lbl_false.value);
                let addr_t = self.addr(lbl_true.value);
                self.push(Instruction::$kind { $($cond,)+ addr_f, addr_t });
            }};
        }

        match kind {
            BranchKind::Nz { cond } => instr!(BranchNz, cond),
            BranchKind::Eq { args } => match BinaryArgs::from(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(BranchEqRR, lhs, rhs),
                BinaryArgs::RV(rhs, lhs) | BinaryArgs::VR(lhs, rhs) => {
                    let lhs = lhs.as_int_unchecked();
                    instr!(BranchEqIR, lhs, rhs);
                }
                BinaryArgs::VV(lhs, rhs) => {
                    let lbl = if lhs == rhs { lbl_true } else { lbl_false };
                    let addr = self.addr(lbl.value);
                    self.push(Instruction::Jump { addr });
                }
            },
            BranchKind::Lt { args } => match BinaryArgs::from(args) {
                BinaryArgs::RR(lhs, rhs) => instr!(BranchLtRR, lhs, rhs),
                BinaryArgs::RV(lhs, rhs) => {
                    let rhs = rhs.as_int_unchecked();
                    instr!(BranchLtRI, lhs, rhs);
                }
                BinaryArgs::VR(lhs, rhs) => {
                    let lhs = lhs.as_int_unchecked();
                    instr!(BranchLtIR, lhs, rhs);
                }
                BinaryArgs::VV(lhs, rhs) => {
                    let lbl = if lhs < rhs { lbl_true } else { lbl_false };
                    let addr = self.addr(lbl.value);
                    self.push(Instruction::Jump { addr });
                }
            },
        }
    }

    fn visit_jump(&mut self, kind: JumpKind, lbl: KeyWithRange) -> Self::Output {
        macro_rules! instr {
            ($kind:ident $(, $cond:ident)*) => {{
                let addr = self.addr(lbl.value);
                self.push(Instruction::$kind { $($cond,)* addr });
            }};
        }

        macro_rules! binary {
            (|$args:ident| $op:tt; $rr:ident, $ir:ident $(, $ri:ident)?) => {
                match BinaryArgs::from($args) {
                    BinaryArgs::RR(lhs, rhs) => instr!($rr, lhs, rhs),
                    BinaryArgs::VR(lhs, rhs) => instr!($ir, lhs, rhs),
                    BinaryArgs::RV(lhs, rhs) => binary!(@ri_body |lhs, rhs| $ir $($ri)?),
                    BinaryArgs::VV(lhs, rhs) => if lhs $op rhs { instr!(Jump); },
                }
            };
            (@ri_body |$lhs:ident, $rhs:ident| $ir:ident $ri:ident) => {
                instr!($ri, $lhs, $rhs)
            };
            (@ri_body |$lhs:ident, $rhs:ident| $ir:ident) => {{
                let addr = self.addr(lbl.value);
                self.push(Instruction::$ir { lhs: $rhs, rhs: $lhs, addr });
            }};
        }

        match kind {
            JumpKind::Unconditional => instr!(Jump),
            JumpKind::Ez { cond } => instr!(JumpEz, cond),
            JumpKind::Nz { cond } => instr!(JumpNz, cond),
            JumpKind::Eq { args } => binary!(|args| ==; JumpEqRR, JumpEqIR),
            JumpKind::Ne { args } => binary!(|args| !=; JumpNeRR, JumpNeIR),
            JumpKind::Lt { args } => binary!(|args| <; JumpLtRR, JumpLtIR, JumpLtRI),
            JumpKind::Le { args } => binary!(|args| <=; JumpLeRR, JumpLeIR, JumpLeRI),
        }
    }

    fn visit_call(&mut self, to: Reg, func: KeyWithRange, args: &[Reg]) -> Self::Output {
        let addr = self.addr(func.value);
        self.push(Instruction::call(addr, args, self.func.regs_used, to));
    }

    fn visit_addr(&mut self, to: Reg, lbl: KeyWithRange) -> Self::Output {
        let addr = self.addr(lbl.value);
        self.push(Instruction::RegA { addr, to });
    }

    fn visit_djump(&mut self, kind: JumpKind, lbl: Reg) -> Self::Output {
        match kind {
            JumpKind::Unconditional => self.push(Instruction::DJump { addr: lbl }),
            _ => panic!("currently only `jump` can be used with a dynamic jump target"),
        }
    }

    fn visit_dcall(&mut self, to: Reg, func: Reg, args: &[Reg]) -> Self::Output {
        self.push(Instruction::dcall(func, args, self.func.regs_used, to));
    }

    fn visit_ret(&mut self, arg: hir::UnaryArg) -> Self::Output {
        match UnaryArg::from(arg) {
            UnaryArg::R(reg) => self.push(Instruction::RetR { reg }),
            UnaryArg::V(val) => self.push(Instruction::RetV { val }),
        }
    }

    fn visit_int(&mut self, to: Reg, int: Int) -> Self::Output {
        self.push(Instruction::Value {
            val: Value::int_unchecked(int),
            to,
        });
    }

    fn visit_ptr(&mut self, to: Reg, ptr: Ptr) -> Self::Output {
        self.push(Instruction::Value {
            val: Value::ptr_unchecked(ptr),
            to,
        });
    }

    fn visit_binary(&mut self, op: BinaryOp, to: Reg, args: hir::BinaryArgs) -> Self::Output {
        macro_rules! binary {
            ($rr:ident, $vr:ident $(, $rv:ident)?) => {
                match BinaryArgs::from(args) {
                    BinaryArgs::RR(lhs, rhs) => self.push(Instruction::$rr { lhs, rhs, to }),
                    BinaryArgs::VR(lhs, rhs) => self.push(Instruction::$vr { lhs: lhs.as_int(), rhs, to }),
                    BinaryArgs::RV(lhs, rhs) => binary!(@rv_body |lhs, rhs| $vr $($rv)?),
                    BinaryArgs::VV(..) => unreachable!(),
                }
            };
            (@rv_body |$lhs:ident, $rhs:ident| $vr:ident $rv:ident) => {
                self.push(Instruction::$rv { lhs: $lhs, rhs: $rhs.as_int(), to })
            };
            (@rv_body |$lhs:ident, $rhs:ident| $vr:ident) => {
                self.push(Instruction::$vr { lhs: $rhs.as_int(), rhs: $lhs, to })
            };
        }

        match op {
            BinaryOp::Add => binary!(AddRR, AddIR),
            BinaryOp::Sub => binary!(SubRR, SubIR, SubRI),
            BinaryOp::Mul => binary!(MulRR, MulIR),
            BinaryOp::Div => binary!(DivRR, DivIR, DivRI),
            BinaryOp::Mod => binary!(ModRR, ModIR, ModRI),
        }
    }

    fn visit_unary(&mut self, op: UnaryOp, to: Reg, arg: Reg) -> Self::Output {
        match op {
            UnaryOp::Neg => self.push(Instruction::Neg { reg: arg, to }),
        }
    }

    fn visit_incr(&mut self, reg: Reg) -> Self::Output {
        self.push(Instruction::Incr { reg });
    }

    fn visit_decr(&mut self, reg: Reg) -> Self::Output {
        self.push(Instruction::Decr { reg });
    }

    fn visit_arr(&mut self, to: Reg, len: hir::UnaryArg) -> Self::Output {
        match UnaryArg::from(len) {
            UnaryArg::R(len) => self.push(Instruction::ArrR { len, to }),
            UnaryArg::V(len) => self.push(Instruction::ArrI {
                len: len.as_int(),
                to,
            }),
        }
    }

    fn visit_get(&mut self, to: Reg, arr: Reg, idx: hir::UnaryArg) -> Self::Output {
        match UnaryArg::from(idx) {
            UnaryArg::R(idx) => self.push(Instruction::GetR { arr, idx, to }),
            UnaryArg::V(idx) => self.push(Instruction::GetI {
                arr,
                idx: idx.as_int(),
                to,
            }),
        }
    }

    fn visit_set(&mut self, arr: Reg, idx_and_val: hir::SetArgs) -> Self::Output {
        match BinaryArgs::from(idx_and_val) {
            BinaryArgs::RR(idx, val) => self.push(Instruction::SetRR { arr, idx, val }),
            BinaryArgs::RV(idx, val) => self.push(Instruction::SetRI { arr, idx, val }),
            BinaryArgs::VR(idx, val) => self.push(Instruction::SetIR {
                arr,
                idx: idx.as_int(),
                val,
            }),
            BinaryArgs::VV(idx, val) => self.push(Instruction::SetII {
                arr,
                idx: idx.as_int(),
                val,
            }),
        }
    }

    fn visit_len(&mut self, to: Reg, arr: Reg) -> Self::Output {
        self.push(Instruction::Len { arr, to });
    }

    fn visit_type(&mut self, to: Reg, obj: Reg) -> Self::Output {
        self.push(Instruction::Type { obj, to });
    }

    fn visit_putc(&mut self, arg: hir::UnaryArg) -> Self::Output {
        match UnaryArg::from(arg) {
            UnaryArg::R(ch) => self.push(Instruction::PutcR { ch }),
            UnaryArg::V(ch) => {
                let ch = u32::try_from(ch.as_int()).unwrap();
                self.push(Instruction::PutcI { ch });
            }
        }
    }
}
