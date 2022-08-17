use crate::{
    common::{Addr, BytecodeBuilder, Int, Reg},
    mir::{self, Instruction},
    vm::{Opcode, Value},
};

type MirIndex = usize;
type ByteIndex = usize;

#[must_use]
pub fn lower_mir(instructions: &[Instruction]) -> Box<[u8]> {
    let mut ctx = LoweringContext::new();
    ctx.lower(instructions);
    ctx.patch_references();
    ctx.builder.into_inner()
}

struct LoweringContext {
    builder: BytecodeBuilder,
    instr_starts: Vec<ByteIndex>,
    instr_refs: Vec<(ByteIndex, MirIndex)>,
}

impl LoweringContext {
    fn new() -> Self {
        Self {
            builder: BytecodeBuilder::default(),
            instr_starts: Vec::new(),
            instr_refs: Vec::new(),
        }
    }

    fn lower(&mut self, instructions: &[Instruction]) {
        self.instr_starts.push(0);
        let mut lowerer = LowerInstruction::new(&mut self.builder, &mut self.instr_refs);
        for instr in instructions {
            lowerer.lower(instr);
            self.instr_starts.push(lowerer.builder.len());
        }
    }

    fn patch_references(&mut self) {
        for &(byte_index, mir_index) in &self.instr_refs {
            let new_index = self.instr_starts[mir_index].try_into().unwrap();
            self.builder.write_at::<Addr>(byte_index, new_index);
        }
    }
}

struct LowerInstruction<'a> {
    builder: &'a mut BytecodeBuilder,
    instr_refs: &'a mut Vec<(ByteIndex, MirIndex)>,
}

impl<'a> LowerInstruction<'a> {
    fn new(
        builder: &'a mut BytecodeBuilder,
        instr_refs: &'a mut Vec<(ByteIndex, MirIndex)>,
    ) -> Self {
        Self {
            builder,
            instr_refs,
        }
    }

    fn lower(&mut self, instr: &Instruction) {
        mir::Visit::visit(self, instr);
    }

    fn addr(&mut self, mir_idx: MirIndex) {
        self.instr_refs.push((self.builder.len(), mir_idx));
        self.builder.push(Addr::MAX);
    }
}

macro_rules! push {
    ($this:ident; $($([$at:tt])? $val:path),+) => {{
        $(
        push!(@interpret $this $($at)? $val);
        )+
    }};
    (@interpret $this:ident @ $val:expr) => {
        $this.addr($val);
    };
    (@interpret $this:ident * $val:expr) => {
        $this.builder.push(*$val);
    };
    (@interpret $this:ident $val:expr) => {
        $this.builder.push($val);
    };
}

impl mir::Visit for LowerInstruction<'_> {
    type Output = ();

    fn unimplemented(&mut self) -> Self::Output {
        ()
    }

    fn visit_exit(&mut self) -> Self::Output {
        push!(self; Opcode::Exit);
    }

    fn visit_func(&mut self, end: usize, num_regs: Reg) -> Self::Output {
        push!(self; Opcode::Func, [@]end, num_regs);
    }

    fn visit_reg_a(&mut self, addr: usize, to: Reg) -> Self::Output {
        push!(self; Opcode::RegA, [@]addr, to);
    }

    fn visit_reg_r(&mut self, from: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::RegR, from, to);
    }

    fn visit_branch_nz(&mut self, cond: Reg, addr_f: usize, addr_t: usize) -> Self::Output {
        push!(self; Opcode::BranchNz, cond, [@]addr_f, [@]addr_t);
    }

    fn visit_branch_eq_rr(
        &mut self,
        lhs: Reg,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    ) -> Self::Output {
        push!(self; Opcode::BranchEqRR, lhs, rhs, [@]addr_f, [@]addr_t);
    }

    fn visit_branch_eq_ir(
        &mut self,
        lhs: Int,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    ) -> Self::Output {
        push!(self; Opcode::BranchEqIR, lhs, rhs, [@]addr_f, [@]addr_t);
    }

    fn visit_branch_lt_rr(
        &mut self,
        lhs: Reg,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    ) -> Self::Output {
        push!(self; Opcode::BranchLtRR, lhs, rhs, [@]addr_f, [@]addr_t);
    }

    fn visit_branch_lt_ri(
        &mut self,
        lhs: Reg,
        rhs: Int,
        addr_f: usize,
        addr_t: usize,
    ) -> Self::Output {
        push!(self; Opcode::BranchLtRI, lhs, rhs, [@]addr_f, [@]addr_t);
    }

    fn visit_branch_lt_ir(
        &mut self,
        lhs: Int,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    ) -> Self::Output {
        push!(self; Opcode::BranchLtIR, lhs, rhs, [@]addr_f, [@]addr_t);
    }

    fn visit_jump(&mut self, addr: usize) -> Self::Output {
        push!(self; Opcode::Jump, [@]addr)
    }

    fn visit_jump_ez(&mut self, cond: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpEz, cond, [@]addr);
    }

    fn visit_jump_nz(&mut self, cond: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpNz, cond, [@]addr);
    }

    fn visit_jump_lt_rr(&mut self, lhs: Reg, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpLtRR, lhs, rhs, [@]addr);
    }

    fn visit_jump_lt_ri(&mut self, lhs: Reg, rhs: Value, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpLtRI, lhs, rhs, [@]addr);
    }

    fn visit_jump_lt_ir(&mut self, lhs: Value, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpLtIR, lhs, rhs, [@]addr);
    }

    fn visit_jump_le_rr(&mut self, lhs: Reg, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpLeRR, lhs, rhs, [@]addr);
    }

    fn visit_jump_le_ri(&mut self, lhs: Reg, rhs: Value, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpLeRI, lhs, rhs, [@]addr);
    }

    fn visit_jump_le_ir(&mut self, lhs: Value, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpLeIR, lhs, rhs, [@]addr);
    }

    fn visit_jump_eq_rr(&mut self, lhs: Reg, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpEqRR, lhs, rhs, [@]addr);
    }

    fn visit_jump_eq_ir(&mut self, lhs: Value, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpEqIR, lhs, rhs, [@]addr);
    }

    fn visit_jump_ne_rr(&mut self, lhs: Reg, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpNeRR, lhs, rhs, [@]addr);
    }

    fn visit_jump_ne_ir(&mut self, lhs: Value, rhs: Reg, addr: usize) -> Self::Output {
        push!(self; Opcode::JumpNeIR, lhs, rhs, [@]addr);
    }

    fn visit_call0(&mut self, addr: usize, num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::Call0, [@]addr, num_regs, to);
    }

    fn visit_call1(
        &mut self,
        addr: usize,
        args: &[Reg; 1],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call1, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call2(
        &mut self,
        addr: usize,
        args: &[Reg; 2],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call2, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call3(
        &mut self,
        addr: usize,
        args: &[Reg; 3],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call3, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call4(
        &mut self,
        addr: usize,
        args: &[Reg; 4],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call4, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call5(
        &mut self,
        addr: usize,
        args: &[Reg; 5],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call5, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call6(
        &mut self,
        addr: usize,
        args: &[Reg; 6],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call6, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call7(
        &mut self,
        addr: usize,
        args: &[Reg; 7],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call7, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call8(
        &mut self,
        addr: usize,
        args: &[Reg; 8],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::Call8, [@]addr, [*]args, num_regs, to);
    }

    fn visit_call_l(
        &mut self,
        addr: usize,
        num_args: u32,
        args: &[Reg],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::CallL, [@]addr, num_args);
        for &arg in args {
            push!(self; arg);
        }
        push!(self; num_regs, to);
    }

    fn visit_djump(&mut self, addr: Reg) -> Self::Output {
        push!(self; Opcode::DJump, addr);
    }

    fn visit_dcall0(&mut self, addr: Reg, num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall0, addr, num_regs, to);
    }

    fn visit_dcall1(&mut self, addr: Reg, args: &[Reg; 1], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall1, addr, [*]args, num_regs, to);
    }

    fn visit_dcall2(&mut self, addr: Reg, args: &[Reg; 2], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall2, addr, [*]args, num_regs, to);
    }

    fn visit_dcall3(&mut self, addr: Reg, args: &[Reg; 3], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall3, addr, [*]args, num_regs, to);
    }

    fn visit_dcall4(&mut self, addr: Reg, args: &[Reg; 4], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall4, addr, [*]args, num_regs, to);
    }

    fn visit_dcall5(&mut self, addr: Reg, args: &[Reg; 5], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall5, addr, [*]args, num_regs, to);
    }

    fn visit_dcall6(&mut self, addr: Reg, args: &[Reg; 6], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall6, addr, [*]args, num_regs, to);
    }

    fn visit_dcall7(&mut self, addr: Reg, args: &[Reg; 7], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall7, addr, [*]args, num_regs, to);
    }

    fn visit_dcall8(&mut self, addr: Reg, args: &[Reg; 8], num_regs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DCall8, addr, [*]args, num_regs, to);
    }

    fn visit_dcall_l(
        &mut self,
        addr: Reg,
        num_args: u32,
        args: &[Reg],
        num_regs: Reg,
        to: Reg,
    ) -> Self::Output {
        push!(self; Opcode::DCallL, addr, num_args);
        for &arg in args {
            push!(self; arg);
        }
        push!(self; num_regs, to);
    }

    fn visit_ret_r(&mut self, reg: Reg) -> Self::Output {
        push!(self; Opcode::RetR, reg);
    }

    fn visit_ret_v(&mut self, val: Value) -> Self::Output {
        push!(self; Opcode::RetV, val);
    }

    fn visit_value(&mut self, val: Value, to: Reg) -> Self::Output {
        push!(self; Opcode::Value, val, to);
    }

    fn visit_add_rr(&mut self, lhs: Reg, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::AddRR, lhs, rhs, to);
    }

    fn visit_add_ir(&mut self, lhs: Int, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::AddIR, lhs, rhs, to);
    }

    fn visit_sub_rr(&mut self, lhs: Reg, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::SubRR, lhs, rhs, to);
    }

    fn visit_sub_ri(&mut self, lhs: Reg, rhs: Int, to: Reg) -> Self::Output {
        push!(self; Opcode::SubRI, lhs, rhs, to);
    }

    fn visit_sub_ir(&mut self, lhs: Int, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::SubIR, lhs, rhs, to);
    }

    fn visit_mul_rr(&mut self, lhs: Reg, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::MulRR, lhs, rhs, to);
    }

    fn visit_mul_ir(&mut self, lhs: Int, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::MulIR, lhs, rhs, to);
    }

    fn visit_div_rr(&mut self, lhs: Reg, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DivRR, lhs, rhs, to);
    }

    fn visit_div_ri(&mut self, lhs: Reg, rhs: Int, to: Reg) -> Self::Output {
        push!(self; Opcode::DivRI, lhs, rhs, to);
    }

    fn visit_div_ir(&mut self, lhs: Int, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::DivIR, lhs, rhs, to);
    }

    fn visit_mod_rr(&mut self, lhs: Reg, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::ModRR, lhs, rhs, to);
    }

    fn visit_mod_ri(&mut self, lhs: Reg, rhs: Int, to: Reg) -> Self::Output {
        push!(self; Opcode::ModRI, lhs, rhs, to);
    }

    fn visit_mod_ir(&mut self, lhs: Int, rhs: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::ModIR, lhs, rhs, to);
    }

    fn visit_neg(&mut self, reg: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::Neg, reg, to);
    }

    fn visit_incr(&mut self, reg: Reg) -> Self::Output {
        push!(self; Opcode::Incr, reg);
    }

    fn visit_decr(&mut self, reg: Reg) -> Self::Output {
        push!(self; Opcode::Decr, reg);
    }

    fn visit_arr_r(&mut self, len: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::ArrR, len, to);
    }

    fn visit_arr_i(&mut self, len: Int, to: Reg) -> Self::Output {
        push!(self; Opcode::ArrI, len, to);
    }

    fn visit_get_r(&mut self, arr: Reg, idx: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::GetR, arr, idx, to);
    }

    fn visit_get_i(&mut self, arr: Reg, idx: Int, to: Reg) -> Self::Output {
        push!(self; Opcode::GetI, arr, idx, to);
    }

    fn visit_set_ii(&mut self, arr: Reg, idx: Int, val: Value) -> Self::Output {
        push!(self; Opcode::SetII, arr, idx, val);
    }

    fn visit_set_rr(&mut self, arr: Reg, idx: Reg, val: Reg) -> Self::Output {
        push!(self; Opcode::SetRR, arr, idx, val);
    }

    fn visit_set_ri(&mut self, arr: Reg, idx: Reg, val: Value) -> Self::Output {
        push!(self; Opcode::SetRI, arr, idx, val);
    }

    fn visit_set_ir(&mut self, arr: Reg, idx: Int, val: Reg) -> Self::Output {
        push!(self; Opcode::SetIR, arr, idx, val);
    }

    fn visit_len(&mut self, arr: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::Len, arr, to);
    }

    fn visit_type(&mut self, obj: Reg, to: Reg) -> Self::Output {
        push!(self; Opcode::Type, obj, to);
    }

    fn visit_putc_r(&mut self, ch: Reg) -> Self::Output {
        push!(self; Opcode::PutcR, ch);
    }

    fn visit_putc_i(&mut self, ch: u32) -> Self::Output {
        push!(self; Opcode::PutcI, ch);
    }
}
