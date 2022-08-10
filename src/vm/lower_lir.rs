use crate::{
    common::{Addr, BytecodeBuilder},
    lir::Instruction,
    vm::Opcode,
};

type TypedBcIdx = usize;
type UntypedBcIdx = usize;

#[must_use]
pub fn lower_lir(instructions: &[Instruction]) -> Box<[u8]> {
    let mut ctx = LoweringContext::new(instructions);
    ctx.finish();
    ctx.patch_references();
    ctx.builder.into_inner()
}

struct LoweringContext<'a> {
    instructions: &'a [Instruction],
    builder: BytecodeBuilder,
    instr_starts: Vec<UntypedBcIdx>,
    instr_refs: Vec<(UntypedBcIdx, TypedBcIdx)>,
}

impl<'a> LoweringContext<'a> {
    fn new(instructions: &'a [Instruction]) -> Self {
        Self {
            instructions,
            builder: BytecodeBuilder::default(),
            instr_starts: Vec::with_capacity(instructions.len() + 1),
            instr_refs: Vec::new(),
        }
    }

    fn addr(&mut self, typed_idx: TypedBcIdx) {
        self.instr_refs.push((self.builder.len(), typed_idx));
        self.builder.push(Addr::MAX);
    }

    fn finish(&mut self) {
        self.instr_starts.push(0);
        for instr in self.instructions {
            self.lower_instruction(instr);
            self.instr_starts.push(self.builder.len());
        }
    }

    fn patch_references(&mut self) {
        for (instr, index) in self.instr_refs.drain(..) {
            let new_index = self.instr_starts[index].try_into().unwrap();
            self.builder.write_at::<Addr>(instr, new_index);
        }
    }

    fn lower_instruction(&mut self, instr: &Instruction) {
        use Instruction::*;

        macro_rules! write {
            ($($value:expr),+) => {{
                $(
                self.builder.push($value);
                )+
            }};
        }

        macro_rules! addr {
            ($($addr:ident),+) => {{
                $(
                self.addr($addr);
                )+
            }};
        }

        match instr.clone() {
            Exit => write!(Opcode::Exit),
            Func { end, num_regs } => {
                write!(Opcode::Func);
                addr!(end);
                write!(num_regs);
            }
            RegA { addr, to } => {
                write!(Opcode::RegA);
                addr!(addr);
                write!(to);
            }
            RegR { from, to } => write!(Opcode::RegR, from, to),
            BranchNz {
                cond,
                addr_f,
                addr_t,
            } => {
                write!(Opcode::BranchNz, cond);
                addr!(addr_f, addr_t);
            }
            BranchEqRR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(Opcode::BranchEqRR, lhs, rhs);
                addr!(addr_f, addr_t);
            }
            BranchEqIR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(Opcode::BranchEqIR, lhs, rhs);
                addr!(addr_f, addr_t);
            }
            BranchLtRR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(Opcode::BranchLtRR, lhs, rhs);
                addr!(addr_f, addr_t);
            }
            BranchLtRI {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(Opcode::BranchLtRI, lhs, rhs);
                addr!(addr_f, addr_t);
            }
            BranchLtIR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(Opcode::BranchLtIR, lhs, rhs);
                addr!(addr_f, addr_t);
            }
            Jump { addr } => {
                write!(Opcode::Jump);
                addr!(addr);
            }
            JumpEz { cond, addr } => {
                write!(Opcode::JumpEz, cond);
                addr!(addr);
            }
            JumpNz { cond, addr } => {
                write!(Opcode::JumpNz, cond);
                addr!(addr);
            }
            JumpLtRR { lhs, rhs, addr } => {
                write!(Opcode::JumpLtRR, lhs, rhs);
                addr!(addr);
            }
            JumpLtRI { lhs, rhs, addr } => {
                write!(Opcode::JumpLtRI, lhs, rhs);
                addr!(addr);
            }
            JumpLtIR { lhs, rhs, addr } => {
                write!(Opcode::JumpLtIR, lhs, rhs);
                addr!(addr);
            }
            JumpLeRR { lhs, rhs, addr } => {
                write!(Opcode::JumpLeRR, lhs, rhs);
                addr!(addr);
            }
            JumpLeRI { lhs, rhs, addr } => {
                write!(Opcode::JumpLeRI, lhs, rhs);
                addr!(addr);
            }
            JumpLeIR { lhs, rhs, addr } => {
                write!(Opcode::JumpLeIR, lhs, rhs);
                addr!(addr);
            }
            JumpEqRR { lhs, rhs, addr } => {
                write!(Opcode::JumpEqRR, lhs, rhs);
                addr!(addr);
            }
            JumpEqIR { lhs, rhs, addr } => {
                write!(Opcode::JumpEqIR, lhs, rhs);
                addr!(addr);
            }
            JumpNeRR { lhs, rhs, addr } => {
                write!(Opcode::JumpNeRR, lhs, rhs);
                addr!(addr);
            }
            JumpNeIR { lhs, rhs, addr } => {
                write!(Opcode::JumpNeIR, lhs, rhs);
                addr!(addr);
            }
            Call0 { addr, num_regs, to } => {
                write!(Opcode::Call0);
                addr!(addr);
                write!(num_regs, to);
            }
            Call1 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call1);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call2 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call2);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call3 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call3);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call4 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call4);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call5 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call5);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call6 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call6);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call7 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call7);
                addr!(addr);
                write!(args, num_regs, to);
            }
            Call8 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::Call8);
                addr!(addr);
                write!(args, num_regs, to);
            }
            CallL {
                addr,
                num_args,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::CallL);
                addr!(addr);
                write!(num_args);
                for arg in args {
                    write!(arg);
                }
                write!(num_regs, to);
            }
            DJump { addr } => write!(Opcode::DJump, addr),
            DCall0 { addr, num_regs, to } => {
                write!(Opcode::DCall0, addr, num_regs, to);
            }
            DCall1 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall1, addr, args, num_regs, to);
            }
            DCall2 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall2, addr, args, num_regs, to);
            }
            DCall3 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall3, addr, args, num_regs, to);
            }
            DCall4 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall4, addr, args, num_regs, to);
            }
            DCall5 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall5, addr, args, num_regs, to);
            }
            DCall6 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall6, addr, args, num_regs, to);
            }
            DCall7 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall7, addr, args, num_regs, to);
            }
            DCall8 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCall8, addr, args, num_regs, to);
            }
            DCallL {
                addr,
                num_args,
                args,
                num_regs,
                to,
            } => {
                write!(Opcode::DCallL, addr, num_args);
                for arg in args {
                    write!(arg);
                }
                write!(num_regs, to);
            }
            RetR { reg } => write!(Opcode::RetR, reg),
            RetV { val } => write!(Opcode::RetV, val),
            Value { val, to } => write!(Opcode::Value, val, to),
            AddRR { lhs, rhs, to } => write!(Opcode::AddRR, lhs, rhs, to),
            AddIR { lhs, rhs, to } => write!(Opcode::AddIR, lhs, rhs, to),
            SubRR { lhs, rhs, to } => write!(Opcode::SubRR, lhs, rhs, to),
            SubRI { lhs, rhs, to } => write!(Opcode::SubRI, lhs, rhs, to),
            SubIR { lhs, rhs, to } => write!(Opcode::SubIR, lhs, rhs, to),
            MulRR { lhs, rhs, to } => write!(Opcode::MulRR, lhs, rhs, to),
            MulIR { lhs, rhs, to } => write!(Opcode::MulIR, lhs, rhs, to),
            DivRR { lhs, rhs, to } => write!(Opcode::DivRR, lhs, rhs, to),
            DivRI { lhs, rhs, to } => write!(Opcode::DivRI, lhs, rhs, to),
            DivIR { lhs, rhs, to } => write!(Opcode::DivIR, lhs, rhs, to),
            ModRR { lhs, rhs, to } => write!(Opcode::ModRR, lhs, rhs, to),
            ModRI { lhs, rhs, to } => write!(Opcode::ModRI, lhs, rhs, to),
            ModIR { lhs, rhs, to } => write!(Opcode::ModIR, lhs, rhs, to),
            Neg { reg, to } => write!(Opcode::Neg, reg, to),
            Incr { reg } => write!(Opcode::Incr, reg),
            Decr { reg } => write!(Opcode::Decr, reg),
            ArrR { len, to } => write!(Opcode::ArrR, len, to),
            ArrI { len, to } => write!(Opcode::ArrI, len, to),
            GetR { arr, idx, to } => write!(Opcode::GetR, arr, idx, to),
            GetI { arr, idx, to } => write!(Opcode::GetI, arr, idx, to),
            SetII { arr, idx, val } => write!(Opcode::SetII, arr, idx, val),
            SetRR { arr, idx, val } => write!(Opcode::SetRR, arr, idx, val),
            SetRI { arr, idx, val } => write!(Opcode::SetRI, arr, idx, val),
            SetIR { arr, idx, val } => write!(Opcode::SetIR, arr, idx, val),
            Len { arr, to } => write!(Opcode::Len, arr, to),
            Type { obj, to } => write!(Opcode::Type, obj, to),
            PutcR { ch } => write!(Opcode::PutcR, ch),
            PutcI { ch } => write!(Opcode::PutcI, ch),
        }
    }
}
