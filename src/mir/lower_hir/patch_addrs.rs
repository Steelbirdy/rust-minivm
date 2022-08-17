use crate::{
    common::Key,
    mir::{lower_hir::LoweringContext, Instruction},
};

pub(super) struct PatchAddrs<'ctx, 'a> {
    ctx: &'a mut LoweringContext<'ctx>,
    lbls: &'a [Key],
}

impl<'ctx, 'a> PatchAddrs<'ctx, 'a> {
    pub(super) fn new(ctx: &'a mut LoweringContext<'ctx>) -> Self {
        Self { ctx, lbls: &[] }
    }

    pub(super) fn patch(&mut self, ref_index: usize, lbls: &'a [Key]) {
        use Instruction::*;

        self.lbls = lbls;
        let mut i = 0;
        #[allow(non_snake_case)]
        macro_rules! L {
            ($($index:literal -> $field:ident),+) => {{
                $(
                i += 1;
                *$field = self.ctx.label_locs[&lbls[$index]];
                )+
            }};
        }

        match &mut self.ctx.out[ref_index] {
            Func { end, .. } => L!(0 -> end),
            RegA { addr, .. } => L!(0 -> addr),
            BranchNz { addr_f, addr_t, .. }
            | BranchEqRR { addr_f, addr_t, .. }
            | BranchEqIR { addr_f, addr_t, .. }
            | BranchLtRR { addr_f, addr_t, .. }
            | BranchLtRI { addr_f, addr_t, .. }
            | BranchLtIR { addr_f, addr_t, .. } => L!(0 -> addr_f, 1 -> addr_t),
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
            | CallL { addr, .. } => L!(0 -> addr),
            x => unreachable!("{x:?}"),
        }

        assert_eq!(i, lbls.len(), "{:?}", self.ctx.out[ref_index]);
    }
}
