use crate::{
    common::Key,
    hir::{InstructionKind, LabelInfo, Program},
};
use rustc_hash::FxHashMap;

impl Program {
    pub(in crate::hir) fn trace_jumps(&mut self) {
        let keys: Vec<_> = self.functions.keys().copied().collect();
        for func in keys {
            self.trace_jumps_in_function(func);
        }
    }

    fn trace_jumps_in_function(&mut self, name: Key) {
        use InstructionKind::*;

        let func = self.functions.get_mut(&name).unwrap();
        let instrs = &mut func.instructions;

        for instr in &mut *instrs {
            instr.jumps = JumpFlags::EMPTY;
        }

        let mut label_sets = FxHashMap::<Key, JumpFlags>::default();
        let mut add_flag_to_label = |key: Key, flag: JumpFlags| {
            label_sets
                .entry(key)
                .or_insert(JumpFlags::EMPTY)
                .insert(flag);
        };

        for i in 0..instrs.len() {
            let instr = &mut instrs[i];

            match &instr.kind {
                Exit => {
                    instr.jumps.insert(JumpFlags::STOP);
                }
                Branch {
                    lbl_false,
                    lbl_true,
                    ..
                } => {
                    instr.jumps.insert(JumpFlags::OUT);
                    add_flag_to_label(lbl_false.value, JumpFlags::IN);
                    add_flag_to_label(lbl_true.value, JumpFlags::IN);
                }
                Jump { lbl, .. } => {
                    instr.jumps.insert(JumpFlags::OUT);
                    add_flag_to_label(lbl.value, JumpFlags::IN);
                }
                Ret { .. } => {
                    instr.jumps.insert(JumpFlags::RETURN);
                }
                Addr { lbl, .. } => {
                    add_flag_to_label(lbl.value, JumpFlags::IN);
                }
                DJump { .. } => {
                    instr.jumps.insert(JumpFlags::OUT);
                }
                Label { .. }
                | Reg { .. }
                | Call { .. }
                | DCall { .. }
                | Int { .. }
                | Str { .. }
                | Ptr { .. }
                | Binary { .. }
                | Unary { .. }
                | Incr { .. }
                | Decr { .. }
                | Arr { .. }
                | Get { .. }
                | Set { .. }
                | Len { .. }
                | Type { .. }
                | Putc { .. } => {}
            }
        }

        for (lbl, jumps) in label_sets {
            let LabelInfo { func, index, .. } = self.labels[&lbl];
            let func = self.functions.get_mut(&func).unwrap();
            // index + 1 because the instruction after the Label is the one we want to mark
            let instr = &mut func.instructions[index + 1];
            instr.jumps.insert(jumps);
        }
    }
}

bitflags::bitflags! {
    pub struct JumpFlags: u8 {
        /// The instruction stops program execution.
        const STOP   = 0b00000001;
        /// The instruction returns from the current function.
        const RETURN = 0b00000010;
        /// The instruction jumps back to itself.
        const SELF   = 0b00000100;
        /// The instruction is jumped to.
        const IN     = 0b00001000;
        /// The instructions jumps.
        const OUT    = 0b00010000;
    }
}

impl JumpFlags {
    pub const EMPTY: Self = Self::empty();
}
