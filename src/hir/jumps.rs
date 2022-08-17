use crate::{
    common::{Key, Reg},
    hir::{Function, InstructionKind, LabelInfo, Program},
};
use rustc_hash::FxHashMap;
use text_size::TextSize;

impl Function {
    pub(crate) fn register_is_used(&self, reg: Reg, start: usize) -> bool {
        let instructions = &self.instructions[start..];
        for instr in instructions {
            let usage = instr.kind.register_use();
            if usage.used.contains(&reg) {
                return true;
            }
            if usage.initialized.contains(&reg) {
                return false;
            }
        }
        false
    }
}

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
        let mut label_sets = FxHashMap::<Key, JumpFlags>::default();

        macro_rules! compute_flags {
            ($instr:ident, $($label:expr),+) => {{
                let src = $instr.range.start();
                let labels = [$($label),+];
                let dests = labels.map(|lbl| self.labels[&lbl].range.start());
                let (src_flag, dest_flags) = categorize_jumps(src, dests);

                $instr.jumps |= src_flag;
                for (lbl, flag) in labels.into_iter().zip(dest_flags) {
                    label_sets.entry(lbl).or_insert(JumpFlags::EMPTY).insert(flag);
                }
            }};
        }

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
                    compute_flags!(instr, lbl_false.value, lbl_true.value);
                }
                Jump { lbl, .. } => {
                    compute_flags!(instr, lbl.value);
                }
                Ret { .. } => {
                    instr.jumps.insert(JumpFlags::RETURN);
                }
                Addr { lbl, .. } => {
                    label_sets
                        .entry(lbl.value)
                        .or_insert(JumpFlags::EMPTY)
                        .insert(JumpFlags::IN);
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
            let instr = &mut func.instructions[index];
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

fn categorize_jump(src: TextSize, dest: TextSize) -> (JumpFlags, JumpFlags) {
    if src == dest {
        (JumpFlags::SELF, JumpFlags::SELF)
    } else {
        (JumpFlags::OUT, JumpFlags::IN)
    }
}

fn categorize_jumps<const N: usize>(
    src: TextSize,
    dests: [TextSize; N],
) -> (JumpFlags, [JumpFlags; N]) {
    let mut first = JumpFlags::empty();
    let ret = dests.map(|dest| {
        let (src_flag, dest_flag) = categorize_jump(src, dest);
        first.insert(src_flag);
        dest_flag
    });
    (first, ret)
}
