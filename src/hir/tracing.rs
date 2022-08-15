use crate::{
    common::Key,
    hir::{InstructionKind, LabelInfo, Program},
};
use rustc_hash::FxHashMap;

pub type Jumps = FxHashMap<Key, Vec<JumpSet>>;

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
        let mut label_sets = FxHashMap::<Key, JumpSet>::default();

        macro_rules! add_flag_to_labels {
            ($flag:expr, $($label:expr),+) => {{
                $(
                label_sets.entry($label).or_default().insert($flag);
                )+
            }};
        }

        for i in 0..instrs.len() {
            let instr = &mut instrs[i];

            match &instr.kind {
                Exit => {
                    instr.jumps.insert(JumpFlag::Out);
                }
                Branch {
                    lbl_false,
                    lbl_true,
                    ..
                } => {
                    instr.jumps.insert(JumpFlag::Out);
                    add_flag_to_labels!(JumpFlag::In, lbl_false.value, lbl_true.value);
                }
                Jump { lbl, .. } => {
                    instr.jumps.insert(JumpFlag::Out);
                    add_flag_to_labels!(JumpFlag::In, lbl.value);
                }
                Ret { .. } => {
                    instr.jumps.insert(JumpFlag::Out);
                }
                Label { .. }
                | Reg { .. }
                | Call { .. }
                | Addr { .. }
                | DJump { .. }
                | DCall { .. }
                | Int { .. }
                | Str { .. }
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
            instr.jumps.insert_all(jumps);
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum JumpFlag {
    In = 1,
    Out = 2,
}

#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct JumpSet(u8);

impl JumpSet {
    pub const EMPTY: JumpSet = JumpSet(0);

    pub fn contains(self, flag: JumpFlag) -> bool {
        self.0 & (flag as u8) != 0
    }

    pub fn insert(&mut self, flag: JumpFlag) {
        self.0 |= flag as u8;
    }

    pub fn insert_all(&mut self, other: Self) {
        self.0 |= other.0;
    }
}
