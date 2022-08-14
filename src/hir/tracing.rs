use crate::{
    common::Key,
    hir::{Instruction, LabelInfo, Program},
};
use rustc_hash::FxHashMap;

pub type Jumps = FxHashMap<Key, Vec<JumpSet>>;

impl Program {
    pub(in crate::hir) fn trace_jumps(&self) {
        for &func in self.functions.keys() {
            self.trace_jumps_in_function(func);
        }
    }

    fn add_flag(&self, func: Key, index: usize, flag: JumpFlag) {
        let jumps = &self.functions[&func].jumps[index];
        jumps.set(jumps.get().with(flag));
    }

    fn add_flag_to_label(&self, lbl: Key, flag: JumpFlag) {
        let LabelInfo { func, index, .. } = self.labels[&lbl];
        self.add_flag(func, index, flag);
    }

    fn trace_jumps_in_function(&self, name: Key) {
        use Instruction::*;

        let func = &self.functions[&name];
        for (i, instr) in func.instructions.iter().enumerate() {
            match &instr.value {
                Exit => {
                    self.add_flag(name, i, JumpFlag::Out);
                }
                Branch {
                    lbl_false,
                    lbl_true,
                    ..
                } => {
                    self.add_flag(name, i, JumpFlag::Out);
                    self.add_flag_to_label(lbl_false.value, JumpFlag::In);
                    self.add_flag_to_label(lbl_true.value, JumpFlag::In);
                }
                Jump { lbl, .. } => {
                    self.add_flag(name, i, JumpFlag::Out);
                    self.add_flag_to_label(lbl.value, JumpFlag::In);
                }
                Ret { .. } => {
                    self.add_flag(name, i, JumpFlag::Out);
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
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum JumpFlag {
    In = 1,
    Out = 2,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct JumpSet(u8);

impl JumpSet {
    pub const EMPTY: JumpSet = JumpSet(0);

    pub fn contains(self, flag: JumpFlag) -> bool {
        self.0 & (flag as u8) != 0
    }

    pub fn with(self, flag: JumpFlag) -> Self {
        Self(self.0 | (flag as u8))
    }
}
