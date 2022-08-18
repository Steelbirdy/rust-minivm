use crate::{
    common::Reg,
    hir::{tracing::Tracer, Function, Instruction, Program},
};
use std::ops::ControlFlow;

pub fn register_is_used(program: &Program, func: &Function, reg: Reg, start: usize) -> bool {
    RegisterUse::new(program, func).reg_is_used(reg, start)
}

struct RegisterUse<'a> {
    program: &'a Program,
    func: &'a Function,
}

impl<'a> RegisterUse<'a> {
    pub fn new(program: &'a Program, func: &'a Function) -> Self {
        Self { program, func }
    }

    pub fn reg_is_used(&self, reg: Reg, start: usize) -> bool {
        fn callback(reg: Reg, instr: &Instruction) -> ControlFlow<bool> {
            let usage = instr.kind.register_use();
            if usage.used.contains(&reg) {
                ControlFlow::Break(true)
            } else if usage.initialized.contains(&reg) {
                ControlFlow::Break(false)
            } else {
                ControlFlow::Continue(())
            }
        }

        let mut tracer = Tracer::new(self.program, self.func, |instr| callback(reg, instr));
        tracer.trace_at(start).unwrap_or_default()
    }
}
