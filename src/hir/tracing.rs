use crate::{
    common::{Key, Reg},
    hir::{
        self, BranchKind, Function, Instruction, JumpKind, KeyWithRange, LabelInfo, Program,
        UnaryArg,
    },
};
use rustc_hash::FxHashSet;
use std::marker::PhantomData;
use std::ops::ControlFlow;

pub struct Tracer<'a, F, T: 'a> {
    program: &'a Program,
    func: &'a Function,
    visited: FxHashSet<usize>,
    callback: F,
    __phantom: PhantomData<&'a T>,
}

impl<'a, F, T> Tracer<'a, F, T>
where
    F: FnMut(&Instruction) -> ControlFlow<T>,
{
    pub fn new(program: &'a Program, func: &'a Function, callback: F) -> Self {
        Self {
            program,
            func,
            visited: FxHashSet::default(),
            callback,
            __phantom: PhantomData,
        }
    }

    pub fn trace(&mut self) -> Option<T> {
        self.trace_at(0)
    }

    pub fn trace_at(&mut self, start: usize) -> Option<T> {
        match self._trace(start) {
            ControlFlow::Break(value) => value,
            ControlFlow::Continue(()) => None,
        }
    }

    fn trace_at_label(&mut self, key: Key) -> ControlFlow<Option<T>> {
        let LabelInfo { func, index, .. } = self.program.labels[&key];
        assert_eq!(func, self.func.name.value);
        self._trace(index)
    }

    fn _trace(&mut self, mut start: usize) -> ControlFlow<Option<T>> {
        while start < self.func.instructions.len() {
            if !self.visited.insert(start) {
                start += 1;
                continue;
            }

            let instr = &self.func.instructions[start];
            if let ControlFlow::Break(value) = (self.callback)(instr) {
                return ControlFlow::Break(Some(value));
            }

            match hir::Visit::visit(self, &instr.kind) {
                x @ ControlFlow::Break(_) => {
                    return x;
                }
                ControlFlow::Continue(()) => {
                    start += 1;
                    continue;
                }
            }
        }
        ControlFlow::Continue(())
    }
}

impl<'a, F, T> hir::Visit for Tracer<'a, F, T>
where
    F: FnMut(&Instruction) -> ControlFlow<T>,
{
    type Output = ControlFlow<Option<T>>;

    fn unimplemented(&mut self) -> Self::Output {
        ControlFlow::Continue(())
    }

    fn visit_exit(&mut self) -> Self::Output {
        ControlFlow::Break(None)
    }

    fn visit_branch(
        &mut self,
        _kind: BranchKind,
        lbl_false: KeyWithRange,
        lbl_true: KeyWithRange,
    ) -> Self::Output {
        match self.trace_at_label(lbl_false.value) {
            x @ ControlFlow::Break(Some(_)) => x,
            ControlFlow::Break(None) | ControlFlow::Continue(()) => {
                self.trace_at_label(lbl_true.value)
            }
        }
    }

    fn visit_jump(&mut self, kind: JumpKind, lbl: KeyWithRange) -> Self::Output {
        match self.trace_at_label(lbl.value) {
            x @ ControlFlow::Break(Some(_)) => x,
            ControlFlow::Break(None) | ControlFlow::Continue(()) => {
                if kind == JumpKind::Unconditional {
                    ControlFlow::Break(None)
                } else {
                    ControlFlow::Continue(())
                }
            }
        }
    }

    fn visit_addr(&mut self, _to: Reg, lbl: KeyWithRange) -> Self::Output {
        self.trace_at_label(lbl.value).into()
    }

    fn visit_ret(&mut self, _arg: UnaryArg) -> Self::Output {
        ControlFlow::Break(None)
    }
}
