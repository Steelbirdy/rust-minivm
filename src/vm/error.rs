use crate::{Diagnostic, Process, vm::Ptr, VmDiagnostic};
use std::fmt;
use codespan_reporting::diagnostic::Severity;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    OutOfBounds { ptr: Ptr, index: usize, len: usize },
    StackOverflow,
}

impl VmDiagnostic for RuntimeError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn to_diagnostic(&self, _process: &Process) -> Diagnostic {
        match self {
            Self::OutOfBounds { ptr, index, len } => {
                Diagnostic::error()
                    .with_message(format!("index out of bounds: index was {index} but array at 0x{ptr} has length {len}"))
            }
            Self::StackOverflow => Diagnostic::error().with_message("stack overflow"),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OutOfBounds { ptr, index, len } => {
                write!(
                    f,
                    "index out of bounds: index was {index} but array at 0x{ptr} has length {len}"
                )
            }
            Self::StackOverflow => write!(f, "stack overflow"),
        }
    }
}
