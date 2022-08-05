use crate::{
    common::Int,
    vm::{Len, Ptr},
};
use std::fmt;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    OutOfBounds { ptr: Ptr, index: Int, len: Len },
    StackOverflow,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OutOfBounds { ptr, index, len } => {
                write!(
                    f,
                    "index out of bounds: index was {index} but array at {ptr:?} has length {len}"
                )
            }
            Self::StackOverflow => write!(f, "stack overflow"),
        }
    }
}
