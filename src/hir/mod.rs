mod error;
mod lower_ast;
mod repr;
mod tracing;
mod visit;

pub use error::{HirError, HirErrorKind};
pub use repr::Program;
pub use tracing::{JumpSet, Jumps};

pub(crate) use lower_ast::LoweringContext;
pub(crate) use repr::{
    BinaryArgs, BinaryOp, BranchKind, Function, Instruction, InstructionWithRange, JumpKind,
    KeyWithRange, LabelInfo, SetArgs, UnaryArg, UnaryOp,
};
pub(crate) use visit::Visit;
