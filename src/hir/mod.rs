mod error;
mod jumps;
mod lower_ast;
mod register_use;
mod repr;
mod tracing;
mod visit;

pub use error::{HirError, HirErrorKind};
pub use jumps::JumpFlags;
pub use repr::Program;

pub(crate) use lower_ast::LoweringContext;
pub(crate) use register_use::register_is_used;
pub(crate) use repr::{
    BinaryArgs, BinaryOp, BranchKind, Function, Instruction, InstructionKind, JumpKind,
    KeyWithRange, LabelInfo, SetArgs, UnaryArg, UnaryOp,
};
pub(crate) use visit::Visit;
