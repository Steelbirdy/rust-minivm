mod error;
mod lower_ast;
mod repr;

pub use error::{HirError, HirErrorKind};
pub use repr::Program;

pub(crate) use lower_ast::LoweringContext;
pub(crate) use repr::{
    BinaryArgs, BinaryOp, BranchKind, Function, Instruction, InstructionWithRange, JumpKind,
    KeyWithRange, SetArgs, UnaryArg, UnaryOp,
};

use crate::{common::List, parse::SyntaxTree, Interner};

pub fn lower_ast(interner: &mut Interner, tree: &SyntaxTree) -> Result<Program, List<HirError>> {
    lower_ast::LoweringContext::new(interner, tree).finish()
}
