#![cfg_attr(not(feature = "unsafe"), deny(unsafe_code))]
#![warn(clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::match_same_arms,
    clippy::enum_glob_use
)]

#[macro_use]
mod common;

pub mod hir;
pub mod mir;
pub mod parse;
mod vm;

pub use common::{
    config::{self, Diagnostic, Process},
    BytecodeBuffer, BytecodeReader, Interner,
};
pub use vm::{disassemble as disassemble_opcode, lower_mir, Gc, Value, Vm};

use common::config::VmDiagnostic;
use parse::{grammar, Parser};

pub type Result<T> = std::result::Result<T, Vec<Diagnostic>>;

fn errors_to_diagnostics<T: VmDiagnostic>(
    errors: impl IntoIterator<Item = T>,
    process: &Process,
) -> Vec<Diagnostic> {
    errors
        .into_iter()
        .map(|err| err.to_diagnostic(process))
        .collect()
}

#[must_use]
pub fn lex(process: &Process) -> parse::Tokens {
    parse::Tokens::tokenize(process.source)
}

pub fn parse(tokens: &parse::Tokens, process: &Process) -> Result<parse::SyntaxTree> {
    let result = Parser::parse(process.source, tokens, grammar::assembly);
    let (tree, errors) = result.into_parts();
    if errors.is_empty() {
        Ok(tree)
    } else {
        Err(errors_to_diagnostics(errors, process))
    }
}

pub fn validate(tree: &parse::SyntaxTree, process: &Process) -> Result<()> {
    parse::validate(tree).map_err(|errors| errors_to_diagnostics(errors, process))
}

pub fn build_hir(
    tree: &parse::SyntaxTree,
    interner: &mut Interner,
    process: &Process,
) -> Result<hir::Program> {
    hir::LoweringContext::new(tree, interner)
        .finish()
        .map_err(|errors| errors_to_diagnostics(errors, process))
}

pub fn build_mir(
    hir: &hir::Program,
    interner: &Interner,
    gc: &mut Gc,
    _process: &Process,
) -> Result<Vec<mir::Instruction>> {
    Ok(mir::lower_hir(hir, interner, gc))
}

pub fn build_bytecode(mir: &[mir::Instruction], _process: &Process) -> Result<BytecodeBuffer> {
    Ok(vm::lower_mir(mir))
}

pub fn run(bytecode: BytecodeReader, gc: Gc, process: &Process) -> Result<Value> {
    macro_rules! trace {
        ($str:literal $(, $args:expr)* $(,)?) => {
            #[cfg(feature = "trace-execution")]
            println!($str, $($args),*)
        };
    }

    trace!("=== EXECUTION ===");
    let ret = Vm::new(bytecode, gc, &process.config).run();
    trace!("===           ===");
    ret.map_err(|err| vec![err.to_diagnostic(process)])
}

pub fn compile_and_run(process: &Process) -> Result<Value> {
    let mut interner = Interner::new();
    let mut gc = Gc::new();

    // Parsing (source -> AST)
    let tokens = lex(process);
    let tree = parse(&tokens, process)?;
    validate(&tree, process)?;

    // HIR Generation (AST -> HIR)
    let program = build_hir(&tree, &mut interner, process)?;

    // MIR Generation (HIR -> MIR)
    let instructions = build_mir(&program, &interner, &mut gc, process)?;

    // Bytecode Generation (MIR -> Bytecode)
    let bytecode = build_bytecode(&instructions, process)?;
    if process.config.dump_bytecode {
        println!("=== BYTECODE ===");
        println!("{}", disassemble_opcode(bytecode.reader(), &mut gc));
        println!("================\n");
    }

    // Execution
    run(bytecode.reader(), gc, process)
}
