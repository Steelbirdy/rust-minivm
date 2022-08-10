#![cfg_attr(not(feature = "unsafe"), deny(unsafe_code))]
#![warn(clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::match_same_arms,
    clippy::enum_glob_use
)]

mod common;
pub mod hir;
pub mod lir;
pub mod parse;
mod vm;

pub use common::{
    config::{self, Diagnostic, Process},
    BytecodeReader, Interner,
};
pub use vm::{disassemble as disassemble_opcode, lower_lir, Gc, Value, Vm};

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
    hir::LoweringContext::new(interner, tree)
        .finish()
        .map_err(|errors| errors_to_diagnostics(errors, process))
}

pub fn build_lir(
    hir: &hir::Program,
    interner: &Interner,
    gc: &mut Gc,
    _process: &Process,
) -> Result<Vec<lir::Instruction>> {
    Ok(lir::lower_hir(hir, gc, interner))
}

pub fn build_bytecode(lir: &[lir::Instruction], _process: &Process) -> Result<Box<[u8]>> {
    Ok(vm::lower_lir(lir))
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
    Ok(ret)
}

pub fn compile_and_run(process: &Process) -> Result<Value> {
    let mut interner = Interner::new();
    let mut gc = Gc::new();

    // Parsing (source -> AST)
    let tokens = lex(process);
    let tree = parse(&tokens, process)?;
    validate(&tree, process)?;

    // HIR Generation (AST -> HIR)
    let hir_program = build_hir(&tree, &mut interner, process)?;

    // LIR Generation (HIR -> LIR)
    let lir_instructions = build_lir(&hir_program, &interner, &mut gc, process)?;

    // Bytecode Generation (LIR -> Bytecode)
    let bytecode = build_bytecode(&lir_instructions, process)?;
    let reader = BytecodeReader::new(&bytecode);
    if process.config.dump_bytecode {
        println!("=== BYTECODE ===");
        println!("{}", disassemble_opcode(reader));
        println!("================\n");
    }

    // Execution
    run(reader, gc, process)
}
