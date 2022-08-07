#![cfg_attr(not(feature = "unsafe"), deny(unsafe_code))]
#![warn(clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::match_same_arms,
    clippy::enum_glob_use
)]

mod common;
mod compile;
mod parse;
mod vm;

use codespan_reporting::diagnostic::Severity;
pub use common::{config::{self, Diagnostic, VmDiagnostic}, BytecodeBuffer, BytecodeBuilder, BytecodeReader, Interner};
pub use compile::{assemble, disassemble as disassemble_bytecode, CompileError};
pub use parse::{lex, parse, validate, ParseConfig, ParseError as ValidateError};
pub use vm::{
    disassemble as disassemble_opcode, lower_bytecode, trace_jumps, Gc, RuntimeError, Value, Vm,
};
use crate::config::Process;

pub type ParseError = eventree_wrapper::parser::ParseError<ParseConfig>;

#[derive(Debug)]
pub enum Error {
    Parse(ParseError),
    Validate(ValidateError),
    Compile(CompileError),
    Runtime(RuntimeError),
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
}

impl From<ValidateError> for Error {
    fn from(e: ValidateError) -> Self {
        Self::Validate(e)
    }
}

impl From<CompileError> for Error {
    fn from(e: CompileError) -> Self {
        Self::Compile(e)
    }
}

impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

impl VmDiagnostic for Error {
    fn severity(&self) -> Severity {
        match self {
            Self::Parse(e) => e.severity(),
            Self::Validate(e) => e.severity(),
            Self::Compile(e) => e.severity(),
            Self::Runtime(e) => e.severity(),
        }
    }

    fn to_diagnostic(&self, process: &Process) -> Diagnostic {
        match self {
            Self::Parse(e) => e.to_diagnostic(process),
            Self::Validate(e) => e.to_diagnostic(process),
            Self::Compile(e) => e.to_diagnostic(process),
            Self::Runtime(e) => e.to_diagnostic(process),
        }
    }
}

pub fn compile(process: &config::Process, gc: &mut Gc) -> Result<BytecodeBuffer, Vec<Error>> {
    fn collect_errors<E: Into<Error>>(errors: impl IntoIterator<Item = E>) -> Vec<Error> {
        errors
            .into_iter()
            .map(Into::into)
            .collect()
    }

    let mut interner = Interner::new();

    let parse_result = parse(process.source);
    if parse_result.has_errors() {
        return Err(collect_errors(parse_result.into_parts().1));
    }
    if let Err(errors) = validate(parse_result.syntax_tree()) {
        return Err(collect_errors(errors));
    }
    let bytecode = match assemble(parse_result.syntax_tree(), &mut interner) {
        Ok(b) => b,
        Err(errors) => {
            return Err(collect_errors(errors));
        }
    };
    if process.config.dump_bytecode {
        println!("=== BYTECODE ===");
        println!("{}", disassemble_bytecode(bytecode.reader()));
        println!("================\n");
    }
    let jumps = trace_jumps(bytecode.reader());

    // Lowering (bytecode -> internal bytecode)
    let opcode = lower_bytecode(bytecode.reader(), &jumps, gc);
    if process.config.dump_internal_bytecode {
        println!("=== INTERNAL BYTECODE ===");
        println!("{}", disassemble_opcode(opcode.reader()));
        println!("=========================\n");
    }
    Ok(opcode)
}

pub fn run(code: BytecodeReader, gc: Gc, process: &config::Process) -> Result<Value, RuntimeError> {
    Vm::new(code, gc, &process.config).run()
}
