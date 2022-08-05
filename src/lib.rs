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

pub use common::config::Diagnostic;
pub use common::{config, BytecodeBuffer, BytecodeBuilder, BytecodeReader, Interner};
pub use compile::{assemble, disassemble as disassemble_bytecode};
pub use parse::{lex, parse, validate};
pub use vm::{
    disassemble as disassemble_opcode, lower_bytecode, trace_jumps, Gc, RuntimeError, Value, Vm,
};

use common::config::VmDiagnostic;

pub fn compile(process: &config::Process) -> Result<(BytecodeBuffer, Gc), Vec<Diagnostic>> {
    let mut interner = Interner::new();
    let mut gc = Gc::new();

    let parse_result = parse(process.source);
    if parse_result.has_errors() {
        return Err(parse_result
            .errors()
            .iter()
            .map(|err| err.to_diagnostic(process))
            .collect());
    }
    if let Err(errors) = validate(parse_result.syntax_tree()) {
        return Err(errors
            .iter()
            .map(|err| err.to_diagnostic(process))
            .collect());
    }
    let bytecode = match assemble(parse_result.syntax_tree(), &mut interner) {
        Ok(b) => b,
        Err(errors) => {
            return Err(errors
                .iter()
                .map(|err| err.to_diagnostic(process))
                .collect());
        }
    };
    if process.config.dump_bytecode {
        println!("=== BYTECODE ===");
        println!("{}", disassemble_bytecode(bytecode.reader()));
        println!("================\n");
    }
    let jumps = trace_jumps(bytecode.reader());

    // Lowering (bytecode -> internal bytecode)
    let opcode = lower_bytecode(bytecode.reader(), &jumps, &mut gc);
    if process.config.dump_internal_bytecode {
        println!("=== INTERNAL BYTECODE ===");
        println!("{}", disassemble_opcode(opcode.reader()));
        println!("=========================\n");
    }
    Ok((opcode, gc))
}

pub fn run(code: BytecodeReader, gc: Gc, process: &config::Process) -> Result<Value, RuntimeError> {
    Ok(Vm::new(code, gc, &process.config).run())
}
