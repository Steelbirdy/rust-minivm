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
pub use common::{config, ByteReader, Interner, Read};
pub use compile::{assemble, disassemble as disassemble_bytecode};
pub use parse::{lex, parse, validate};
pub use vm::{disassemble as disassemble_opcode, lower_bytecode, trace_jumps, Gc, Value, Vm};

use common::config::VmDiagnostic;

pub fn run(process: &config::Process) -> Result<Value, Vec<Diagnostic>> {
    let mut interner = Interner::new();
    let mut gc = Gc::new();

    // Parsing (source -> AST)
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

    // Assembly (AST -> bytecode)
    let bytecode = assemble(parse_result.syntax_tree(), &mut interner);
    if process.config.dump_bytecode {
        println!("=== BYTECODE ===");
        println!("{}", disassemble_bytecode(bytecode.as_ref()));
        println!("================\n");
    }
    let jumps = trace_jumps(bytecode.as_ref());

    // Lowering (bytecode -> internal bytecode)
    let opcode = common::OwnedByteBuffer::new(lower_bytecode(bytecode.as_ref(), &jumps, &mut gc));
    if process.config.dump_internal_bytecode {
        println!("=== INTERNAL BYTECODE ===");
        println!("{}", disassemble_opcode(opcode.as_ref()));
        println!("=========================\n");
    }

    // Run the VM
    if process.config.trace_execution {
        println!("=== EXECUTION ===");
    }
    let ret = Vm::new(opcode, gc, &process.config).run();
    if process.config.trace_execution {
        println!("===           ===");
    }
    Ok(ret)
}
