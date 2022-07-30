#![warn(clippy::cast_lossless)]

mod common;
mod compile;
mod parse;
mod vm;

pub use common::{ByteReader, Interner, Read, config};
pub use compile::{assemble, disassemble as disassemble_bytecode};
pub use parse::{lex, parse};
pub use vm::{Value, disassemble as disassemble_opcode, lower_bytecode, trace_jumps, Gc, Vm};

pub fn run(process: &config::Process) -> Option<Value> {
    let mut interner = Interner::new();
    let mut gc = Gc::new();

    // Parsing (source -> AST)
    let program = match parse(process.source, &mut interner) {
        Ok(p) => p,
        Err(why) => {
            eprintln!("{why:?}");
            return None;
        }
    };

    // Assembly (AST -> bytecode)
    let bytecode = assemble(&program, &interner);
    if process.config.dump_bytecode {
        println!("=== BYTECODE ===");
        println!("{}", disassemble_bytecode(bytecode.as_ref()));
        println!("================\n");
    }
    let jumps = trace_jumps(bytecode.as_ref());

    // Lowering (bytecode -> internal bytecode)
    let opcode = common::OwnedByteBuffer::new(
        lower_bytecode(bytecode.as_ref(), &jumps, &mut gc)
    );
    if process.config.dump_internal_bytecode {
        println!("=== INTERNAL BYTECODE ===");
        println!("{}", disassemble_opcode(opcode.as_ref()));
        println!("=========================\n");
    }

    // Run the VM
    if process.config.trace_execution {
        println!("=== EXECUTION ===");
    }
    Some(Vm::new(opcode.as_ref(), &mut gc, &process.config).run())
}