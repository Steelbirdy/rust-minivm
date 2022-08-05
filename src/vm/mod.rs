mod debug;
mod error;
mod gc;
mod opcode;
mod run;
mod tracing;
mod value;

pub use debug::{disassemble, disassemble_instruction};
pub use error::RuntimeError;
pub use gc::Gc;
pub use opcode::{lower_bytecode, Opcode};
pub use run::Vm;
pub use tracing::{register_is_used, trace_jumps, Jump, JumpSet};
pub use value::{Value, ValueKind};

pub type Ptr = gc::Ptr;
pub type Len = u32;
