mod debug;
mod error;
mod gc;
mod lower_mir;
mod opcode;
mod run;
mod value;

pub use debug::{disassemble, disassemble_instruction};
pub use error::RuntimeError;
pub use gc::Gc;
pub use lower_mir::lower_mir;
pub use opcode::Opcode;
pub use run::Vm;
pub use value::{TaggedValue, Value, ValueKind};

pub type Ptr = gc::Ptr;
pub type Len = u32;
