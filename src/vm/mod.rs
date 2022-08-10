mod debug;
mod gc;
mod lower_lir;
mod opcode;
mod run;
mod value;

pub use debug::{disassemble, disassemble_instruction};
pub use gc::Gc;
pub use lower_lir::lower_lir;
pub use opcode::Opcode;
pub use run::Vm;
pub use value::{Value, ValueKind};

pub type Ptr = gc::Ptr;
pub type Len = u32;
