#[macro_use]
mod macros;

mod bytes;
pub mod config;
mod interner;

pub use bytes::{AsBytes, BytecodeBuffer, BytecodeBuilder, BytecodeReader};
pub use interner::{Interner, Key};

pub const ENTRY_POINT_LBL: &str = "__entry";

pub type Short = i32;
pub type Int = i64;
pub type UShort = u32;
pub type UInt = u64;
pub type Reg = u16;
pub type Addr = u32;

pub const INT_MAX: Int = Int::MAX >> 1;
pub const INT_MIN: Int = Int::MIN >> 1;

pub const fn int_is_valid(int: Int) -> bool {
    INT_MIN <= int && int <= INT_MAX
}

pub type List<T> = smallvec::SmallVec<[T; 3]>;
pub use smallvec::smallvec as list;
