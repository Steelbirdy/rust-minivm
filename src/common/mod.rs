mod bytes;
mod interner;
pub mod config;

pub use bytes::{
    ByteBuffer, ByteEditor, ByteReader, ByteWriter, FromBytes, OwnedByteBuffer, Push, Read,
    ToBytes, Write,
};
pub use interner::{Interner, Key};

pub const ENTRY_POINT_LBL: &str = "__entry";

pub type Short = i32;
pub type Int = i64;
pub type UShort = u32;
pub type UInt = u64;
pub type Reg = u16;
pub type Addr = u32;

pub type List<T> = smallvec::SmallVec<[T; 3]>;
