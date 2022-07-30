use codespan_reporting::files::{SimpleFile, SimpleFiles};

pub const NUM_FRAMES: usize = 128;
pub const NUM_REGISTERS: usize = 16 * NUM_FRAMES;

pub type Files<'a> = SimpleFiles<&'a str, &'a str>;
pub type File<'a> = SimpleFile<&'a str, &'a str>;
pub type FileId = usize;

#[derive(Debug, Clone)]
pub struct Process<'a> {
    pub source: &'a str,
    pub config: RunConfig,
}

#[derive(Debug, Copy, Clone)]
pub struct RunConfig {
    pub dump_bytecode: bool,
    pub dump_internal_bytecode: bool,
    pub trace_execution: bool,
    pub print_output: bool,
}