use codespan_reporting::{
    diagnostic::Severity,
    files::{SimpleFile, SimpleFiles},
};

pub const NUM_FRAMES: usize = 128;
pub const NUM_REGISTERS_PER_FRAME: usize = 16;
pub const NUM_REGISTERS: usize = NUM_REGISTERS_PER_FRAME * NUM_FRAMES;

pub type Files<'a> = SimpleFiles<&'a str, &'a str>;
pub type File<'a> = SimpleFile<&'a str, &'a str>;
pub type FileId = usize;

#[derive(Debug, Clone)]
pub struct Process<'a> {
    pub source: &'a str,
    file_id: FileId,
    files: Files<'a>,
    pub config: &'a RunConfig,
}

impl<'a> Process<'a> {
    #[must_use]
    pub fn new(source: &'a str, file_name: &'a str, config: &'a RunConfig) -> Self {
        let mut files = Files::new();
        let file_id = files.add(file_name, source);
        Self {
            source,
            file_id,
            files,
            config,
        }
    }

    #[must_use]
    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    #[must_use]
    pub fn files(&self) -> &Files<'a> {
        &self.files
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RunConfig {
    pub dump_bytecode: bool,
    pub trace_execution: bool,
}

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<FileId>;

pub trait VmDiagnostic {
    fn severity(&self) -> Severity;

    fn to_diagnostic(&self, process: &Process) -> Diagnostic;
}
