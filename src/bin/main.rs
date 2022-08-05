use clap::Parser;
use codespan_reporting::term::termcolor::ColorChoice;
use rust_minivm::{
    config::{Diagnostic, Process, RunConfig},
    RuntimeError,
    Value,
};

#[derive(Parser)]
struct Cli {
    #[clap(value_parser)]
    file_path: Option<String>,
    #[clap(long)]
    debug: bool,
}

#[derive(Debug)]
enum Error {
    Diagnostics(Vec<Diagnostic>),
    RuntimeError(RuntimeError),
}

impl From<Vec<Diagnostic>> for Error {
    fn from(diagnostics: Vec<Diagnostic>) -> Self {
        Error::Diagnostics(diagnostics)
    }
}

impl From<RuntimeError> for Error {
    fn from(err: RuntimeError) -> Self {
        Error::RuntimeError(err)
    }
}

fn run<'a>(process: &Process) -> Result<Value, Error> {
    let (bytecode, gc) = rust_minivm::compile(process)?;
    let ret = rust_minivm::run(bytecode.reader(), gc, process)?;
    Ok(ret)
}

fn run_from_file(path: &str, config: &RunConfig) {
    let path = std::path::Path::new(path);
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => return eprintln!("{err}"),
    };
    let file_name = {
        let os_str = path.file_name().unwrap();
        os_str.to_str().unwrap()
    };
    let process = Process::new(&contents, file_name, config);
    let _ = match run(&process) {
        Ok(value) => value,
        Err(Error::Diagnostics(diagnostics)) => return display_diagnostics(diagnostics, &process),
        Err(Error::RuntimeError(err)) => return display_runtime_error(err, &process),
    };
}

fn display_diagnostics(diagnostics: Vec<Diagnostic>, process: &Process) {
    let writer =
        codespan_reporting::term::termcolor::StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        codespan_reporting::term::emit(
            &mut writer.lock(),
            &config,
            process.files(),
            &diagnostic,
        ).unwrap();
    }
}

fn display_runtime_error(err: RuntimeError, process: &Process) {
    todo!()
}

fn main() {
    let args: Cli = Parser::parse();
    let run_config = RunConfig {
        dump_bytecode: args.debug,
        dump_internal_bytecode: args.debug,
        trace_execution: args.debug,
        print_output: args.debug,
    };

    if let Some(path) = &args.file_path {
        run_from_file(path, &run_config)
    } else {
        todo!("REPL")
    }
}
