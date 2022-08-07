use clap::Parser;
use codespan_reporting::term::termcolor::ColorChoice;
use rust_minivm::{config::{Diagnostic, Process, RunConfig}, Value, Error, VmDiagnostic};

#[derive(Parser)]
struct Cli {
    #[clap(value_parser)]
    file_path: Option<String>,
    #[clap(long)]
    debug: bool,
}

fn run<'a>(process: &Process) -> Result<Value, Vec<Error>> {
    let mut gc = rust_minivm::Gc::new();
    let bytecode = rust_minivm::compile(process, &mut gc)?;
    let ret = rust_minivm::run(bytecode.reader(), gc, process).map_err(|e| vec![e.into()])?;
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
    if let Err(errors) = run(&process) {
        let diagnostics = errors.into_iter().map(|e| e.to_diagnostic(&process)).collect();
        display_diagnostics(diagnostics, &process);
    }
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
