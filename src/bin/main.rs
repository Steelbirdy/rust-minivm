use clap::Parser;
use codespan_reporting::term::termcolor::ColorChoice;
use rust_minivm::{
    config::{Diagnostic, Process, RunConfig},
    Value,
};

#[derive(Parser)]
struct Cli {
    #[clap(value_parser)]
    file_path: Option<String>,
    #[clap(long)]
    debug: bool,
}

fn run<'a>(
    source: &'a str,
    file_name: &'a str,
    config: RunConfig,
) -> (Process<'a>, Result<Value, Vec<Diagnostic>>) {
    let process = Process::new(source, file_name, config);
    let ret = rust_minivm::run(&process);
    (process, ret)
}

type RunOutput = (Process<'static>, Result<Value, Vec<Diagnostic>>);

fn run_from_file(path: &str, config: RunConfig) -> Result<RunOutput, Box<dyn std::error::Error>> {
    let path = std::path::Path::new(path);
    let contents = std::fs::read_to_string(path)?;
    let contents = Box::leak(contents.into_boxed_str());
    let file_name = {
        let tmp = path.file_name().unwrap();
        let tmp = tmp.to_str().unwrap();
        let tmp = tmp.to_string().into_boxed_str();
        Box::leak(tmp)
    };
    Ok(run(contents, file_name, config))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Cli = Parser::parse();
    let run_config = RunConfig {
        dump_bytecode: args.debug,
        dump_internal_bytecode: args.debug,
        trace_execution: args.debug,
        print_output: args.debug,
    };

    let (process, out) = if let Some(path) = &args.file_path {
        run_from_file(path, run_config)?
    } else {
        // run_repl(run_config)
        todo!("REPL")
    };

    if let Err(diagnostics) = out {
        let writer =
            codespan_reporting::term::termcolor::StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for diagnostic in diagnostics {
            codespan_reporting::term::emit(
                &mut writer.lock(),
                &config,
                process.files(),
                &diagnostic,
            )?;
        }
    }

    Ok(())
}
