use clap::Parser;
use rust_minivm::config::{self, RunConfig};

#[derive(Parser)]
struct Cli {
    #[clap(value_parser)]
    file_path: Option<String>,
    #[clap(long)]
    debug: bool,
}

fn run(source: &str, config: RunConfig) {
    let process = config::Process {
        source,
        config,
    };
    rust_minivm::run(&process);
}

// fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
//     let mut stdin = std::io::stdin().lines();
//     loop {
//         let mut buf = String::new();
//
//         print!(">>> ");
//         std::io::stdout().flush()?;
//         while let Some(Ok(line)) = stdin.next() {
//             if line.is_empty() {
//                 break;
//             }
//             buf.push_str(&line);
//             buf.push('\n');
//             print!("... ");
//             std::io::stdout().flush()?;
//         }
//
//         if buf.is_empty() {
//             return Ok(());
//         }
//         run(&buf);
//     }
// }

fn run_from_file(path: &str, config: RunConfig) -> Result<(), Box<dyn std::error::Error>> {
    let contents = std::fs::read_to_string(path)?;
    run(&contents, config);
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Cli = Parser::parse();
    let run_config = RunConfig {
        dump_bytecode: args.debug,
        dump_internal_bytecode: args.debug,
        trace_execution: args.debug,
        print_output: args.debug,
    };

    if let Some(path) = args.file_path {
        run_from_file(&path, run_config)
    } else {
        // run_repl(run_config)
        todo!("REPL")
    }
}
