use expect_test::Expect;
use rust_minivm::{Value, Error, config::{RunConfig, Process}, Gc, VmDiagnostic};

pub fn check(source: &str, expected: Expect) {
    expected.assert_eq(&run_to_string(source))
}

macro_rules! new_process {
    ($source:ident) => {
        Process::new($source, "main.vasm", &RunConfig {
            dump_bytecode: false,
            print_output: false,
            trace_execution: false,
            dump_internal_bytecode: false,
        })
    };
}

fn run_with_process(process: &Process) -> Result<Value, Vec<Error>> {
    let mut gc = Gc::new();
    let compiled = rust_minivm::compile(&process, &mut gc)?;
    rust_minivm::run(compiled.reader(), gc, &process)
        .map_err(|e| vec![e.into()])
}

pub fn run_to_string(source: &str) -> String {
    let process = new_process!(source);
    match run_with_process(&process) {
        Ok(out) => format!("{out:?}"),
        Err(errors) => {
            let diagnostics = errors.into_iter()
                .map(|e| e.to_diagnostic(&process));

            let mut buf = Vec::<u8>::new();
            let mut writer = codespan_reporting::term::termcolor::NoColor::new(&mut buf);
            let config = codespan_reporting::term::Config::default();

            for diagnostic in diagnostics {
                codespan_reporting::term::emit(
                    &mut writer,
                    &config,
                    process.files(),
                    &diagnostic,
                ).unwrap();
            }

            String::from_utf8(buf).unwrap()
        }
    }
}