use expect_test::Expect;
use rust_minivm::config::{Process, RunConfig};

pub fn check(source: &str, expected: Expect) {
    expected.assert_eq(&run_to_string(source))
}

macro_rules! new_process {
    ($source:ident) => {
        Process::new(
            $source,
            "main.vasm",
            RunConfig {
                dump_bytecode: false,
                trace_execution: false,
            },
        )
    };
}

pub fn run_to_string(source: &str) -> String {
    let process = new_process!(source);
    match rust_minivm::compile_and_run(&process) {
        Ok(out) => format!("{out:?}"),
        Err(diagnostics) => {
            let mut buf = Vec::<u8>::new();
            let mut writer = codespan_reporting::term::termcolor::NoColor::new(&mut buf);
            let config = codespan_reporting::term::Config::default();

            for diagnostic in diagnostics {
                codespan_reporting::term::emit(&mut writer, &config, process.files(), &diagnostic)
                    .unwrap();
            }

            String::from_utf8(buf).unwrap()
        }
    }
}
