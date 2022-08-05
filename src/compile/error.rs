use crate::config::{Diagnostic, Process, VmDiagnostic};
use codespan_reporting::diagnostic::{Label, Severity};
use text_size::TextRange;

#[derive(Debug, Clone)]
pub enum CompileError {
    NameNotFound {
        range: TextRange,
    },
    CallLabel {
        def_range: TextRange,
        usages: Vec<TextRange>,
    },
}

impl VmDiagnostic for CompileError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn to_diagnostic(&self, process: &Process) -> Diagnostic {
        let file_id = process.file_id();
        match self {
            Self::NameNotFound { range } => Diagnostic::error()
                .with_message("undefined name")
                .with_labels(vec![
                    Label::primary(file_id, *range).with_message("this name was not defined")
                ]),
            Self::CallLabel { def_range, usages } => {
                let mut labels = vec![
                    Label::secondary(file_id, *def_range).with_message("label is defined here")
                ];
                for &usage in usages {
                    labels
                        .push(Label::primary(file_id, usage).with_message("label is called here"));
                }

                Diagnostic::error()
                    .with_message("only functions can be called")
                    .with_labels(labels)
                    .with_notes(vec!["try converting the label into a function".to_string()])
            }
        }
    }
}
