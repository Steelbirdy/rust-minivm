use crate::{config::Process, Diagnostic, VmDiagnostic};
use codespan_reporting::diagnostic::{Label, Severity};
use text_size::TextRange;

pub struct HirError {
    pub range: TextRange,
    pub kind: HirErrorKind,
}

pub enum HirErrorKind {
    CharOutOfRange,
    IntOutOfRange,
    RegOutOfRange,
    InvalidEscape,
    DuplicateName { original: TextRange },
    UndefinedName,
    ZeroDivision,
    UninitRegUse,
}

impl VmDiagnostic for HirError {
    fn severity(&self) -> Severity {
        use HirErrorKind::*;

        match self.kind {
            UninitRegUse => Severity::Warning,
            CharOutOfRange
            | IntOutOfRange
            | RegOutOfRange
            | InvalidEscape
            | DuplicateName { .. }
            | UndefinedName
            | ZeroDivision => Severity::Error,
        }
    }

    fn to_diagnostic(&self, process: &Process) -> Diagnostic {
        use HirErrorKind::*;

        let file_id = process.file_id();
        match self.kind {
            CharOutOfRange => Diagnostic::error()
                .with_message("invalid character")
                .with_labels(vec![
                    Label::primary(file_id, self.range).with_message("value is not valid UTF-8")
                ]),
            IntOutOfRange => Diagnostic::error()
                .with_message("invalid integer")
                .with_labels(vec![
                    Label::primary(file_id, self.range).with_message("integer is out of range")
                ]),
            RegOutOfRange => Diagnostic::error()
                .with_message("invalid register")
                .with_labels(vec![Label::primary(file_id, self.range)
                    .with_message("register number is out of range")]),
            InvalidEscape => Diagnostic::error()
                .with_message("invalid escape sequence")
                .with_labels(vec![Label::primary(file_id, self.range)]),
            DuplicateName { original } => Diagnostic::error()
                .with_message("duplicate name")
                .with_labels(vec![
                    Label::secondary(file_id, original)
                        .with_message("the name is first defined here..."),
                    Label::primary(file_id, self.range).with_message("...but is redefined here"),
                ]),
            UndefinedName => Diagnostic::error()
                .with_message("undefined name")
                .with_labels(vec![
                    Label::primary(file_id, self.range).with_message("name is never defined")
                ]),
            ZeroDivision => Diagnostic::error()
                .with_message("division or modulo by zero")
                .with_labels(vec![
                    Label::primary(file_id, self.range).with_message("operation will always fail")
                ]),
            UninitRegUse => Diagnostic::warning()
                .with_message("use of uninitialized register")
                .with_labels(vec![Label::primary(file_id, self.range)
                    .with_message("register is never initialized")])
                .with_notes(vec!["the register may be a function argument".to_string()]),
        }
    }
}
