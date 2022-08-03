use crate::{
    common::config::{Diagnostic, Process, VmDiagnostic},
    parse::ParseConfig,
};
use codespan_reporting::diagnostic::{Label, Severity};
use eventree_wrapper::parser::{self, ExpectedKind};
use std::borrow::Cow;
use std::fmt;
use text_size::{TextRange, TextSize};

#[derive(Debug, Clone)]
pub enum ParseError {
    NestedFunction {
        outer_func_range: TextRange,
        inner_func_range: TextRange,
    },
    NoEntryPoint,
}

fn format_expected(expected: &ExpectedKind<ParseConfig>) -> Cow<'static, str> {
    match expected {
        ExpectedKind::Named(name) => Cow::Borrowed(*name),
        ExpectedKind::Unnamed(one) => Cow::Borrowed(one.as_str()),
        ExpectedKind::AnyUnnamed(set) => {
            assert!(!set.is_empty());
            let mut kinds = set.kinds();
            if set.len() == 1 {
                Cow::Borrowed(kinds.next().unwrap().as_str())
            } else if set.len() == 2 {
                let (first, second) = (kinds.next().unwrap(), kinds.next().unwrap());
                Cow::Owned(format!("{} or {}", first.as_str(), second.as_str()))
            } else {
                let mut buf = String::new();
                while kinds.len() > 1 {
                    let kind = kinds.next().unwrap();
                    buf.push_str(kind.as_str());
                    buf.push_str(", ");
                }
                let last = kinds.next().unwrap();
                buf.push_str("or ");
                buf.push_str(last.as_str());
                Cow::Owned(buf)
            }
        }
    }
}

impl VmDiagnostic for parser::ParseError<ParseConfig> {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn to_diagnostic(&self, process: &Process) -> Diagnostic {
        match self {
            Self::Unexpected {
                expected,
                found,
                range,
            } => {
                let expected = format_expected(expected);
                Diagnostic::error()
                    .with_message("unexpected token")
                    .with_labels(vec![Label::primary(process.file_id(), *range)
                        .with_message(format!(
                            "expected {expected}, found {}",
                            found.as_str()
                        ))])
            }
            Self::Missing { expected, offset } => {
                let expected = format_expected(expected);
                let range = TextRange::at(*offset, TextSize::from(1));
                Diagnostic::error()
                    .with_message("missing token")
                    .with_labels(vec![Label::primary(process.file_id(), range)
                        .with_message(format!("expected {expected}"))])
            }
            Self::User(err) => err.to_diagnostic(process),
        }
    }
}

impl VmDiagnostic for ParseError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn to_diagnostic(&self, process: &Process) -> Diagnostic {
        match self {
            Self::NestedFunction {
                outer_func_range: outer,
                inner_func_range: inner,
            } => Diagnostic::error()
                .with_message("nested functions are not supported")
                .with_labels(vec![
                    Label::primary(process.file_id(), *inner)
                        .with_message("nested function starts here"),
                    Label::secondary(process.file_id(), *outer)
                        .with_message("enclosing function starts here"),
                ])
                .with_notes(vec!["functions must be closed using `end`".to_string()]),
            Self::NoEntryPoint => Diagnostic::error()
                .with_message("no entry point found")
                .with_notes(vec![
                    "a function named `__entry` must serve as an entry point to the program"
                        .to_string(),
                ]),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NestedFunction {
                inner_func_range: range,
                ..
            } => {
                let (start, end) = (u32::from(range.start()), u32::from(range.end()));
                write!(
                    f,
                    "error at {start}..{end}: nested functions are not allowed"
                )
            }
            Self::NoEntryPoint => {
                write!(
                    f,
                    "error: no entry point found. Assembly file must have a function named \
                that acts as the start of the program."
                )
            }
        }
    }
}
