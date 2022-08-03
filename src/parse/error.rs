use std::fmt;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub enum ParseError {
    NestedFunction { range: TextRange },
    NoEntryPoint,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NestedFunction { range } => {
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
