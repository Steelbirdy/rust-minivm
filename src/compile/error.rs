use text_size::TextRange;

pub enum LowerAstError {
    IntOutOfRange { range: TextRange },
    RegOutOfRange { range: TextRange },
    InvalidEscape { range: TextRange },
    DuplicateName {
        first: TextRange,
        second: TextRange,
    },
    UndefinedName { range: TextRange },
}