pub mod ast;
mod lexer;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(#[allow(clippy::all)] grammar, r"\src\parse\grammar.rs");

use crate::{
    common::{Interner},
    config::Process,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use text_size::TextSize;

pub use lexer::{Spanned, TokenKind, lex, LexerError, LexerErrorKind};

pub type ParseResult =
    Result<ast::Program, ParseError>;

pub type ParseError = lalrpop_util::ParseError<TextSize, TokenKind, LexerError>;

pub fn parse(source: &str, interner: &mut Interner) -> ParseResult {
    let tokens = lex(source);
    grammar::ProgramParser::new().parse(source, interner, tokens)
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;
    use super::*;
    use expect_test::{Expect, expect};

    fn check(source: &str, expect: Expect) {
        let mut interner = Interner::new();
        let ret = parse(source, &mut interner).unwrap();
        let mut buf = String::new();
        for ast::Function { name, instructions } in ret.functions {
            let _ = writeln!(buf, "Func {{ name: {:?} }}", interner.lookup(name));
            for instr in instructions {
                let _ = writeln!(buf, "{instr:?}");
            }
            buf.push_str("End\n\n");
        }
        expect.assert_eq(buf.trim_end());
    }

    #[test]
    fn empty() {
        check("\
func __entry
end",
              expect![r#"
Func { name: "__entry" }
End"#]);
    }

    #[test]
    fn leading_newlines() {
        check("


func __entry
end",
        expect![r#"
Func { name: "__entry" }
End"#]);
    }

    #[test]
    fn trailing_newlines() {
        check("\
func __entry
end

",
        expect![r#"
Func { name: "__entry" }
End"#]);
    }
}