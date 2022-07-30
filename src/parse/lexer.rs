use logos::Logos;
use text_size::{TextRange, TextSize};
use std::fmt;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
};
use crate::{
    config::Process,
};

pub type Spanned = Result<(TextSize, TokenKind, TextSize), LexerError>;

pub fn lex(source: &str) -> impl Iterator<Item = Spanned> + '_ {
    assert!(source.len() < u32::MAX as usize, "source file is too long");

    <TokenKind as logos::Logos>::lexer(source)
        .spanned()
        .map(|(kind, span)| {
            let (start, end) = (TextSize::from(span.start as u32), TextSize::from(span.end as u32));
            if kind == TokenKind::Error {
                Err(LexerError {
                    kind: LexerErrorKind::Unrecognized,
                    range: TextRange::new(start, end),
                })
            } else {
                Ok((start, kind, end))
            }
        })
}

#[derive(Debug)]
pub struct LexerError {
    kind: LexerErrorKind,
    range: TextRange,
}

#[derive(Debug)]
pub enum LexerErrorKind {
    Unrecognized,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            LexerErrorKind::Unrecognized => {
                let [start, end] = [self.range.start(), self.range.end()].map(u32::from);
                write!(f, "unrecognized token at {start}..{end}")
            }
        }
    }
}

impl std::error::Error for LexerError {}

#[derive(Logos, Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TokenKind {
    #[token("add")]
    Add,
    #[token("addr")]
    Addr,
    #[token("arr")]
    Arr,
    #[token("call")]
    Call,
    #[token("copy")]
    Copy,
    #[token("decr")]
    Decr,
    #[token("div")]
    Div,
    #[token("end")]
    End,
    #[token("exit")]
    Exit,
    #[token("func")]
    Func,
    #[token("get")]
    Get,
    #[token("incr")]
    Incr,
    #[token("int")]
    Int,
    #[token("jump")]
    Jump,
    #[token("jumpeq")]
    JumpEq,
    #[token("jumpez")]
    JumpEz,
    #[token("jumpge")]
    JumpGe,
    #[token("jumpgt")]
    JumpGt,
    #[token("jumple")]
    JumpLe,
    #[token("jumplt")]
    JumpLt,
    #[token("jumpne")]
    JumpNe,
    #[token("jumpnz")]
    JumpNz,
    #[token("len")]
    Len,
    #[token("mod")]
    Mod,
    #[token("mul")]
    Mul,
    #[token("neg")]
    Neg,
    #[token("putc")]
    Putc,
    #[token("ret")]
    Ret,
    #[token("set")]
    Set,
    #[token("str")]
    Str,
    #[token("sub")]
    Sub,
    #[token("type")]
    Type,

    #[token("<-")]
    Arrow,
    #[token("@")]
    At,
    #[regex(r"-?\d+")]
    IntLiteral,
    #[regex(r":[^\r\n]*")]
    StringLiteral,
    #[regex(r"[a-zA-Z_\.<>][a-zA-Z0-9_\.-<>:]*")]
    Label,
    #[regex(r"r\d+")]
    Register,

    #[regex(r"[ \t]+", logos::skip)]
    Whitespace,
    #[regex(r"#[^\r\n]*", logos::skip)]
    Comment,
    #[regex(r"\r\n|\r|\n")]
    Newline,

    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use std::fmt::Write;

    fn tokenize(source: &str) -> Result<String, Box<dyn std::error::Error>> {
        lex(source).try_fold(String::new(), |mut buf, next| {
                let (start, kind, end) = next?;
                writeln!(buf, "{:?}@{}..{}", kind, u32::from(start), u32::from(end))?;
                Ok(buf)
            })
    }

    fn check(source: &str, expect: Expect) {
        let actual = tokenize(source).unwrap();
        expect.assert_eq(actual.trim_end());
    }

    fn check_error(source: &str, expect: Expect) {
        let actual = tokenize(source).unwrap_err();
        expect.assert_eq(&actual.to_string())
    }

    #[test]
    fn lex_add() {
        check("add", expect!["Add@0..3"]);
    }

    #[test]
    fn lex_addr() {
        check("addr", expect!["Addr@0..4"]);
    }

    #[test]
    fn lex_arr() {
        check("arr", expect!["Arr@0..3"]);
    }

    #[test]
    fn lex_call() {
        check("call", expect!["Call@0..4"]);
    }

    #[test]
    fn lex_copy() {
        check("copy", expect!["Copy@0..4"]);
    }

    #[test]
    fn lex_decr() {
        check("decr", expect!["Decr@0..4"]);
    }

    #[test]
    fn lex_div() {
        check("div", expect!["Div@0..3"]);
    }

    #[test]
    fn lex_end() {
        check("end", expect!["End@0..3"]);
    }

    #[test]
    fn lex_exit() {
        check("exit", expect!["Exit@0..4"]);
    }

    #[test]
    fn lex_func() {
        check("func", expect!["Func@0..4"]);
    }

    #[test]
    fn lex_get() {
        check("get", expect!["Get@0..3"]);
    }

    #[test]
    fn lex_incr() {
        check("incr", expect!["Incr@0..4"]);
    }

    #[test]
    fn lex_int() {
        check("int", expect!["Int@0..3"]);
    }

    #[test]
    fn lex_jump() {
        check("jump", expect!["Jump@0..4"]);
    }

    #[test]
    fn lex_jumpeq() {
        check("jumpeq", expect!["JumpEq@0..6"]);
    }

    #[test]
    fn lex_jumpez() {
        check("jumpez", expect!["JumpEz@0..6"]);
    }

    #[test]
    fn lex_jumpge() {
        check("jumpge", expect!["JumpGe@0..6"]);
    }

    #[test]
    fn lex_jumpgt() {
        check("jumpgt", expect!["JumpGt@0..6"]);
    }

    #[test]
    fn lex_jumple() {
        check("jumple", expect!["JumpLe@0..6"]);
    }

    #[test]
    fn lex_jumplt() {
        check("jumplt", expect!["JumpLt@0..6"]);
    }

    #[test]
    fn lex_jumpne() {
        check("jumpne", expect!["JumpNe@0..6"]);
    }

    #[test]
    fn lex_jumpnz() {
        check("jumpnz", expect!["JumpNz@0..6"]);
    }

    #[test]
    fn lex_len() {
        check("len", expect!["Len@0..3"]);
    }

    #[test]
    fn lex_mod() {
        check("mod", expect!["Mod@0..3"]);
    }

    #[test]
    fn lex_mul() {
        check("mul", expect!["Mul@0..3"]);
    }

    #[test]
    fn lex_neg() {
        check("neg", expect!["Neg@0..3"]);
    }

    #[test]
    fn lex_putc() {
        check("putc", expect!["Putc@0..4"]);
    }

    #[test]
    fn lex_ret() {
        check("ret", expect!["Ret@0..3"]);
    }

    #[test]
    fn lex_set() {
        check("set", expect!["Set@0..3"]);
    }

    #[test]
    fn lex_str() {
        check("str", expect!["Str@0..3"]);
    }

    #[test]
    fn lex_sub() {
        check("sub", expect!["Sub@0..3"]);
    }

    #[test]
    fn lex_type() {
        check("type", expect!["Type@0..4"]);
    }

    #[test]
    fn lex_arrow() {
        check("<-", expect!["Arrow@0..2"]);
    }

    #[test]
    fn lex_at() {
        check("@", expect!["At@0..1"]);
    }

    #[test]
    fn lex_intliteral() {
        check("420 -69", expect!["\
IntLiteral@0..3
IntLiteral@4..7"]);
    }

    #[test]
    fn lex_stringliteral() {
        check(":hello, world!", expect!["StringLiteral@0..14"]);
    }

    #[test]
    fn lex_label() {
        check("label", expect!["Label@0..5"]);
    }

    #[test]
    fn lex_label_non_alphabetic() {
        check(".<> ....", expect!["\
Label@0..3
Label@4..8"]);
    }

    #[test]
    fn lex_label_starting_with_r() {
        check("r1x r12345t r.0", expect!["\
Label@0..3
Label@4..11
Label@12..15"])
    }

    #[test]
    fn lex_register() {
        check("r0 r10", expect!["\
Register@0..2
Register@3..6"]);
    }

    #[test]
    fn lex_whitespace() {
        check("   \tadd", expect!["Add@4..7"]);
    }

    #[test]
    fn lex_comment() {
        check("# this is a comment\nadd", expect!["\
Newline@19..20
Add@20..23"]);
    }

    #[test]
    fn lex_newline() {
        check("\n\r\r\n", expect!["Newline@0..1
Newline@1..2
Newline@2..4"]);
    }

    #[test]
    fn lex_error() {
        check_error("$", expect!["unrecognized token at 0..1"]);
    }
}