use eventree_wrapper::{eventree::SyntaxKind, parser::SimpleTokens};
use logos::Logos;

#[must_use]
pub fn lex(source: &str) -> SimpleTokens<TokenKind> {
    SimpleTokens::tokenize(source)
}

#[derive(Logos, Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum TokenKind {
    #[token("add")]
    Add,
    #[token("addr")]
    Addr,
    #[token("arr")]
    Arr,
    #[token("bb")]
    BB,
    #[token("beq")]
    BEq,
    #[token("blt")]
    BLt,
    #[token("call")]
    Call,
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
    #[token("putchar")]
    Putc,
    #[token("reg")]
    Reg,
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
    #[regex(r"'([^\\']|\\.)'")]
    CharLiteral,
    #[regex(r"[a-zA-Z_\.<>][a-zA-Z0-9_\.-<>:]*")]
    Label,
    #[regex(r"r\d+")]
    Register,

    #[regex(r"[ \t]+")]
    Whitespace,
    #[regex(r"#[^\r\n]*(\r\n|\r|\n)")]
    Comment,
    #[regex(r"(\r\n|\r|\n)+")]
    LineBreak,

    #[error]
    Error,
}

impl TokenKind {
    pub fn as_str(self) -> &'static str {
        use TokenKind::*;
        match self {
            Add => "`add`",
            Addr => "`addr`",
            Arr => "`arr`",
            BB => "`bb`",
            BEq => "`beq`",
            BLt => "`blt`",
            Call => "`call`",
            Decr => "`decr`",
            Div => "`div`",
            End => "`end`",
            Exit => "`exit`",
            Func => "`func`",
            Get => "`get`",
            Incr => "`incr`",
            Int => "`int`",
            Jump => "`jump`",
            JumpEq => "`jumpeq`",
            JumpEz => "`jumpez`",
            JumpGe => "`jumpge`",
            JumpGt => "`jumpgt`",
            JumpLe => "`jumple`",
            JumpLt => "`jumplt`",
            JumpNe => "`jumpne`",
            JumpNz => "`jumpnz`",
            Len => "`len`",
            Mod => "`mod`",
            Mul => "`mul`",
            Neg => "`neg`",
            Putc => "`putchar`",
            Reg => "`reg`",
            Ret => "`ret`",
            Set => "`set`",
            Str => "`str`",
            Sub => "`sub`",
            Type => "`type`",
            Arrow => "`<-`",
            At => "`@`",
            IntLiteral => "integer",
            StringLiteral => "string",
            CharLiteral => "char",
            Label => "label",
            Register => "register",
            Whitespace => "whitespace",
            Comment => "comment",
            LineBreak => "line break",
            Error => "<unknown>",
        }
    }
}

#[allow(unsafe_code)]
unsafe impl SyntaxKind for TokenKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    #[allow(clippy::cast_possible_truncation)]
    unsafe fn from_raw(raw: u16) -> Self {
        std::mem::transmute(raw as u8)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use std::fmt::Write;

    fn tokenize(source: &str) -> String {
        lex(source)
            .iter()
            .fold(String::new(), |mut buf, (range, kind)| {
                let (start, end) = (range.start(), range.end());
                let _ = writeln!(buf, "{:?}@{}..{}", kind, u32::from(start), u32::from(end));
                buf
            })
    }

    fn check(source: &str, expect: Expect) {
        let actual = tokenize(source);
        expect.assert_eq(actual.trim_end());
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
    fn lex_bb() {
        check("bb", expect!["BB@0..2"]);
    }

    #[test]
    fn lex_beq() {
        check("beq", expect!["BEq@0..3"]);
    }

    #[test]
    fn lex_blt() {
        check("blt", expect!["BLt@0..3"]);
    }

    #[test]
    fn lex_call() {
        check("call", expect!["Call@0..4"]);
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
    fn lex_putchar() {
        check("putchar", expect!["Putchar@0..4"]);
    }

    #[test]
    fn lex_reg() {
        check("reg", expect!["Reg@0..4"]);
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
        check(
            "420 -69",
            expect![
                "\
IntLiteral@0..3
Whitespace@3..4
IntLiteral@4..7"
            ],
        );
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
        check(
            ".<> ....",
            expect![
                "\
Label@0..3
Whitespace@3..4
Label@4..8"
            ],
        );
    }

    #[test]
    fn lex_label_starting_with_r() {
        check(
            "r1x r12345t r.0",
            expect![
                "\
Label@0..3
Whitespace@3..4
Label@4..11
Whitespace@11..12
Label@12..15"
            ],
        )
    }

    #[test]
    fn lex_register() {
        check(
            "r0 r10",
            expect![
                "\
Register@0..2
Whitespace@2..3
Register@3..6"
            ],
        );
    }

    #[test]
    fn lex_whitespace() {
        check(
            "   \tadd",
            expect![
                "\
Whitespace@0..4
Add@4..7"
            ],
        );
    }

    #[test]
    fn lex_comment() {
        check(
            "# this is a comment\nadd",
            expect![
                "\
Comment@0..19
LineBreak@19..20
Add@20..23"
            ],
        );
    }

    #[test]
    fn lex_line_break() {
        check(
            "\n\r\r\n",
            expect![
                "\
LineBreak@0..4"
            ],
        );
    }

    #[test]
    fn lex_error() {
        check("$", expect!["Error@0..1"]);
    }
}
