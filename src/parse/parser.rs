use crate::parse::{ast::NodeKind, ParseConfig, ParseError, TokenKind};
use eventree_wrapper::parser::{self, CompletedMarker, SimpleTokens, TokenSet};
use text_size::TextRange;

pub type Parser<'t> = parser::Parser<ParseConfig, &'t SimpleTokens<TokenKind>>;

lazy_static::lazy_static! {
    static ref UNARY_JUMP: TokenSet<TokenKind> = TokenSet::new([
        TokenKind::JumpEz,
        TokenKind::JumpNz,
    ]);
    static ref BINARY_JUMP: TokenSet<TokenKind> = TokenSet::new([
        TokenKind::JumpEq,
        TokenKind::JumpGe,
        TokenKind::JumpGt,
        TokenKind::JumpLe,
        TokenKind::JumpLt,
        TokenKind::JumpNe,
    ]);
    static ref CHAR_OR_INT: TokenSet<TokenKind> = TokenSet::new([
        TokenKind::CharLiteral,
        TokenKind::IntLiteral,
    ]);
    static ref INT_OR_REG: TokenSet<TokenKind> = TokenSet::new([
        TokenKind::CharLiteral,
        TokenKind::IntLiteral,
        TokenKind::Register,
    ]);
    static ref LBL_OR_REG: TokenSet<TokenKind> = TokenSet::new([
        TokenKind::Label,
        TokenKind::Register,
    ]);
}

pub(in crate::parse) fn assembly(p: &mut Parser) {
    let marker = p.start();
    while !p.is_at_end() {
        while p.is_at(TokenKind::LineBreak) {
            p.bump();
        }
        if p.is_at_end() {
            break;
        }
        function(p);
    }
    p.complete(marker, NodeKind::Root);
}

fn function(p: &mut Parser) {
    let marker = p.start();
    p.expect(TokenKind::Func);
    let func_start = p.previous_range();
    p.expect(TokenKind::Label);

    loop {
        p.expect(TokenKind::LineBreak);
        if p.is_at(TokenKind::End) {
            break;
        }
        instruction(p, func_start);
    }
    p.expect(TokenKind::End);
    p.complete(marker, NodeKind::Function);
}

macro_rules! make_instruction_func {
    [$($kind:tt $($expect_kind:ident)? = [$($first:tt $($expect:ident)?),* $(,)?] => $out:ident;)*] => {
        fn instruction(p: &mut Parser, func_start: TextRange) -> Option<CompletedMarker> {
            if p.is_at(TokenKind::Register) {
                register_instruction(p)
            } else if p.is_at(TokenKind::At) {
                let marker = p.start();
                p.bump();
                p.expect(TokenKind::Label);
                Some(p.complete(marker, NodeKind::LabelDef))
            } else if p.is_at(TokenKind::Func) {
                let inner_func_range = p.peek_range().unwrap();
                p.custom_error(ParseError::NestedFunction { outer_func_range: func_start, inner_func_range });
                Some(p.recover())
            }
            $(
            else if make_instruction_func!(@cond p $kind $($expect_kind)?) {
                let marker = p.start();
                p.bump();
                $(
                make_instruction_func!(@action p $first $($expect)?);
                )*
                Some(p.complete(marker, NodeKind::$out))
            }
            )*
            else {
                let _guard = p.expected("instruction name, `@`, register, or `end`");
                p.error()
            }
        }
    };
    [@reg $($kind:tt $($expect_kind:ident)? = [$($first:tt $($expect:ident)?),* $(,)?] => $out:ident;)*] => {
        fn register_instruction(p: &mut Parser) -> Option<CompletedMarker> {
            assert!(p.is_at(TokenKind::Register));
            let marker = p.start();
            p.bump();
            p.expect(TokenKind::Arrow);
            if p.is_at(TokenKind::Call) {
                p.bump();
                p.expect_any(*LBL_OR_REG);
                while p.is_at(TokenKind::Register) {
                    p.bump();
                }
                Some(p.complete(marker, NodeKind::InstrCall))
            }
            $(
            else if make_instruction_func!(@cond p $kind $($expect_kind)?) {
                p.bump();
                $(
                make_instruction_func!(@action p $first $($expect)?);
                )*
                Some(p.complete(marker, NodeKind::$out))
            }
            )*
            else {
                p.complete(marker, NodeKind::Error);
                let _guard = p.expected("instruction");
                p.error()
            }
        }
    };
    (@cond $p:ident *$kind:ident) => {
        $p.is_at_any(*$kind)
    };
    (@cond $p:ident $kind:ident) => {
        $p.is_at(TokenKind::$kind)
    };
    (@action $p:ident *$kind:ident) => {
        $p.expect_any(*$kind)
    };
    (@action $p:ident $kind:ident) => {
        $p.expect(TokenKind::$kind)
    };
}

make_instruction_func![
    Decr = [Register] => InstrDecr;
    Exit = [] => InstrExit;
    Incr = [Register] => InstrIncr;
    Jump = [*LBL_OR_REG] => InstrJump;
    JumpEq = [*INT_OR_REG, *INT_OR_REG, Label] => InstrJumpEq;
    JumpEz = [*INT_OR_REG, Label] => InstrJumpEz;
    JumpGe = [*INT_OR_REG, *INT_OR_REG, Label] => InstrJumpGe;
    JumpGt = [*INT_OR_REG, *INT_OR_REG, Label] => InstrJumpGt;
    JumpLe = [*INT_OR_REG, *INT_OR_REG, Label] => InstrJumpLe;
    JumpLt = [*INT_OR_REG, *INT_OR_REG, Label] => InstrJumpLt;
    JumpNe = [*INT_OR_REG, *INT_OR_REG, Label] => InstrJumpNe;
    JumpNz = [*INT_OR_REG, Label] => InstrJumpNz;
    Putc = [*INT_OR_REG] => InstrPutc;
    Ret = [*INT_OR_REG] => InstrRet;
    Set = [Register, *INT_OR_REG, *INT_OR_REG] => InstrSet;
];

make_instruction_func![@reg
    Add = [*INT_OR_REG, *INT_OR_REG] => InstrAdd;
    Addr = [Label] => InstrAddr;
    Arr = [*INT_OR_REG] => InstrArr;
    Copy = [Register] => InstrCopy;
    Div = [*INT_OR_REG, *INT_OR_REG] => InstrDiv;
    Get = [Register, *INT_OR_REG] => InstrGet;
    Int = [*CHAR_OR_INT] => InstrInt;
    Len = [Register] => InstrLen;
    Mod = [*INT_OR_REG, *INT_OR_REG] => InstrMod;
    Mul = [*INT_OR_REG, *INT_OR_REG] => InstrMul;
    Neg = [*INT_OR_REG] => InstrNeg;
    Str = [StringLiteral] => InstrStr;
    Sub = [*INT_OR_REG, *INT_OR_REG] => InstrSub;
    Type = [Register] => InstrType;
];
