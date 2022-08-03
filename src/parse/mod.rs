pub mod ast;
mod error;
mod lexer;
mod parser;

pub use ast::validate;
pub use error::ParseError;
pub use lexer::{lex, TokenKind};

pub type SyntaxTree = eventree_wrapper::eventree::SyntaxTree<ParseConfig>;

use eventree_wrapper::{eventree::TreeConfig, parser::TokenSet};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ParseConfig {}

impl eventree_wrapper::parser::ParseConfig for ParseConfig {
    type Error = ParseError;

    const ERROR_NODE_KIND: Self::NodeKind = ast::NodeKind::Error;

    fn is_trivia(kind: &Self::TokenKind) -> bool {
        const TRIVIA: &[TokenKind] = &[TokenKind::Comment, TokenKind::Whitespace];
        TRIVIA.contains(kind)
    }

    fn default_recovery_set() -> TokenSet<Self::TokenKind> {
        lazy_static::lazy_static! {
            static ref DEFAULT_RECOVERY_SET: TokenSet<TokenKind> = {
                TokenSet::new([TokenKind::LineBreak])
            };
        }
        *DEFAULT_RECOVERY_SET
    }
}

impl TreeConfig for ParseConfig {
    type NodeKind = ast::NodeKind;
    type TokenKind = TokenKind;
}

pub type ParseResult = eventree_wrapper::parser::ParseResult<ParseConfig>;

pub fn parse(source: &str) -> ParseResult {
    let tokens = lex(source);
    parser::Parser::parse(source, &tokens, parser::assembly)
}

#[cfg(test)]
mod tests {
    use super::ast::*;
    use super::*;
    use eventree_wrapper::syntax_tree::{AstNode, AstToken};
    use expect_test::{expect, Expect};

    fn check(source: &str, expect: Expect) -> ParseResult {
        let ret = parse(source);
        expect.assert_debug_eq(&ret);
        ret
    }

    #[test]
    fn empty() {
        let result = check(
            "",
            expect![
                r#"
Root@0..0
"#
            ],
        );
        let tree = result.syntax_tree();
        let _root = Root::cast(tree.root(), tree).unwrap();
    }

    #[test]
    fn empty_entry_point() {
        let result = check(
            "\
func __entry
end",
            expect![
                r#"
Root@0..16
  Function@0..16
    Func@0..4 "func"
    Whitespace@4..5 " "
    Label@5..12 "__entry"
    LineBreak@12..13 "\n"
    End@13..16 "end"
"#
            ],
        );
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.functions(tree).count(), 1);
        let entry_point = root.entry_point(tree).unwrap();
        let name = entry_point.name(tree).unwrap();
        assert_eq!(name.text(tree), "__entry");
        assert!(entry_point.instructions(tree).next().is_none());
    }

    #[test]
    fn leading_newlines() {
        check(
            "


func __entry
end",
            expect![
                r#"
Root@0..19
  LineBreak@0..3 "\n\n\n"
  Function@3..19
    Func@3..7 "func"
    Whitespace@7..8 " "
    Label@8..15 "__entry"
    LineBreak@15..16 "\n"
    End@16..19 "end"
"#
            ],
        );
    }

    #[test]
    fn trailing_newlines() {
        check(
            "\
func __entry
end

",
            expect![
                r#"
Root@0..18
  Function@0..16
    Func@0..4 "func"
    Whitespace@4..5 " "
    Label@5..12 "__entry"
    LineBreak@12..13 "\n"
    End@13..16 "end"
  LineBreak@16..18 "\n\n"
"#
            ],
        );
    }

    #[test]
    fn function_with_instructions() {
        let result = check(
            "\
func __entry
  r0 <- call main
  exit
end",
            expect![[r#"
Root@0..41
  Function@0..41
    Func@0..4 "func"
    Whitespace@4..5 " "
    Label@5..12 "__entry"
    LineBreak@12..13 "\n"
    Whitespace@13..15 "  "
    InstrCall@15..30
      Register@15..17 "r0"
      Whitespace@17..18 " "
      Arrow@18..20 "<-"
      Whitespace@20..21 " "
      Call@21..25 "call"
      Whitespace@25..26 " "
      Label@26..30 "main"
    LineBreak@30..31 "\n"
    Whitespace@31..33 "  "
    InstrExit@33..37
      Exit@33..37 "exit"
    LineBreak@37..38 "\n"
    End@38..41 "end"
"#]],
        );
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        let entry_point = root.entry_point(tree).unwrap();
        let mut instructions = entry_point.instructions(tree);

        let call = match instructions.next().unwrap() {
            Instruction::Call(c) => c,
            _ => unreachable!(),
        };
        assert!(matches!(call.to(tree), Some(Register(_))));
        assert_eq!(call.func(tree).unwrap().text(tree), "main");
        assert!(call.args(tree).next().is_none());

        assert!(matches!(instructions.next(), Some(Instruction::Exit(_))));
        assert!(instructions.next().is_none());
    }

    #[test]
    fn two_functions() {
        check(
            "\
func __entry
  r0 <- call main
end

func main
  exit
end",
            expect![[r#"
Root@0..56
  Function@0..34
    Func@0..4 "func"
    Whitespace@4..5 " "
    Label@5..12 "__entry"
    LineBreak@12..13 "\n"
    Whitespace@13..15 "  "
    InstrCall@15..30
      Register@15..17 "r0"
      Whitespace@17..18 " "
      Arrow@18..20 "<-"
      Whitespace@20..21 " "
      Call@21..25 "call"
      Whitespace@25..26 " "
      Label@26..30 "main"
    LineBreak@30..31 "\n"
    End@31..34 "end"
  LineBreak@34..36 "\n\n"
  Function@36..56
    Func@36..40 "func"
    Whitespace@40..41 " "
    Label@41..45 "main"
    LineBreak@45..46 "\n"
    Whitespace@46..48 "  "
    InstrExit@48..52
      Exit@48..52 "exit"
    LineBreak@52..53 "\n"
    End@53..56 "end"
"#]],
        );
    }

    #[test]
    fn binary_ops() {
        check(
            "\
func __entry
    r0 <- add 2 1
    r0 <- sub r0 0
    r0 <- mul r0 1
    r0 <- div r0 1
    r0 <- mod r0 1
end",
            expect![[r#"
  Root@0..110
    Function@0..110
      Func@0..4 "func"
      Whitespace@4..5 " "
      Label@5..12 "__entry"
      LineBreak@12..13 "\n"
      Whitespace@13..17 "    "
      InstrAdd@17..30
        Register@17..19 "r0"
        Whitespace@19..20 " "
        Arrow@20..22 "<-"
        Whitespace@22..23 " "
        Add@23..26 "add"
        Whitespace@26..27 " "
        IntLiteral@27..28 "2"
        Whitespace@28..29 " "
        IntLiteral@29..30 "1"
      LineBreak@30..31 "\n"
      Whitespace@31..35 "    "
      InstrSub@35..49
        Register@35..37 "r0"
        Whitespace@37..38 " "
        Arrow@38..40 "<-"
        Whitespace@40..41 " "
        Sub@41..44 "sub"
        Whitespace@44..45 " "
        Register@45..47 "r0"
        Whitespace@47..48 " "
        IntLiteral@48..49 "0"
      LineBreak@49..50 "\n"
      Whitespace@50..54 "    "
      InstrMul@54..68
        Register@54..56 "r0"
        Whitespace@56..57 " "
        Arrow@57..59 "<-"
        Whitespace@59..60 " "
        Mul@60..63 "mul"
        Whitespace@63..64 " "
        Register@64..66 "r0"
        Whitespace@66..67 " "
        IntLiteral@67..68 "1"
      LineBreak@68..69 "\n"
      Whitespace@69..73 "    "
      InstrDiv@73..87
        Register@73..75 "r0"
        Whitespace@75..76 " "
        Arrow@76..78 "<-"
        Whitespace@78..79 " "
        Div@79..82 "div"
        Whitespace@82..83 " "
        Register@83..85 "r0"
        Whitespace@85..86 " "
        IntLiteral@86..87 "1"
      LineBreak@87..88 "\n"
      Whitespace@88..92 "    "
      InstrMod@92..106
        Register@92..94 "r0"
        Whitespace@94..95 " "
        Arrow@95..97 "<-"
        Whitespace@97..98 " "
        Mod@98..101 "mod"
        Whitespace@101..102 " "
        Register@102..104 "r0"
        Whitespace@104..105 " "
        IntLiteral@105..106 "1"
      LineBreak@106..107 "\n"
      End@107..110 "end"
"#]],
        );
    }
}
