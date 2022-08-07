use std::collections::hash_map::Entry;
use eventree_wrapper::syntax_tree::{AstNode, AstToken};
use rustc_hash::FxHashMap;
use text_size::{TextRange, TextSize};
use crate::{common::{self, Int, Reg, Key}, compile::{
    error::LowerAstError,
    hir::Instruction,
}, Interner, parse::{
    ast,
    SyntaxTree,
}};
use crate::common::List;
use crate::compile::hir::Function;

pub type LResult<T> = Result<T, LowerAstError>;

pub struct LoweringContext<'a> {
    interner: &'a mut Interner,
    tree: &'a SyntaxTree,
    labels_defined: FxHashMap<Key, LabelInfo>,
    errors: Vec<LowerAstError>,
}

impl<'a> LoweringContext<'a> {
    pub fn new(interner: &'a mut Interner, tree: &'a SyntaxTree) -> Self {
        Self {
            interner,
            tree,
            labels_defined: FxHashMap::default(),
            errors: Vec::new(),
        }
    }

    pub fn finish(mut self) -> Result<List<Function>, List<LowerAstError>> {
        let root = ast::Root::cast(self.tree.root(), self.tree).expect("invalid root");
        self.discover_labels(root);
        todo!()
    }

    fn discover_labels(&mut self, root: ast::Root) {
        for func in root.functions(self.tree) {
            let name = match func.name(self.tree) {
                Some(n) => n,
                None => continue,
            };
            if let Err(error) = self.define_function(name) {
                self.errors.push(error);
            }
            self.discover_labels_in_function(func);
        }
    }

    fn discover_labels_in_function(&mut self, func: ast::Function) {
        for instr in func.instructions(self.tree) {
            if let ast::Instruction::Label(label_def) = instr {
                let name = match label_def.name(self.tree) {
                    Some(n) => n,
                    None => continue,
                };
                if let Err(error) = self.define_label(name) {
                    self.errors.push(error);
                }
            }
        }
    }

    fn define_function(&mut self, lbl: ast::Label) -> LResult<Key> {
        self._define_label(lbl)
    }

    fn define_label(&mut self, lbl: ast::Label) -> LResult<Key> {
        self._define_label(lbl)
    }

    fn _define_label(&mut self, lbl: ast::Label) -> LResult<Key> {
        let key = self.interner.intern(lbl.text(self.tree));
        let range = lbl.range(self.tree);
        match self.labels_defined.entry(key) {
            Entry::Vacant(vacant) => {
                vacant.insert(LabelInfo { range });
                Ok(key)
            }
            Entry::Occupied(occupied) => {
                let first = occupied.get().range;
                Err(LowerAstError::DuplicateName {
                    first,
                    second: range,
                })
            }
        }
    }
}

struct LabelInfo {
    range: TextRange,
}

#[derive(Copy, Clone)]
enum IntOrReg {
    Int(<ast::IntLiteral as ToValue>::Output),
    Reg(<ast::Register as ToValue>::Output),
}

enum LblOrReg {
    Lbl(<ast::Label as ToValue>::Output),
    Reg(<ast::Register as ToValue>::Output),
}

pub trait ToValue {
    type Output;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output>;
}

impl ToValue for ast::IntLiteral {
    type Output = Int;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        let text = self.text(ctx.tree);
        match text.parse::<Int>() {
            Ok(v) if v <= common::INT_MAX || v >= common::INT_MIN => Ok(v),
            _ => Err(LowerAstError::IntOutOfRange { range: self.range(ctx.tree) }),
        }
    }
}

impl ToValue for ast::CharLiteral {
    type Output = Int;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        let text = self.text(ctx.tree);
        let start = self.range(ctx.tree).start();
        let mut chars = unescape_string(
            &text[1..text.len() - 1],
            start + TextSize::from(1),
        );

        let ch = chars.next().expect("bug in lexer: invalid character literal")?;
        assert!(chars.next().is_none(), "bug in lexer: too many characters in character literal");

        Ok(Int::from(u32::from(ch)))
    }
}

impl ToValue for ast::StringLiteral {
    type Output = Key;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        let text = self.text(ctx.tree);
        let start = self.range(ctx.tree).start();
        let string = unescape_string(
            &text[1..text.len() - 1],
            start + TextSize::from(1),
        )
        .fold(String::new(), |mut buf, char| {
            buf.push(match char {
                Ok(ch) => ch,
                Err(error) => {
                    ctx.errors.push(error);
                    '?'
                }
            });
            buf
        });
        Ok(ctx.interner.intern(&string))
    }
}

impl ToValue for ast::Register {
    type Output = Reg;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        self
            .text(ctx.tree)[1..]
            .parse::<Reg>()
            .map_err(|_| LowerAstError::RegOutOfRange { range: self.range(ctx.tree) })
    }
}

impl ToValue for ast::Label {
    type Output = Key;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        let name = self.text(ctx.tree);
        let key = ctx.interner.intern(name);
        ctx.labels_defined.get(&key)
            .map(|_| key)
            .ok_or_else(|| LowerAstError::UndefinedName { range: self.range(ctx.tree) })
    }
}

impl ToValue for ast::IntOrReg {
    type Output = IntOrReg;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        match self {
            Self::Char(ch) => ch.to_value(ctx).map(IntOrReg::Int),
            Self::Int(int) => int.to_value(ctx).map(IntOrReg::Int),
            Self::Reg(reg) => reg.to_value(ctx).map(IntOrReg::Reg),
        }
    }
}

impl ToValue for ast::LblOrReg {
    type Output = LblOrReg;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        match self {
            Self::Lbl(lbl) => lbl.to_value(ctx).map(LblOrReg::Lbl),
            Self::Reg(reg) => reg.to_value(ctx).map(LblOrReg::Reg),
        }
    }
}

impl ToValue for ast::CharOrInt {
    type Output = <ast::IntLiteral as ToValue>::Output;

    fn to_value(self, ctx: &mut LoweringContext) -> LResult<Self::Output> {
        match self {
            Self::Char(ch) => ch.to_value(ctx),
            Self::Int(int) => int.to_value(ctx),
        }
    }
}

pub trait Lower {
    fn lower(self, ctx: &mut LoweringContext) -> LResult<()>;
}

fn unescape_string(str: &str, start: TextSize) -> impl Iterator<Item = LResult<char>> + '_ {
    struct Iter<'a> {
        offset: TextSize,
        one: TextSize,
        chars: std::str::Chars<'a>,
    }

    impl Iterator for Iter<'_> {
        type Item = LResult<char>;

        fn next(&mut self) -> Option<Self::Item> {
            self.offset += self.one;
            let mut next = self.chars.next()?;
            if next == '\\' {
                let second = if let Some(x) = self.chars.next() {
                    self.offset += self.one;
                    x
                } else {
                    return Some(Err(LowerAstError::InvalidEscape {
                        range: TextRange::at(self.offset, self.one),
                    }));
                };
                next = match second {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    's' => ' ',
                    '\\' => '\\',
                    _ => return Some(Err(LowerAstError::InvalidEscape {
                        range: TextRange::at(self.offset - self.one, self.one + self.one),
                    })),
                }
            }
            Some(Ok(next))
        }
    }

    Iter {
        offset: start,
        one: TextSize::from(1),
        chars: str.chars(),
    }
}