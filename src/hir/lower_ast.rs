use crate::{
    common::{self, Int, Key, List, Reg},
    config,
    hir::{
        BinaryArgs, BinaryOp, BranchKind, Function, HirError, HirErrorKind, Instruction,
        InstructionWithRange, JumpKind, JumpSet, KeyWithRange, LabelInfo, Program, SetArgs,
        UnaryArg, UnaryOp,
    },
    parse::{ast, SyntaxTree},
    Interner,
};
use eventree_wrapper::syntax_tree::{AstNode, AstToken};
use rustc_hash::FxHashMap;
use std::cell::Cell;
use std::collections::hash_map::Entry;
use text_size::{TextRange, TextSize};

pub struct LoweringContext<'a> {
    tree: &'a SyntaxTree,
    interner: &'a mut Interner,
    labels: FxHashMap<Key, LabelInfo>,
    errors: List<HirError>,
    regs_used: Reg,
}

impl<'a> LoweringContext<'a> {
    pub fn new(tree: &'a SyntaxTree, interner: &'a mut Interner) -> Self {
        Self {
            tree,
            interner,
            labels: FxHashMap::default(),
            errors: List::new(),
            regs_used: 1,
        }
    }

    pub fn finish(mut self) -> Result<Program, List<HirError>> {
        let root = ast::Root::cast(self.tree.root(), self.tree).expect("invalid root");
        self.discover_labels(root);

        let functions: FxHashMap<_, _> = root
            .functions(self.tree)
            .filter_map(|func| func.lower(&mut self))
            .map(|func| (func.name.key, func))
            .collect();

        let labels = self.labels;

        assert!(
            labels.values().all(|info| info.index != usize::MAX),
            "label was not indexed"
        );

        if self.errors.is_empty() {
            let program = Program { functions, labels };
            program.trace_jumps();
            Ok(program)
        } else {
            Err(self.errors)
        }
    }

    fn error(&mut self, error: HirError) {
        self.errors.push(error);
    }

    fn get_key(&mut self, lbl: ast::Label) -> Key {
        self.interner.intern(lbl.text(self.tree))
    }

    fn discover_labels(&mut self, root: ast::Root) {
        for func in root.functions(self.tree) {
            self.discover_labels_in_function(func);
        }
    }

    fn discover_labels_in_function(&mut self, func: ast::Function) {
        let func_name = match func.name(self.tree) {
            Some(lbl) => {
                let func = self.get_key(lbl);
                self.define_label(lbl, func);
                self.mark_label_index(func, 0);
                func
            }
            None => return,
        };
        for instr in func.instructions(self.tree) {
            if let ast::Instruction::Label(label_def) = instr {
                if let Some(name) = label_def.name(self.tree) {
                    self.define_label(name, func_name);
                }
            }
        }
    }

    fn define_label(&mut self, lbl: ast::Label, func: Key) -> Option<KeyWithRange> {
        let key = self.interner.intern(lbl.text(self.tree));
        let range = lbl.range(self.tree);
        match self.labels.entry(key) {
            Entry::Vacant(vacant) => {
                vacant.insert(LabelInfo {
                    range,
                    func,
                    index: usize::MAX,
                });
                Some(KeyWithRange { key, range })
            }
            Entry::Occupied(occupied) => {
                let first = occupied.get();
                self.errors.push(HirError {
                    kind: HirErrorKind::DuplicateName {
                        original: first.range,
                    },
                    range,
                });
                None
            }
        }
    }

    fn mark_label_index(&mut self, lbl: Key, index: usize) {
        match self.labels.entry(lbl) {
            Entry::Vacant(_) => panic!("error in `lower_ast`: label not discovered"),
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().index = index;
            }
        }
    }
}

#[derive(Copy, Clone)]
enum IntOrReg {
    Int(<ast::IntLiteral as Lower>::Output),
    Reg(<ast::Register as Lower>::Output),
}

enum LblOrReg {
    Lbl(<ast::Label as Lower>::Output),
    Reg(<ast::Register as Lower>::Output),
}

macro_rules! lower {
    ($this:ident, $ctx:ident; $($arg:ident),+) => {
        ($($this.$arg($ctx.tree).and_then(|x| x.lower($ctx))?),+)
    };
}

trait Lower {
    type Output;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output>;
}

impl Lower for ast::IntLiteral {
    type Output = Int;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let text = self.text(ctx.tree);
        match text.parse::<Int>() {
            Ok(v) if v <= common::INT_MAX || v >= common::INT_MIN => Some(v),
            _ => {
                ctx.error(HirError {
                    kind: HirErrorKind::IntOutOfRange,
                    range: self.range(ctx.tree),
                });
                None
            }
        }
    }
}

impl Lower for ast::CharLiteral {
    type Output = Int;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let text = self.text(ctx.tree);
        let start = self.range(ctx.tree).start();
        let mut chars = unescape_string(&text[1..text.len() - 1], start + TextSize::from(1));

        let ch = match chars
            .next()
            .expect("bug in lexer: invalid character literal")
        {
            Ok(ch) => ch,
            Err(error) => {
                ctx.error(error);
                return None;
            }
        };
        assert!(
            chars.next().is_none(),
            "bug in lexer: too many characters in character literal"
        );
        Some(Int::from(u32::from(ch)))
    }
}

impl Lower for ast::StringLiteral {
    type Output = Key;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let text = self.text(ctx.tree);
        let start = self.range(ctx.tree).start();
        let string = unescape_string(&text[1..text.len() - 1], start + TextSize::from(1)).fold(
            String::new(),
            |mut buf, char| {
                buf.push(match char {
                    Ok(ch) => ch,
                    Err(error) => {
                        ctx.errors.push(error);
                        '?'
                    }
                });
                buf
            },
        );
        Some(ctx.interner.intern(&string))
    }
}

impl Lower for ast::Register {
    type Output = Reg;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let reg = self.text(ctx.tree)[1..].parse::<Reg>().ok();
        match reg {
            Some(reg) if usize::try_from(reg).unwrap() < config::NUM_REGISTERS_PER_FRAME => {
                ctx.regs_used = ctx.regs_used.max(reg + 1);
                Some(reg)
            }
            _ => {
                ctx.error(HirError {
                    kind: HirErrorKind::RegOutOfRange,
                    range: self.range(ctx.tree),
                });
                None
            }
        }
    }
}

impl Lower for ast::Label {
    type Output = KeyWithRange;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let name = self.text(ctx.tree);
        let key = ctx.interner.intern(name);
        let ret = ctx.labels.get(&key).map(|_| key);
        if ret.is_none() {
            ctx.error(HirError {
                kind: HirErrorKind::UndefinedName,
                range: self.range(ctx.tree),
            });
        }
        Some(KeyWithRange {
            key,
            range: self.range(ctx.tree),
        })
    }
}

impl Lower for ast::IntOrReg {
    type Output = IntOrReg;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        match self {
            Self::Char(ch) => ch.lower(ctx).map(IntOrReg::Int),
            Self::Int(int) => int.lower(ctx).map(IntOrReg::Int),
            Self::Reg(reg) => reg.lower(ctx).map(IntOrReg::Reg),
        }
    }
}

impl Lower for ast::LblOrReg {
    type Output = LblOrReg;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        match self {
            Self::Lbl(lbl) => lbl.lower(ctx).map(LblOrReg::Lbl),
            Self::Reg(reg) => reg.lower(ctx).map(LblOrReg::Reg),
        }
    }
}

impl Lower for ast::CharOrInt {
    type Output = <ast::IntLiteral as Lower>::Output;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        match self {
            Self::Char(ch) => ch.lower(ctx),
            Self::Int(int) => int.lower(ctx),
        }
    }
}

impl Lower for ast::Function {
    type Output = Function;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        ctx.regs_used = 0;

        let name = lower!(self, ctx; name);
        let dummy_label = InstructionWithRange {
            inner: Instruction::Label { name },
            range: name.range,
        };
        let body = self.instructions(ctx.tree).enumerate().map(|(i, instr)| {
            let indexed = Indexed {
                index: i,
                value: instr,
            };
            indexed.lower(ctx)
        });

        let instructions: Vec<_> = std::iter::once(Some(dummy_label))
            .chain(body)
            .collect::<Option<_>>()?;
        let mut jumps = Vec::with_capacity(instructions.len());
        jumps.resize(jumps.capacity(), Cell::new(JumpSet::EMPTY));

        Some(Function {
            name,
            instructions,
            jumps,
            regs_used: ctx.regs_used,
        })
    }
}

struct Indexed<T> {
    index: usize,
    value: T,
}

impl Lower for Indexed<ast::Instruction> {
    type Output = InstructionWithRange;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let Indexed {
            index,
            value: instr,
        } = self;

        let inner = match instr {
            ast::Instruction::Label(instr) => {
                let indexed = Indexed {
                    index,
                    value: instr,
                };
                indexed.lower(ctx)
            }
            ast::Instruction::Add(instr) => instr.lower(ctx),
            ast::Instruction::Addr(instr) => instr.lower(ctx),
            ast::Instruction::Arr(instr) => instr.lower(ctx),
            ast::Instruction::BranchBool(instr) => instr.lower(ctx),
            ast::Instruction::BranchEq(instr) => instr.lower(ctx),
            ast::Instruction::BranchLt(instr) => instr.lower(ctx),
            ast::Instruction::Call(instr) => instr.lower(ctx),
            ast::Instruction::Decr(instr) => instr.lower(ctx),
            ast::Instruction::Div(instr) => instr.lower(ctx),
            ast::Instruction::Exit(instr) => instr.lower(ctx),
            ast::Instruction::Get(instr) => instr.lower(ctx),
            ast::Instruction::Incr(instr) => instr.lower(ctx),
            ast::Instruction::Int(instr) => instr.lower(ctx),
            ast::Instruction::Jump(instr) => instr.lower(ctx),
            ast::Instruction::JumpEq(instr) => instr.lower(ctx),
            ast::Instruction::JumpEz(instr) => instr.lower(ctx),
            ast::Instruction::JumpGe(instr) => instr.lower(ctx),
            ast::Instruction::JumpGt(instr) => instr.lower(ctx),
            ast::Instruction::JumpLe(instr) => instr.lower(ctx),
            ast::Instruction::JumpLt(instr) => instr.lower(ctx),
            ast::Instruction::JumpNe(instr) => instr.lower(ctx),
            ast::Instruction::JumpNz(instr) => instr.lower(ctx),
            ast::Instruction::Len(instr) => instr.lower(ctx),
            ast::Instruction::Mod(instr) => instr.lower(ctx),
            ast::Instruction::Mul(instr) => instr.lower(ctx),
            ast::Instruction::Neg(instr) => instr.lower(ctx),
            ast::Instruction::Putc(instr) => instr.lower(ctx),
            ast::Instruction::Reg(instr) => instr.lower(ctx),
            ast::Instruction::Ret(instr) => instr.lower(ctx),
            ast::Instruction::Set(instr) => instr.lower(ctx),
            ast::Instruction::Str(instr) => instr.lower(ctx),
            ast::Instruction::Sub(instr) => instr.lower(ctx),
            ast::Instruction::Type(instr) => instr.lower(ctx),
        }?;
        Some(InstructionWithRange {
            inner,
            range: instr.range(ctx.tree),
        })
    }
}

impl Lower for Indexed<ast::LabelDef> {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let Indexed { index, value: defn } = self;
        let name = lower!(defn, ctx; name);
        ctx.mark_label_index(name.key, index);
        Some(Instruction::Label { name })
    }
}

impl Lower for ast::InstrAdd {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, lhs, rhs) = lower!(self, ctx; to, lhs, rhs);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(0), IntOrReg::Reg(reg)) | (IntOrReg::Reg(reg), IntOrReg::Int(0)) => {
                return if reg == to {
                    None
                } else {
                    Some(Instruction::Reg { to, from: reg })
                };
            }
            (IntOrReg::Int(1), IntOrReg::Reg(reg)) | (IntOrReg::Reg(reg), IntOrReg::Int(1))
                if reg == to =>
            {
                return Some(Instruction::Incr { reg });
            }
            (IntOrReg::Int(-1), IntOrReg::Reg(reg)) | (IntOrReg::Reg(reg), IntOrReg::Int(-1))
                if reg == to =>
            {
                return Some(Instruction::Decr { reg });
            }
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) | (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) => {
                BinaryArgs::IR(lhs, rhs)
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let int = match lhs.checked_add(rhs) {
                    Some(x) if common::int_is_valid(x) => x,
                    _ => {
                        ctx.error(HirError {
                            kind: HirErrorKind::IntOutOfRange,
                            range: self.range(ctx.tree),
                        });
                        return None;
                    }
                };
                return Some(Instruction::Int { to, int });
            }
        };
        Some(Instruction::Binary {
            op: BinaryOp::Add,
            args,
            to,
        })
    }
}

impl Lower for ast::InstrAddr {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, lbl) = lower!(self, ctx; to, label);
        Some(Instruction::Addr { to, lbl })
    }
}

impl Lower for ast::InstrArr {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, len) = lower!(self, ctx; to, len);
        let len = match len {
            IntOrReg::Int(len) => UnaryArg::I(len),
            IntOrReg::Reg(len) => UnaryArg::R(len),
        };
        Some(Instruction::Arr { to, len })
    }
}

impl Lower for ast::InstrBB {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (cond, lbl_false, lbl_true) = lower!(self, ctx; cond, lbl_false, lbl_true);
        Some(match cond {
            IntOrReg::Int(cond) => {
                let lbl = if cond == 0 { lbl_false } else { lbl_true };
                Instruction::Jump {
                    kind: JumpKind::Unconditional,
                    lbl,
                }
            }
            IntOrReg::Reg(cond) => Instruction::Branch {
                kind: BranchKind::Nz { cond },
                lbl_false,
                lbl_true,
            },
        })
    }
}

impl Lower for ast::InstrBEq {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl_false, lbl_true) = lower!(self, ctx; lhs, rhs, lbl_false, lbl_true);
        let kind =
            match (lhs, rhs) {
                (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BranchKind::Eq {
                    args: BinaryArgs::RR(lhs, rhs),
                },
                (IntOrReg::Reg(cond), IntOrReg::Int(0))
                | (IntOrReg::Int(0), IntOrReg::Reg(cond)) => BranchKind::Nz { cond },
                (IntOrReg::Int(lhs), IntOrReg::Reg(rhs))
                | (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) => BranchKind::Eq {
                    args: BinaryArgs::IR(lhs, rhs),
                },
                (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                    let lbl = if lhs == rhs { lbl_true } else { lbl_false };
                    return Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    });
                }
            };
        Some(Instruction::Branch {
            kind,
            lbl_false,
            lbl_true,
        })
    }
}

impl Lower for ast::InstrBLt {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl_false, lbl_true) = lower!(self, ctx; lhs, rhs, lbl_false, lbl_true);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Reg(lhs), IntOrReg::Int(rhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let lbl = if lhs < rhs { lbl_true } else { lbl_false };
                return Some(Instruction::Jump {
                    kind: JumpKind::Unconditional,
                    lbl,
                });
            }
        };
        Some(Instruction::Branch {
            kind: BranchKind::Lt { args },
            lbl_false,
            lbl_true,
        })
    }
}

impl Lower for ast::InstrCall {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, func) = lower!(self, ctx; to, func);
        let args = self
            .args(ctx.tree)
            .map(|arg| arg.lower(ctx))
            .collect::<Option<_>>()?;
        Some(match func {
            LblOrReg::Lbl(func) => Instruction::Call { to, func, args },
            LblOrReg::Reg(func) => Instruction::DCall { to, func, args },
        })
    }
}

impl Lower for ast::InstrDecr {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let reg = lower!(self, ctx; reg);
        Some(Instruction::Decr { reg })
    }
}

impl Lower for ast::InstrDiv {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, lhs, rhs) = lower!(self, ctx; to, lhs, rhs);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Reg(_), IntOrReg::Int(0)) => {
                ctx.error(HirError {
                    kind: HirErrorKind::ZeroDivision,
                    range: self.range(ctx.tree),
                });
                return None;
            }
            // TODO: Is it best to make this assumption given that `rhs` might be 0?
            //  same with the equivalent case for `mod`
            (IntOrReg::Int(0), IntOrReg::Reg(_)) => {
                return Some(Instruction::Int { to, int: 0 });
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) => {
                return if to == reg {
                    None
                } else {
                    Some(Instruction::Reg { to, from: reg })
                };
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) => {
                return Some(Instruction::Unary {
                    op: UnaryOp::Neg,
                    arg: reg,
                    to,
                })
            }
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Reg(lhs), IntOrReg::Int(rhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if rhs == 0 {
                    ctx.error(HirError {
                        kind: HirErrorKind::ZeroDivision,
                        range: self.range(ctx.tree),
                    });
                    return None;
                }
                let int = lhs.checked_div(rhs);
                return match int {
                    Some(int) => Some(Instruction::Int { to, int }),
                    None => {
                        ctx.error(HirError {
                            kind: HirErrorKind::IntOutOfRange,
                            range: self.range(ctx.tree),
                        });
                        None
                    }
                };
            }
        };
        Some(Instruction::Binary {
            op: BinaryOp::Div,
            args,
            to,
        })
    }
}

impl Lower for ast::InstrExit {
    type Output = Instruction;

    fn lower(self, _ctx: &mut LoweringContext) -> Option<Self::Output> {
        Some(Instruction::Exit)
    }
}

impl Lower for ast::InstrGet {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, arr, idx) = lower!(self, ctx; to, arr, idx);
        let idx = match idx {
            IntOrReg::Int(idx) => UnaryArg::I(idx),
            IntOrReg::Reg(idx) => UnaryArg::R(idx),
        };
        Some(Instruction::Get { to, arr, idx })
    }
}

impl Lower for ast::InstrIncr {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let reg = lower!(self, ctx; reg);
        Some(Instruction::Incr { reg })
    }
}

impl Lower for ast::InstrInt {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, int) = lower!(self, ctx; to, value);
        Some(Instruction::Int { to, int })
    }
}

impl Lower for ast::InstrJump {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let lbl = lower!(self, ctx; addr);
        Some(match lbl {
            LblOrReg::Lbl(lbl) => Instruction::Jump {
                kind: JumpKind::Unconditional,
                lbl,
            },
            LblOrReg::Reg(lbl) => Instruction::DJump {
                kind: JumpKind::Unconditional,
                lbl,
            },
        })
    }
}

impl Lower for ast::InstrJumpEq {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl) = lower!(self, ctx; lhs, rhs, addr);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) | (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) => {
                BinaryArgs::IR(lhs, rhs)
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return if lhs == rhs {
                    Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    None
                }
            }
        };
        Some(Instruction::Jump {
            kind: JumpKind::Eq { args },
            lbl,
        })
    }
}

impl Lower for ast::InstrJumpEz {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (cond, lbl) = lower!(self, ctx; cond, addr);
        let kind = match cond {
            IntOrReg::Int(0) => JumpKind::Unconditional,
            IntOrReg::Int(_) => return None,
            IntOrReg::Reg(cond) => JumpKind::Ez { cond },
        };
        Some(Instruction::Jump { kind, lbl })
    }
}

impl Lower for ast::InstrJumpGe {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl) = lower!(self, ctx; lhs, rhs, addr);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(rhs), IntOrReg::Reg(lhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(rhs), IntOrReg::Reg(lhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return if lhs >= rhs {
                    Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    None
                }
            }
        };
        Some(Instruction::Jump {
            kind: JumpKind::Le { args },
            lbl,
        })
    }
}

impl Lower for ast::InstrJumpGt {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl) = lower!(self, ctx; lhs, rhs, addr);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(rhs), IntOrReg::Reg(lhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(rhs), IntOrReg::Reg(lhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return if lhs > rhs {
                    Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    None
                }
            }
        };
        Some(Instruction::Jump {
            kind: JumpKind::Lt { args },
            lbl,
        })
    }
}

impl Lower for ast::InstrJumpLe {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl) = lower!(self, ctx; lhs, rhs, addr);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Reg(lhs), IntOrReg::Int(rhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return if lhs <= rhs {
                    Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    None
                }
            }
        };
        Some(Instruction::Jump {
            kind: JumpKind::Le { args },
            lbl,
        })
    }
}

impl Lower for ast::InstrJumpLt {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl) = lower!(self, ctx; lhs, rhs, addr);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Reg(lhs), IntOrReg::Int(rhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return if lhs < rhs {
                    Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    None
                }
            }
        };
        Some(Instruction::Jump {
            kind: JumpKind::Lt { args },
            lbl,
        })
    }
}

impl Lower for ast::InstrJumpNe {
    type Output = Instruction;

    #[allow(clippy::if_not_else)]
    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (lhs, rhs, lbl) = lower!(self, ctx; lhs, rhs, addr);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) | (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) => {
                BinaryArgs::IR(lhs, rhs)
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return if lhs != rhs {
                    Some(Instruction::Jump {
                        kind: JumpKind::Unconditional,
                        lbl,
                    })
                } else {
                    None
                }
            }
        };
        Some(Instruction::Jump {
            kind: JumpKind::Ne { args },
            lbl,
        })
    }
}

impl Lower for ast::InstrJumpNz {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (cond, lbl) = lower!(self, ctx; cond, addr);
        let kind = match cond {
            IntOrReg::Int(0) => return None,
            IntOrReg::Int(_) => JumpKind::Unconditional,
            IntOrReg::Reg(cond) => JumpKind::Nz { cond },
        };
        Some(Instruction::Jump { kind, lbl })
    }
}

impl Lower for ast::InstrLen {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, arr) = lower!(self, ctx; to, arr);
        Some(Instruction::Len { to, arr })
    }
}

impl Lower for ast::InstrMod {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, lhs, rhs) = lower!(self, ctx; to, lhs, rhs);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Reg(_), IntOrReg::Int(0)) => {
                ctx.error(HirError {
                    kind: HirErrorKind::ZeroDivision,
                    range: self.range(ctx.tree),
                });
                return None;
            }
            (IntOrReg::Int(0), IntOrReg::Reg(_)) | (IntOrReg::Reg(_), IntOrReg::Int(1 | -1)) => {
                return Some(Instruction::Int { to, int: 0 });
            }
            (IntOrReg::Reg(lhs), IntOrReg::Int(rhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if rhs == 0 {
                    ctx.error(HirError {
                        kind: HirErrorKind::ZeroDivision,
                        range: self.range(ctx.tree),
                    });
                    return None;
                }
                return match lhs.checked_rem(rhs) {
                    Some(int) => Some(Instruction::Int { to, int }),
                    None => {
                        ctx.error(HirError {
                            kind: HirErrorKind::IntOutOfRange,
                            range: self.range(ctx.tree),
                        });
                        None
                    }
                };
            }
        };
        Some(Instruction::Binary {
            op: BinaryOp::Mod,
            args,
            to,
        })
    }
}

impl Lower for ast::InstrMul {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, lhs, rhs) = lower!(self, ctx; to, lhs, rhs);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Reg(_), IntOrReg::Int(0)) | (IntOrReg::Int(0), IntOrReg::Reg(_)) => {
                return Some(Instruction::Int { to, int: 0 });
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) | (IntOrReg::Int(1), IntOrReg::Reg(reg)) => {
                return if reg == to {
                    None
                } else {
                    Some(Instruction::Reg { to, from: reg })
                };
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) | (IntOrReg::Int(-1), IntOrReg::Reg(reg)) => {
                return Some(Instruction::Unary {
                    op: UnaryOp::Neg,
                    to,
                    arg: reg,
                })
            }
            (IntOrReg::Reg(rhs), IntOrReg::Int(lhs)) | (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => {
                BinaryArgs::IR(lhs, rhs)
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return match lhs.checked_mul(rhs) {
                    Some(int) => Some(Instruction::Int { to, int }),
                    None => {
                        ctx.error(HirError {
                            kind: HirErrorKind::IntOutOfRange,
                            range: self.range(ctx.tree),
                        });
                        None
                    }
                };
            }
        };
        Some(Instruction::Binary {
            op: BinaryOp::Mul,
            args,
            to,
        })
    }
}

impl Lower for ast::InstrNeg {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, rhs) = lower!(self, ctx; to, rhs);
        Some(match rhs {
            IntOrReg::Int(int) => Instruction::Int { to, int: -int },
            IntOrReg::Reg(arg) => Instruction::Unary {
                op: UnaryOp::Neg,
                to,
                arg,
            },
        })
    }
}

impl Lower for ast::InstrPutc {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let char = lower!(self, ctx; char);
        let arg = match char {
            IntOrReg::Int(char) => {
                if let Ok(ch) = u32::try_from(char) {
                    if char::try_from(ch).is_err() {
                        ctx.error(HirError {
                            kind: HirErrorKind::CharOutOfRange,
                            range: self.range(ctx.tree),
                        });
                        return None;
                    };
                    UnaryArg::I(char)
                } else {
                    ctx.error(HirError {
                        kind: HirErrorKind::CharOutOfRange,
                        range: self.range(ctx.tree),
                    });
                    return None;
                }
            }
            IntOrReg::Reg(char) => UnaryArg::R(char),
        };
        Some(Instruction::Putc { arg })
    }
}

impl Lower for ast::InstrReg {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, from) = lower!(self, ctx; to, from);
        if to == from {
            None
        } else {
            Some(Instruction::Reg { to, from })
        }
    }
}

impl Lower for ast::InstrRet {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let val = lower!(self, ctx; ret);
        let arg = match val {
            IntOrReg::Int(int) => UnaryArg::I(int),
            IntOrReg::Reg(reg) => UnaryArg::R(reg),
        };
        Some(Instruction::Ret { arg })
    }
}

impl Lower for ast::InstrSet {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (arr, idx, val) = lower!(self, ctx; arr, idx, val);
        let args = match (idx, val) {
            (IntOrReg::Reg(idx), IntOrReg::Reg(val)) => SetArgs::RR(idx, val),
            (IntOrReg::Int(idx), IntOrReg::Reg(val)) => SetArgs::IR(idx, val),
            (IntOrReg::Reg(idx), IntOrReg::Int(val)) => SetArgs::RI(idx, val),
            (IntOrReg::Int(idx), IntOrReg::Int(val)) => SetArgs::II(idx, val),
        };
        Some(Instruction::Set {
            arr,
            idx_and_val: args,
        })
    }
}

impl Lower for ast::InstrStr {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let to = lower!(self, ctx; to);
        let string = self.string(ctx.tree)?;
        let text = string.text(ctx.tree);
        let range = string.range(ctx.tree);
        let text = unescape_string(&text[1..], range.start()).fold(String::new(), |mut buf, ch| {
            buf.push(match ch {
                Ok(ch) => ch,
                Err(error) => {
                    ctx.error(error);
                    '?'
                }
            });
            buf
        });
        let str = ctx.interner.intern(&text);
        Some(Instruction::Str {
            to,
            str: KeyWithRange { key: str, range },
        })
    }
}

impl Lower for ast::InstrSub {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, lhs, rhs) = lower!(self, ctx; to, lhs, rhs);
        let args = match (lhs, rhs) {
            (IntOrReg::Reg(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::RR(lhs, rhs),
            (IntOrReg::Reg(reg), IntOrReg::Int(0)) => {
                return if reg == to {
                    None
                } else {
                    Some(Instruction::Reg { to, from: reg })
                };
            }
            (IntOrReg::Int(0), IntOrReg::Reg(reg)) => {
                return Some(Instruction::Unary {
                    op: UnaryOp::Neg,
                    to,
                    arg: reg,
                });
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) if reg == to => {
                return Some(Instruction::Decr { reg });
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) if reg == to => {
                return Some(Instruction::Incr { reg });
            }
            (IntOrReg::Int(lhs), IntOrReg::Reg(rhs)) => BinaryArgs::IR(lhs, rhs),
            (IntOrReg::Reg(lhs), IntOrReg::Int(rhs)) => BinaryArgs::RI(lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                return match lhs.checked_sub(rhs) {
                    Some(x) if common::int_is_valid(x) => Some(Instruction::Int { to, int: x }),
                    _ => {
                        ctx.error(HirError {
                            kind: HirErrorKind::IntOutOfRange,
                            range: self.range(ctx.tree),
                        });
                        None
                    }
                };
            }
        };
        Some(Instruction::Binary {
            op: BinaryOp::Sub,
            args,
            to,
        })
    }
}

impl Lower for ast::InstrType {
    type Output = Instruction;

    fn lower(self, ctx: &mut LoweringContext) -> Option<Self::Output> {
        let (to, obj) = lower!(self, ctx; to, obj);
        Some(Instruction::Type { to, obj })
    }
}

fn unescape_string(
    str: &str,
    start: TextSize,
) -> impl Iterator<Item = Result<char, HirError>> + '_ {
    struct Iter<'a> {
        offset: TextSize,
        one: TextSize,
        chars: std::str::Chars<'a>,
    }

    impl Iterator for Iter<'_> {
        type Item = Result<char, HirError>;

        fn next(&mut self) -> Option<Self::Item> {
            self.offset += self.one;
            let mut next = self.chars.next()?;
            if next == '\\' {
                let second = if let Some(x) = self.chars.next() {
                    self.offset += self.one;
                    x
                } else {
                    return Some(Err(HirError {
                        kind: HirErrorKind::InvalidEscape,
                        range: TextRange::at(self.offset, self.one),
                    }));
                };
                next = match second {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    's' => ' ',
                    '\\' => '\\',
                    _ => {
                        return Some(Err(HirError {
                            kind: HirErrorKind::InvalidEscape,
                            range: TextRange::at(self.offset - self.one, self.one + self.one),
                        }))
                    }
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
