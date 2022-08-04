use crate::{
    common::{Addr, ByteWriter, Int, Interner, Key, Push, Reg},
    compile::bytecode::Bytecode,
    parse::{ast, SyntaxTree},
};
use eventree_wrapper::syntax_tree::{AstNode, AstToken};
use rustc_hash::FxHashMap;
use std::cell::Cell;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Asm {
    Op(Bytecode),
    Int(Int),
    Reg(Reg),
    LblRef(Key),
    LblLoc(Key),
    FuncEndRef(u32),
    FuncEndLoc(u32),
    DelayedInt(Rc<Cell<Int>>),
    DelayedReg(Rc<Cell<Reg>>),
}

pub struct Assembler<'a> {
    tree: &'a SyntaxTree,
    interner: &'a mut Interner,
    num_funcs: u32,
    num_regs: Reg,
    asm: Vec<Asm>,
}

impl<'a> Assembler<'a> {
    pub fn new(program: &'a SyntaxTree, interner: &'a mut Interner) -> Self {
        Self {
            tree: program,
            interner,
            num_funcs: 0,
            num_regs: 0,
            asm: Vec::new(),
        }
    }

    pub fn finish(mut self) -> Box<[u8]> {
        let root = ast::Root::cast(self.tree.root(), self.tree).unwrap();

        self.push(Bytecode::Jump);
        self.push(Asm::LblRef(Key::entry_point()));
        for func in root.functions(self.tree) {
            self.num_regs = 0;
            self.push(func);
            self.num_funcs += 1;
        }
        self.patch_delayed();
        self.link()
    }

    fn patch_delayed(&mut self) {
        for instr in &mut self.asm {
            match instr {
                Asm::DelayedInt(ref shared) => {
                    let int = shared.get();
                    *instr = Asm::Int(int);
                }
                Asm::DelayedReg(ref shared) => {
                    let reg = shared.get();
                    *instr = Asm::Reg(reg);
                }
                _ => {}
            }
        }
    }

    fn intern_label(&mut self, label: ast::Label) -> Key {
        let text = label.text(self.tree);
        self.interner.intern(text)
    }

    fn link(&self) -> Box<[u8]> {
        let mut func_ends = FxHashMap::<u32, Addr>::default();
        let mut labels = FxHashMap::<Key, Addr>::default();

        let mut loc = 0;
        for instr in &self.asm {
            match instr {
                Asm::Op(_) => {
                    loc += std::mem::size_of::<Bytecode>();
                }
                Asm::Int(_) => {
                    loc += std::mem::size_of::<Int>();
                }
                Asm::Reg(_) => {
                    loc += std::mem::size_of::<Reg>();
                }
                Asm::LblRef(_) | Asm::FuncEndRef(_) => {
                    loc += std::mem::size_of::<Addr>();
                }
                Asm::LblLoc(lbl) => {
                    labels.insert(*lbl, loc.try_into().unwrap());
                }
                Asm::FuncEndLoc(index) => {
                    func_ends.insert(*index, loc.try_into().unwrap());
                }
                Asm::DelayedInt(_) | Asm::DelayedReg(_) => unreachable!(),
            }
        }

        let mut code = ByteWriter::default();
        for instr in &self.asm {
            match instr {
                Asm::Op(opcode) => {
                    code.push(*opcode);
                }
                Asm::Int(int) => {
                    code.push(*int);
                }
                Asm::Reg(reg) => {
                    code.push(*reg);
                }
                Asm::LblRef(lbl) => {
                    code.push(labels[lbl]);
                }
                Asm::LblLoc(_) => {}
                Asm::FuncEndRef(index) => {
                    code.push(func_ends[index]);
                }
                Asm::FuncEndLoc(_) => {}
                Asm::DelayedInt(_) | Asm::DelayedReg(_) => unreachable!(),
            }
        }

        code.into_inner()
    }

    fn push<T: Assemble>(&mut self, value: T) {
        value.assemble(self);
    }

    fn push_op(&mut self, op: Bytecode) {
        self.asm.push(Asm::Op(op));
    }

    fn push_reg(&mut self, reg: Reg) {
        self.num_regs = self.num_regs.max(reg);
        self.asm.push(Asm::Reg(reg));
    }

    fn push_int(&mut self, int: Int) {
        self.asm.push(Asm::Int(int));
    }

    fn push_delayed_int(&mut self) -> Delayed<Int> {
        let ret = Rc::new(Cell::new(0));
        self.asm.push(Asm::DelayedInt(Rc::clone(&ret)));
        Delayed(ret)
    }

    fn push_delayed_reg(&mut self) -> Delayed<Reg> {
        let ret = Rc::new(Cell::new(0));
        self.asm.push(Asm::DelayedReg(Rc::clone(&ret)));
        Delayed(ret)
    }

    fn push_int_or_reg(&mut self, val: IntOrReg) {
        match val {
            IntOrReg::Int(int) => self.push_int(int),
            IntOrReg::Reg(reg) => self.push_reg(reg),
        }
    }

    fn push_lbl_or_reg(&mut self, val: LblOrReg) {
        match val {
            LblOrReg::Lbl(lbl) => self.asm.push(Asm::LblRef(lbl)),
            LblOrReg::Reg(reg) => self.push_reg(reg),
        }
    }
}

struct Delayed<T>(Rc<Cell<T>>);

impl<T: Copy> Delayed<T> {
    pub fn finalize(self, value: T) {
        self.0.set(value);
    }
}

fn unescape_string(str: &str) -> impl Iterator<Item = char> + '_ {
    struct Iter<'a>(std::str::Chars<'a>);
    impl Iterator for Iter<'_> {
        type Item = char;

        fn next(&mut self) -> Option<char> {
            let next = self.0.next()?;
            Some(if next == '\\' {
                match self.0.next().expect("invalid escape sequence: lone '\\'") {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    's' => ' ',
                    '\\' => '\\',
                    x => panic!("invalid escape sequence '\\{x}'"),
                }
            } else {
                next
            })
        }
    }
    Iter(str.chars())
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum IntOrReg {
    Int(Int),
    Reg(Reg),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum LblOrReg {
    Lbl(Key),
    Reg(Reg),
}

macro_rules! args {
    ($this:ident, $asm:ident; $($arg:ident),+) => {
        ($($this.$arg($asm.tree)?.to_value($asm)),+)
    };
}

trait ToValue {
    type Output;

    fn to_value(self, asm: &mut Assembler) -> Self::Output;
}

impl ToValue for ast::CharLiteral {
    type Output = Int;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        let text = self.text(asm.tree);
        let mut unescaped = unescape_string(text);
        let char = unescaped.next().unwrap();
        assert!(unescaped.next().is_none());
        u32::from(char).into()
    }
}

impl ToValue for ast::IntLiteral {
    type Output = Int;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        self.text(asm.tree).parse().unwrap()
    }
}

impl ToValue for ast::Register {
    type Output = Reg;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        self.text(asm.tree)[1..].parse().unwrap()
    }
}

impl ToValue for ast::Label {
    type Output = Key;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        asm.intern_label(self)
    }
}

impl ToValue for ast::IntOrReg {
    type Output = IntOrReg;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        match self {
            Self::Char(ch) => IntOrReg::Int(ch.to_value(asm)),
            Self::Int(int) => IntOrReg::Int(int.to_value(asm)),
            Self::Reg(reg) => IntOrReg::Reg(reg.to_value(asm)),
        }
    }
}

impl ToValue for ast::LblOrReg {
    type Output = LblOrReg;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        match self {
            Self::Lbl(label) => LblOrReg::Lbl(label.to_value(asm)),
            Self::Reg(reg) => LblOrReg::Reg(reg.to_value(asm)),
        }
    }
}

impl ToValue for ast::CharOrInt {
    type Output = Int;

    fn to_value(self, asm: &mut Assembler) -> Self::Output {
        match self {
            Self::Char(ch) => ch.to_value(asm),
            Self::Int(int) => int.to_value(asm),
        }
    }
}

trait Assemble {
    fn assemble(self, asm: &mut Assembler) -> Option<()>;
}

impl Assemble for ast::Function {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let name = asm.intern_label(self.name(asm.tree)?);
        asm.push(Bytecode::Func);
        asm.push(Asm::FuncEndRef(asm.num_funcs));
        let num_regs = asm.push_delayed_reg();
        asm.push(Asm::LblLoc(name));

        for instr in self.instructions(asm.tree) {
            asm.push(instr);
        }

        asm.push(Asm::FuncEndLoc(asm.num_funcs));
        num_regs.finalize(asm.num_regs + 1);
        Some(())
    }
}

impl Assemble for ast::Instruction {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        match self {
            Self::Label(inner) => inner.assemble(asm),
            Self::Add(inner) => inner.assemble(asm),
            Self::Addr(inner) => inner.assemble(asm),
            Self::Arr(inner) => inner.assemble(asm),
            Self::Call(inner) => inner.assemble(asm),
            Self::Decr(inner) => inner.assemble(asm),
            Self::Div(inner) => inner.assemble(asm),
            Self::Exit(inner) => inner.assemble(asm),
            Self::Get(inner) => inner.assemble(asm),
            Self::Incr(inner) => inner.assemble(asm),
            Self::Int(inner) => inner.assemble(asm),
            Self::Jump(inner) => inner.assemble(asm),
            Self::JumpEq(inner) => inner.assemble(asm),
            Self::JumpEz(inner) => inner.assemble(asm),
            Self::JumpGe(inner) => inner.assemble(asm),
            Self::JumpGt(inner) => inner.assemble(asm),
            Self::JumpLe(inner) => inner.assemble(asm),
            Self::JumpLt(inner) => inner.assemble(asm),
            Self::JumpNe(inner) => inner.assemble(asm),
            Self::JumpNz(inner) => inner.assemble(asm),
            Self::Len(inner) => inner.assemble(asm),
            Self::Mod(inner) => inner.assemble(asm),
            Self::Mul(inner) => inner.assemble(asm),
            Self::Neg(inner) => inner.assemble(asm),
            Self::Putc(inner) => inner.assemble(asm),
            Self::Reg(inner) => inner.assemble(asm),
            Self::Ret(inner) => inner.assemble(asm),
            Self::Set(inner) => inner.assemble(asm),
            Self::Str(inner) => inner.assemble(asm),
            Self::Sub(inner) => inner.assemble(asm),
            Self::Type(inner) => inner.assemble(asm),
        }
    }
}

impl Assemble for Int {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push_int(self);
        Some(())
    }
}

impl Assemble for Reg {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push_reg(self);
        Some(())
    }
}

impl Assemble for Key {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push(Asm::LblRef(self));
        Some(())
    }
}

impl Assemble for IntOrReg {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push_int_or_reg(self);
        Some(())
    }
}

impl Assemble for LblOrReg {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push_lbl_or_reg(self);
        Some(())
    }
}

impl Assemble for Bytecode {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push_op(self);
        Some(())
    }
}

impl Assemble for Asm {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.asm.push(self);
        Some(())
    }
}

impl Assemble for ast::LabelDef {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let name = asm.intern_label(self.name(asm.tree)?);
        asm.push(Asm::LblLoc(name));
        Some(())
    }
}

impl Assemble for ast::InstrAdd {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, lhs, rhs) = args![self, asm; to, lhs, rhs];
        match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::AddRR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(0)) | (IntOrReg::Int(0), IntOrReg::Reg(reg)) => {
                if reg == to {
                    return Some(());
                }
                asm.push(Bytecode::Reg);
                asm.push(reg);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) | (IntOrReg::Int(1), IntOrReg::Reg(reg))
                if reg == to =>
            {
                asm.push(Bytecode::Incr);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) | (IntOrReg::Int(-1), IntOrReg::Reg(reg))
                if reg == to =>
            {
                asm.push(Bytecode::Decr);
            }
            (rhs @ IntOrReg::Reg(_), lhs @ IntOrReg::Int(_))
            | (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::AddIR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let out = lhs.checked_add(rhs).expect("int is out of range");
                asm.push(Bytecode::Int);
                asm.push(out);
            }
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrAddr {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, label) = args!(self, asm; to, label);
        asm.push(Bytecode::Addr);
        asm.push(label);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrArr {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, len) = args!(self, asm; to, len);
        asm.push(match len {
            IntOrReg::Reg(_) => Bytecode::ArrR,
            IntOrReg::Int(_) => Bytecode::ArrI,
        });
        asm.push(len);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrCall {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, func) = args!(self, asm; to, func);
        asm.push(match func {
            LblOrReg::Lbl(_) => Bytecode::Call,
            LblOrReg::Reg(_) => Bytecode::DCall,
        });
        asm.push(func);
        let args: Vec<_> = self.args(asm.tree).map(|arg| arg.to_value(asm)).collect();
        asm.push::<Int>(args.len().try_into().unwrap());
        for arg in args {
            asm.push(arg);
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrDecr {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let reg = self.reg(asm.tree)?.to_value(asm);
        asm.push(Bytecode::Decr);
        asm.push(reg);
        Some(())
    }
}

impl Assemble for ast::InstrDiv {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, lhs, rhs) = args!(self, asm; to, lhs, rhs);
        match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::DivRR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Reg(_), IntOrReg::Int(0)) => {
                // TODO: handle 0 / 0
                panic!("division by 0")
            }
            (IntOrReg::Int(0), IntOrReg::Reg(_)) => {
                // TODO: handle 0 / 0
                asm.push(Bytecode::Int);
                asm.push::<Int>(0);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) => {
                if reg == to {
                    return Some(());
                }
                asm.push(Bytecode::Reg);
                asm.push(reg);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) => {
                asm.push(Bytecode::Neg);
                asm.push(reg);
            }
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Int(_)) => {
                asm.push(Bytecode::DivRI);
                asm.push(lhs);
                asm.push(rhs);
            }
            (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::DivIR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let out = lhs.checked_div(rhs).unwrap_or_else(|| {
                    if rhs == 0 {
                        panic!("division by 0");
                    } else {
                        panic!("int is out of range");
                    }
                });
                asm.push(Bytecode::Int);
                asm.push(out);
            }
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrExit {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        asm.push(Bytecode::Exit);
        Some(())
    }
}

impl Assemble for ast::InstrGet {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, arr, idx) = args!(self, asm; to, arr, idx);
        asm.push(match idx {
            IntOrReg::Reg(_) => Bytecode::GetR,
            IntOrReg::Int(_) => Bytecode::GetI,
        });
        asm.push(arr);
        asm.push(idx);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrIncr {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let reg = args!(self, asm; reg);
        asm.push(Bytecode::Incr);
        asm.push(reg);
        Some(())
    }
}

impl Assemble for ast::InstrInt {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, int) = args!(self, asm; to, value);
        asm.push(Bytecode::Int);
        asm.push(int);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrJump {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let addr = args!(self, asm; addr);
        asm.push(match addr {
            LblOrReg::Reg(_) => Bytecode::DJump,
            LblOrReg::Lbl(_) => Bytecode::Jump,
        });
        asm.push(addr);
        Some(())
    }
}

impl Assemble for ast::InstrJumpEq {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (lhs, rhs, addr) = args!(self, asm; lhs, rhs, addr);
        let (op, lhs, rhs) = match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => (Bytecode::JumpEqRR, lhs, rhs),
            (rhs @ IntOrReg::Reg(_), lhs @ IntOrReg::Int(_))
            | (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => (Bytecode::JumpEqIR, lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if lhs == rhs {
                    asm.push(Bytecode::Jump);
                    asm.push(Asm::LblRef(addr));
                }
                return Some(());
            }
        };
        asm.push(op);
        asm.push(lhs);
        asm.push(rhs);
        asm.push(Asm::LblRef(addr));
        Some(())
    }
}

impl Assemble for ast::InstrJumpEz {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (cond, addr) = args!(self, asm; cond, addr);
        match cond {
            IntOrReg::Reg(reg) => {
                asm.push(Bytecode::JumpEz);
                asm.push(reg);
                asm.push(Asm::LblRef(addr));
            }
            IntOrReg::Int(0) => {
                asm.push(Bytecode::Jump);
                asm.push(Asm::LblRef(addr));
            }
            IntOrReg::Int(_) => {}
        }
        Some(())
    }
}

impl Assemble for ast::InstrJumpGe {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (lhs, rhs, addr) = args!(self, asm; lhs, rhs, addr);
        asm.push(match (lhs, rhs) {
            (IntOrReg::Reg(_), IntOrReg::Reg(_)) => Bytecode::JumpLeRR,
            (IntOrReg::Reg(_), IntOrReg::Int(_)) => Bytecode::JumpLeIR,
            (IntOrReg::Int(_), IntOrReg::Reg(_)) => Bytecode::JumpLeRI,
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if lhs >= rhs {
                    asm.push(Bytecode::Jump);
                    asm.push(Asm::LblRef(addr));
                }
                return Some(());
            }
        });
        asm.push(rhs);
        asm.push(lhs);
        asm.push(Asm::LblRef(addr));
        Some(())
    }
}

impl Assemble for ast::InstrJumpGt {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (lhs, rhs, addr) = args!(self, asm; lhs, rhs, addr);
        asm.push(match (lhs, rhs) {
            (IntOrReg::Reg(_), IntOrReg::Reg(_)) => Bytecode::JumpLtRR,
            (IntOrReg::Reg(_), IntOrReg::Int(_)) => Bytecode::JumpLtIR,
            (IntOrReg::Int(_), IntOrReg::Reg(_)) => Bytecode::JumpLtRI,
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if lhs > rhs {
                    asm.push(Bytecode::Jump);
                    asm.push(Asm::LblRef(addr));
                }
                return Some(());
            }
        });
        asm.push(rhs);
        asm.push(lhs);
        asm.push(Asm::LblRef(addr));
        Some(())
    }
}

impl Assemble for ast::InstrJumpLe {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (lhs, rhs, addr) = args!(self, asm; lhs, rhs, addr);
        asm.push(match (lhs, rhs) {
            (IntOrReg::Reg(_), IntOrReg::Reg(_)) => Bytecode::JumpLeRR,
            (IntOrReg::Reg(_), IntOrReg::Int(_)) => Bytecode::JumpLeRI,
            (IntOrReg::Int(_), IntOrReg::Reg(_)) => Bytecode::JumpLeIR,
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if lhs <= rhs {
                    asm.push(Bytecode::Jump);
                    asm.push(Asm::LblRef(addr));
                }
                return Some(());
            }
        });
        asm.push(lhs);
        asm.push(rhs);
        asm.push(Asm::LblRef(addr));
        Some(())
    }
}

impl Assemble for ast::InstrJumpLt {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (lhs, rhs, addr) = args!(self, asm; lhs, rhs, addr);
        asm.push(match (lhs, rhs) {
            (IntOrReg::Reg(_), IntOrReg::Reg(_)) => Bytecode::JumpLtRR,
            (IntOrReg::Reg(_), IntOrReg::Int(_)) => Bytecode::JumpLtRI,
            (IntOrReg::Int(_), IntOrReg::Reg(_)) => Bytecode::JumpLtIR,
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if lhs < rhs {
                    asm.push(Bytecode::Jump);
                    asm.push(Asm::LblRef(addr));
                }
                return Some(());
            }
        });
        asm.push(lhs);
        asm.push(rhs);
        asm.push(Asm::LblRef(addr));
        Some(())
    }
}

impl Assemble for ast::InstrJumpNe {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (lhs, rhs, addr) = args!(self, asm; lhs, rhs, addr);
        let (op, lhs, rhs) = match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => (Bytecode::JumpNeRR, lhs, rhs),
            (rhs @ IntOrReg::Reg(_), lhs @ IntOrReg::Int(_))
            | (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => (Bytecode::JumpNeIR, lhs, rhs),
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                if lhs != rhs {
                    asm.push(Bytecode::Jump);
                    asm.push(Asm::LblRef(addr));
                }
                return Some(());
            }
        };
        asm.push(op);
        asm.push(lhs);
        asm.push(rhs);
        asm.push(Asm::LblRef(addr));
        Some(())
    }
}

impl Assemble for ast::InstrJumpNz {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (cond, addr) = args!(self, asm; cond, addr);
        match cond {
            IntOrReg::Reg(reg) => {
                asm.push(Bytecode::JumpNz);
                asm.push(reg);
                asm.push(Asm::LblRef(addr));
            }
            IntOrReg::Int(0) => {}
            IntOrReg::Int(_) => {
                asm.push(Bytecode::Jump);
                asm.push(Asm::LblRef(addr));
            }
        }
        Some(())
    }
}

impl Assemble for ast::InstrLen {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, arr) = args!(self, asm; to, arr);
        asm.push(Bytecode::Len);
        asm.push(arr);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrMod {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, lhs, rhs) = args!(self, asm; to, lhs, rhs);
        match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::ModRR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Reg(_), IntOrReg::Int(0)) => {
                panic!("cannot compute x % 0")
            }
            (IntOrReg::Int(0), IntOrReg::Reg(_)) => {
                // TODO: Handle 0 % 0
                asm.push(Bytecode::Int);
                asm.push::<Int>(0);
            }
            (IntOrReg::Reg(_), IntOrReg::Int(1 | -1)) => {
                asm.push(Bytecode::Int);
                asm.push::<Int>(0);
            }
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Int(_)) => {
                asm.push(Bytecode::ModRI);
                asm.push(lhs);
                asm.push(rhs);
            }
            (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::ModIR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let out = lhs.checked_rem(rhs).unwrap_or_else(|| {
                    if rhs == 0 {
                        panic!("cannot compute x % 0");
                    } else {
                        panic!("int is out of range");
                    }
                });
                asm.push(Bytecode::Int);
                asm.push(out);
            }
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrMul {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, lhs, rhs) = args!(self, asm; to, lhs, rhs);
        match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::MulRR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Reg(_), IntOrReg::Int(0)) | (IntOrReg::Int(0), IntOrReg::Reg(_)) => {
                asm.push(Bytecode::Int);
                asm.push::<Int>(0);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) | (IntOrReg::Int(1), IntOrReg::Reg(reg)) => {
                if reg == to {
                    return Some(());
                }
                asm.push(Bytecode::Reg);
                asm.push(reg);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) | (IntOrReg::Int(-1), IntOrReg::Reg(reg)) => {
                asm.push(Bytecode::Neg);
                asm.push(reg);
            }
            (rhs @ IntOrReg::Reg(_), lhs @ IntOrReg::Int(_))
            | (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::MulIR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let out = lhs.checked_mul(rhs).expect("int is out of range");
                asm.push(Bytecode::Int);
                asm.push(out);
            }
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrNeg {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, rhs) = args!(self, asm; to, rhs);
        match rhs {
            IntOrReg::Reg(reg) => {
                asm.push(Bytecode::Neg);
                asm.push(reg);
            }
            IntOrReg::Int(int) => {
                asm.push(Bytecode::Int);
                asm.push(-int);
            }
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrPutc {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let char = args!(self, asm; char);
        asm.push(match char {
            IntOrReg::Reg(_) => Bytecode::PutcR,
            IntOrReg::Int(_) => Bytecode::PutcI,
        });
        asm.push(char);
        Some(())
    }
}

impl Assemble for ast::InstrReg {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, from) = args!(self, asm; to, from);
        asm.push(Bytecode::Reg);
        asm.push(from);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrRet {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let ret = args!(self, asm; ret);
        asm.push(match ret {
            IntOrReg::Reg(_) => Bytecode::RetR,
            IntOrReg::Int(_) => Bytecode::RetI,
        });
        asm.push(ret);
        Some(())
    }
}

impl Assemble for ast::InstrSet {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (arr, idx, val) = args!(self, asm; arr, idx, val);
        asm.push(match (idx, val) {
            (IntOrReg::Reg(_), IntOrReg::Reg(_)) => Bytecode::SetRR,
            (IntOrReg::Reg(_), IntOrReg::Int(_)) => Bytecode::SetRI,
            (IntOrReg::Int(_), IntOrReg::Reg(_)) => Bytecode::SetIR,
            (IntOrReg::Int(_), IntOrReg::Int(_)) => Bytecode::SetII,
        });
        asm.push(arr);
        asm.push(idx);
        asm.push(val);
        Some(())
    }
}

impl Assemble for ast::InstrStr {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let to = args!(self, asm; to);
        let str = self.string(asm.tree)?.text(asm.tree);
        asm.push(Bytecode::Str);

        let str_len = asm.push_delayed_int();
        let mut len = 0;
        for ch in unescape_string(&str[1..]) {
            asm.push::<Int>(u32::from(ch).into());
            len += 1;
        }
        str_len.finalize(len);
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrSub {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, lhs, rhs) = args!(self, asm; to, lhs, rhs);
        match (lhs, rhs) {
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::SubRR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(0)) => {
                if reg == to {
                    return Some(());
                }
                asm.push(Bytecode::Reg);
                asm.push(reg);
            }
            (IntOrReg::Int(0), IntOrReg::Reg(reg)) => {
                asm.push(Bytecode::Neg);
                asm.push(reg);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(1)) if reg == to => {
                asm.push(Bytecode::Decr);
            }
            (IntOrReg::Reg(reg), IntOrReg::Int(-1)) if reg == to => {
                asm.push(Bytecode::Incr);
            }
            (lhs @ IntOrReg::Reg(_), rhs @ IntOrReg::Int(_)) => {
                asm.push(Bytecode::SubRI);
                asm.push(lhs);
                asm.push(rhs);
            }
            (lhs @ IntOrReg::Int(_), rhs @ IntOrReg::Reg(_)) => {
                asm.push(Bytecode::SubIR);
                asm.push(lhs);
                asm.push(rhs);
            }
            (IntOrReg::Int(lhs), IntOrReg::Int(rhs)) => {
                let out = lhs.checked_sub(rhs).expect("int is out of range");
                asm.push(Bytecode::Int);
                asm.push(out);
            }
        }
        asm.push(to);
        Some(())
    }
}

impl Assemble for ast::InstrType {
    fn assemble(self, asm: &mut Assembler) -> Option<()> {
        let (to, obj) = args!(self, asm; to, obj);
        asm.push(Bytecode::Type);
        asm.push(obj);
        asm.push(to);
        Some(())
    }
}
