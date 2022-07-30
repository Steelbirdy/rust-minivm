use crate::{
    common::{Addr, ByteWriter, Int, Interner, Key, Push, Reg},
    compile::bytecode::Bytecode,
    parse::ast::{Function, Instruction, Program, Target, Val},
};
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
    program: &'a Program,
    interner: &'a Interner,
    num_funcs: u32,
    num_regs: Reg,
    asm: Vec<Asm>,
}

impl<'a> Assembler<'a> {
    pub fn new(program: &'a Program, interner: &'a Interner) -> Self {
        Self {
            program,
            interner,
            num_funcs: 0,
            num_regs: 0,
            asm: Vec::new(),
        }
    }

    pub fn finish(mut self) -> Box<[u8]> {
        self.push_op(Bytecode::Jump);
        self.push(Asm::LblRef(Key::entry_point()));
        for func in &self.program.functions {
            self.num_regs = 0;
            self.write_function(func);
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

    fn write_function(&mut self, func: &Function) {
        self.push_op(Bytecode::Func);
        self.push(Asm::FuncEndRef(self.num_funcs));
        let asm_num_regs = self.push_delayed_reg();
        self.push(Asm::LblLoc(func.name));

        for instr in &func.instructions {
            match instr {
                Instruction::Exit => {
                    self.push_op(Bytecode::Exit);
                }
                Instruction::Lbl { name } => {
                    self.push(Asm::LblLoc(*name));
                }
                Instruction::Copy { to, from } => {
                    self.push_op(Bytecode::Copy);
                    self.push_reg(*from);
                    self.push_reg(*to);
                }
                Instruction::Jump { target } => {
                    self.push_op(match target {
                        Target::Reg(_) => Bytecode::DJump,
                        Target::Lbl(_) => Bytecode::Jump,
                    });
                    self.push_target(*target);
                }
                Instruction::JumpEz { val, target } => match val {
                    Val::Reg(reg) => {
                        self.push_op(Bytecode::JumpEz);
                        self.push_reg(*reg);
                        self.push(Asm::LblRef(*target));
                    }
                    Val::Int(0) => {
                        self.push_op(Bytecode::Jump);
                        self.push(Asm::LblRef(*target));
                    }
                    Val::Int(_) => {}
                },
                Instruction::JumpNz { val, target } => match val {
                    Val::Reg(reg) => {
                        self.push_op(Bytecode::JumpNz);
                        self.push_reg(*reg);
                        self.push(Asm::LblRef(*target));
                    }
                    Val::Int(0) => {}
                    Val::Int(_) => {
                        self.push_op(Bytecode::Jump);
                        self.push(Asm::LblRef(*target));
                    }
                },
                Instruction::JumpLt { lhs, rhs, target } => {
                    self.push_op(match (lhs, rhs) {
                        (Val::Reg(_), Val::Reg(_)) => Bytecode::JumpLtRR,
                        (Val::Reg(_), Val::Int(_)) => Bytecode::JumpLtRI,
                        (Val::Int(_), Val::Reg(_)) => Bytecode::JumpLtIR,
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            if lhs < rhs {
                                self.push_op(Bytecode::Jump);
                                self.push(Asm::LblRef(*target));
                            }
                            continue;
                        }
                    });
                    self.push_val(*lhs);
                    self.push_val(*rhs);
                    self.push(Asm::LblRef(*target));
                }
                Instruction::JumpLe { lhs, rhs, target } => {
                    self.push_op(match (lhs, rhs) {
                        (Val::Reg(_), Val::Reg(_)) => Bytecode::JumpLeRR,
                        (Val::Reg(_), Val::Int(_)) => Bytecode::JumpLeRI,
                        (Val::Int(_), Val::Reg(_)) => Bytecode::JumpLeIR,
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            if lhs <= rhs {
                                self.push_op(Bytecode::Jump);
                                self.push(Asm::LblRef(*target));
                            }
                            continue;
                        }
                    });
                    self.push_val(*lhs);
                    self.push_val(*rhs);
                    self.push(Asm::LblRef(*target));
                }
                Instruction::JumpGt { lhs, rhs, target } => {
                    self.push_op(match (lhs, rhs) {
                        (Val::Reg(_), Val::Reg(_)) => Bytecode::JumpLtRR,
                        (Val::Reg(_), Val::Int(_)) => Bytecode::JumpLtIR,
                        (Val::Int(_), Val::Reg(_)) => Bytecode::JumpLtRI,
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            if lhs > rhs {
                                self.push_op(Bytecode::Jump);
                                self.push(Asm::LblRef(*target));
                            }
                            continue;
                        }
                    });
                    self.push_val(*rhs);
                    self.push_val(*lhs);
                    self.push(Asm::LblRef(*target));
                }
                Instruction::JumpGe { lhs, rhs, target } => {
                    self.push_op(match (lhs, rhs) {
                        (Val::Reg(_), Val::Reg(_)) => Bytecode::JumpLeRR,
                        (Val::Reg(_), Val::Int(_)) => Bytecode::JumpLeIR,
                        (Val::Int(_), Val::Reg(_)) => Bytecode::JumpLeRI,
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            if lhs >= rhs {
                                self.push_op(Bytecode::Jump);
                                self.push(Asm::LblRef(*target));
                            }
                            continue;
                        }
                    });
                    self.push_val(*rhs);
                    self.push_val(*lhs);
                    self.push(Asm::LblRef(*target));
                }
                Instruction::JumpEq { lhs, rhs, target } => {
                    let (op, lhs, rhs) = match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => (Bytecode::JumpEqRR, lhs, rhs),
                        (rhs @ Val::Reg(_), lhs @ Val::Int(_))
                        | (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => (Bytecode::JumpEqIR, lhs, rhs),
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            if lhs == rhs {
                                self.push_op(Bytecode::Jump);
                                self.push(Asm::LblRef(*target));
                            }
                            continue;
                        }
                    };
                    self.push_op(op);
                    self.push_val(*lhs);
                    self.push_val(*rhs);
                    self.push(Asm::LblRef(*target));
                }
                Instruction::JumpNe { lhs, rhs, target } => {
                    let (op, lhs, rhs) = match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => (Bytecode::JumpNeRR, lhs, rhs),
                        (rhs @ Val::Reg(_), lhs @ Val::Int(_))
                        | (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => (Bytecode::JumpNeIR, lhs, rhs),
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            if lhs != rhs {
                                self.push_op(Bytecode::Jump);
                                self.push(Asm::LblRef(*target));
                            }
                            continue;
                        }
                    };
                    self.push_op(op);
                    self.push_val(*lhs);
                    self.push_val(*rhs);
                    self.push(Asm::LblRef(*target));
                }
                Instruction::Call { to, target, args } => {
                    let (op_func, opv) = match target {
                        Target::Reg(_) => {
                            (Bytecode::dcall as fn(usize) -> Bytecode, Bytecode::DCallV)
                        }
                        Target::Lbl(_) => (Bytecode::call as _, Bytecode::CallV),
                    };
                    if args.len() > Bytecode::MAX_INLINE_ARGS {
                        self.push_op(opv);
                        self.push_target(*target);
                        self.push_int(args.len().try_into().unwrap());
                    } else {
                        let op = op_func(args.len());
                        self.push_op(op);
                        self.push_target(*target);
                    }
                    for arg in args {
                        self.push_reg(*arg);
                    }
                    self.push_reg(*to);
                }
                Instruction::Addr { to, of } => {
                    self.push_op(Bytecode::Addr);
                    self.push(Asm::LblRef(*of));
                    self.push_reg(*to);
                }
                Instruction::Ret { val } => {
                    self.push_op(match val {
                        Val::Reg(_) => Bytecode::RetR,
                        Val::Int(_) => Bytecode::RetI,
                    });
                    self.push_val(*val);
                }
                Instruction::Int { to, int } => {
                    self.push_op(Bytecode::Int);
                    self.push_int(*int);
                    self.push_reg(*to);
                }
                Instruction::Str { to, str } => {
                    self.push_op(Bytecode::Str);
                    let str = self.interner.lookup(*str);

                    let str_len = self.push_delayed_int();
                    let mut len = 0;
                    for ch in unescape_string(str) {
                        self.push_int(u32::from(ch).into());
                        len += 1;
                    }
                    str_len.finalize(len);
                    self.push_reg(*to);
                }
                Instruction::Add { to, lhs, rhs } => {
                    match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::AddRR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Reg(reg), Val::Int(0)) | (Val::Int(0), Val::Reg(reg)) => {
                            if reg == to {
                                continue;
                            }
                            self.push_op(Bytecode::Copy);
                            self.push_reg(*reg);
                        }
                        (Val::Reg(reg), Val::Int(1)) | (Val::Int(1), Val::Reg(reg))
                            if reg == to =>
                        {
                            self.push_op(Bytecode::Incr);
                        }
                        (Val::Reg(reg), Val::Int(-1)) | (Val::Int(-1), Val::Reg(reg))
                            if reg == to =>
                        {
                            self.push_op(Bytecode::Decr);
                        }
                        (rhs @ Val::Reg(_), lhs @ Val::Int(_))
                        | (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::AddIR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            let out = lhs.checked_add(*rhs).expect("int is out of range");
                            self.push_op(Bytecode::Int);
                            self.push_int(out);
                        }
                    }
                    self.push_reg(*to);
                }
                Instruction::Sub { to, lhs, rhs } => {
                    match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::SubRR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Reg(reg), Val::Int(0)) => {
                            if reg == to {
                                continue;
                            }
                            self.push_op(Bytecode::Copy);
                            self.push_reg(*reg);
                        }
                        (Val::Int(0), Val::Reg(reg)) => {
                            self.push_op(Bytecode::Neg);
                            self.push_reg(*reg);
                        }
                        (Val::Reg(reg), Val::Int(1)) if reg == to => {
                            self.push_op(Bytecode::Decr);
                        }
                        (Val::Reg(reg), Val::Int(-1)) if reg == to => {
                            self.push_op(Bytecode::Incr);
                        }
                        (lhs @ Val::Reg(_), rhs @ Val::Int(_)) => {
                            self.push_op(Bytecode::SubRI);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::SubIR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            let out = lhs.checked_sub(*rhs).expect("int is out of range");
                            self.push_op(Bytecode::Int);
                            self.push_int(out);
                        }
                    }
                    self.push_reg(*to);
                }
                Instruction::Mul { to, lhs, rhs } => {
                    match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::MulRR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Reg(_), Val::Int(0)) | (Val::Int(0), Val::Reg(_)) => {
                            self.push_op(Bytecode::Int);
                            self.push_int(0);
                        }
                        (Val::Reg(reg), Val::Int(1)) | (Val::Int(1), Val::Reg(reg)) => {
                            if reg == to {
                                continue;
                            }
                            self.push_op(Bytecode::Copy);
                            self.push_reg(*reg);
                        }
                        (Val::Reg(reg), Val::Int(-1)) | (Val::Int(-1), Val::Reg(reg)) => {
                            self.push_op(Bytecode::Neg);
                            self.push_reg(*reg);
                        }
                        (rhs @ Val::Reg(_), lhs @ Val::Int(_))
                        | (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::MulIR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            let out = lhs.checked_mul(*rhs).expect("int is out of range");
                            self.push_op(Bytecode::Int);
                            self.push_int(out);
                        }
                    }
                    self.push_reg(*to);
                }
                Instruction::Div { to, lhs, rhs } => {
                    match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::DivRR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Reg(_), Val::Int(0)) => {
                            // TODO: handle 0 / 0
                            panic!("division by 0")
                        }
                        (Val::Int(0), Val::Reg(_)) => {
                            // TODO: handle 0 / 0
                            self.push_op(Bytecode::Int);
                            self.push_int(0);
                        }
                        (Val::Reg(reg), Val::Int(1)) => {
                            if reg == to {
                                continue;
                            }
                            self.push_op(Bytecode::Copy);
                            self.push_reg(*reg);
                        }
                        (Val::Reg(reg), Val::Int(-1)) => {
                            self.push_op(Bytecode::Neg);
                            self.push_reg(*reg);
                        }
                        (lhs @ Val::Reg(_), rhs @ Val::Int(_)) => {
                            self.push_op(Bytecode::DivRI);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::DivIR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            let out = lhs.checked_div(*rhs).unwrap_or_else(|| {
                                if *rhs == 0 {
                                    panic!("division by 0");
                                } else {
                                    panic!("int is out of range");
                                }
                            });
                            self.push_op(Bytecode::Int);
                            self.push_int(out);
                        }
                    }
                    self.push_reg(*to);
                }
                Instruction::Mod { to, lhs, rhs } => {
                    match (lhs, rhs) {
                        (lhs @ Val::Reg(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::ModRR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Reg(_), Val::Int(0)) => {
                            panic!("cannot compute x % 0")
                        }
                        (Val::Int(0), Val::Reg(_)) => {
                            // TODO: Handle 0 % 0
                            self.push_op(Bytecode::Int);
                            self.push_int(0);
                        }
                        (Val::Reg(_), Val::Int(1 | -1)) => {
                            self.push_op(Bytecode::Int);
                            self.push_int(0);
                        }
                        (lhs @ Val::Reg(_), rhs @ Val::Int(_)) => {
                            self.push_op(Bytecode::ModRI);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (lhs @ Val::Int(_), rhs @ Val::Reg(_)) => {
                            self.push_op(Bytecode::ModIR);
                            self.push_val(*lhs);
                            self.push_val(*rhs);
                        }
                        (Val::Int(lhs), Val::Int(rhs)) => {
                            let out = lhs.checked_rem(*rhs).unwrap_or_else(|| {
                                if *rhs == 0 {
                                    panic!("cannot compute x % 0");
                                } else {
                                    panic!("int is out of range");
                                }
                            });
                            self.push_op(Bytecode::Int);
                            self.push_int(out);
                        }
                    }
                    self.push_reg(*to);
                }
                Instruction::Neg { to, rhs } => {
                    match rhs {
                        Val::Reg(reg) => {
                            self.push_op(Bytecode::Neg);
                            self.push_reg(*reg);
                        }
                        Val::Int(int) => {
                            self.push_op(Bytecode::Int);
                            self.push_int(-*int);
                        }
                    }
                    self.push_reg(*to);
                }
                Instruction::Incr { reg } => {
                    self.push_op(Bytecode::Incr);
                    self.push_reg(*reg);
                }
                Instruction::Decr { reg } => {
                    self.push_op(Bytecode::Decr);
                    self.push_reg(*reg);
                }
                Instruction::Arr { to, len } => {
                    self.push_op(match len {
                        Val::Reg(_) => Bytecode::ArrR,
                        Val::Int(_) => Bytecode::ArrI,
                    });
                    self.push_val(*len);
                    self.push_reg(*to);
                }
                Instruction::Get { to, arr, idx } => {
                    self.push_op(match idx {
                        Val::Reg(_) => Bytecode::GetR,
                        Val::Int(_) => Bytecode::GetI,
                    });
                    self.push_reg(*arr);
                    self.push_val(*idx);
                    self.push_reg(*to);
                }
                Instruction::Set { arr, idx, val } => {
                    self.push_op(match (idx, val) {
                        (Val::Reg(_), Val::Reg(_)) => Bytecode::SetRR,
                        (Val::Reg(_), Val::Int(_)) => Bytecode::SetRI,
                        (Val::Int(_), Val::Reg(_)) => Bytecode::SetIR,
                        (Val::Int(_), Val::Int(_)) => Bytecode::SetII,
                    });
                    self.push_reg(*arr);
                    self.push_val(*idx);
                    self.push_val(*val);
                }
                Instruction::Len { to, arr } => {
                    self.push_op(Bytecode::Len);
                    self.push_reg(*arr);
                    self.push_reg(*to);
                }
                Instruction::Type { to, obj } => {
                    self.push_op(Bytecode::Type);
                    self.push_reg(*obj);
                    self.push_reg(*to);
                }
                Instruction::Putc { val } => {
                    self.push_op(match val {
                        Val::Reg(_) => Bytecode::PutcR,
                        Val::Int(_) => Bytecode::PutcI,
                    });
                    self.push_val(*val);
                }
            }
        }

        self.push(Asm::FuncEndLoc(self.num_funcs));
        asm_num_regs.finalize(self.num_regs + 1);
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
                Asm::LblRef(_) => {
                    loc += std::mem::size_of::<Addr>();
                }
                Asm::LblLoc(lbl) => {
                    labels.insert(*lbl, loc.try_into().unwrap());
                }
                Asm::FuncEndRef(_) => {
                    loc += std::mem::size_of::<Addr>();
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

    fn push(&mut self, asm: Asm) {
        self.asm.push(asm);
    }

    fn push_op(&mut self, op: Bytecode) {
        self.asm.push(Asm::Op(op));
    }

    fn push_reg(&mut self, reg: Reg) {
        self.num_regs = self.num_regs.max(reg);
        self.asm.push(Asm::Reg(reg));
    }

    fn push_val(&mut self, val: Val) {
        match val {
            Val::Reg(reg) => self.push_reg(reg),
            Val::Int(long) => self.push_int(long),
        }
    }

    fn push_target(&mut self, target: Target) {
        match target {
            Target::Reg(reg) => self.push_reg(reg),
            Target::Lbl(lbl) => self.push(Asm::LblRef(lbl)),
        }
    }

    fn push_int(&mut self, int: Int) {
        self.push(Asm::Int(int));
    }

    fn push_delayed_int(&mut self) -> Delayed<Int> {
        let ret = Rc::new(Cell::new(0));
        self.push(Asm::DelayedInt(Rc::clone(&ret)));
        Delayed(ret)
    }

    fn push_delayed_reg(&mut self) -> Delayed<Reg> {
        let ret = Rc::new(Cell::new(0));
        self.push(Asm::DelayedReg(Rc::clone(&ret)));
        Delayed(ret)
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
