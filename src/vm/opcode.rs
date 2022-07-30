use crate::{
    common::{config, Addr, ByteReader, ByteWriter, FromBytes, Int, Push, Read, Reg, ToBytes, Write},
    compile::Bytecode,
    vm::{self, Gc, Jump, JumpSet, Value},
};
use rustc_hash::FxHashMap;
use std::ops::{Index, IndexMut};

struct Registers(Vec<(bool, Value)>);

fn reg_to_usize(reg: Reg) -> usize {
    usize::try_from(reg).unwrap()
}

impl Registers {
    fn grow(&mut self, new_len: Reg) {
        let new_len = reg_to_usize(new_len);
        if new_len <= self.0.len() {
            return;
        }
        self.0.resize(new_len, (false, Value::int_unchecked(0)));
    }

    fn named(&self, reg: Reg) -> bool {
        self.0[reg_to_usize(reg)].0
    }

    fn set_named(&mut self, reg: Reg, named: bool) {
        self.0[reg_to_usize(reg)].0 = named;
    }
}

impl Index<Reg> for Registers {
    type Output = Value;

    fn index(&self, reg: Reg) -> &Self::Output {
        &self.0[reg_to_usize(reg)].1
    }
}

impl IndexMut<Reg> for Registers {
    fn index_mut(&mut self, reg: Reg) -> &mut Self::Output {
        &mut self.0[reg_to_usize(reg)].1
    }
}

pub fn lower_bytecode(code: ByteReader, jumps: &[JumpSet], gc: &mut Gc) -> Box<[u8]> {
    let mut writer = ByteWriter::default();
    let mut locs = FxHashMap::default();
    let mut froms = FxHashMap::default();

    let mut num_regs = 16;
    let mut registers = Registers(vec![
        (false, Value::int_unchecked(0));
        config::NUM_REGISTERS
    ]);

    macro_rules! take {
        ($($ty:ty),+ $(,)?) => {
            ($(code.take::<$ty>().unwrap()),+)
        };
    }

    macro_rules! write {
        ($($expr:expr),* $(,)?) => {{
            $(writer.push($expr);)*
        }};
    }

    macro_rules! addr_placeholder {
        ($addr:expr) => {{
            froms.insert(writer.len(), $addr);
            writer.push::<Addr>(0);
        }};
    }

    macro_rules! binary_jump_op {
        (rr $op:tt $rr:ident $ir:ident) => {{
            let lhs = code.take::<Reg>().unwrap();
            let rhs = code.take::<Reg>().unwrap();
            let addr = code.take::<Addr>().unwrap();
            if registers.named(lhs) && registers.named(rhs) {
                if registers[lhs] $op registers[rhs] {
                    writer.push(Opcode::Jump);
                    addr_placeholder!(addr);
                }
            } else if registers.named(lhs) {
                writer.push(Opcode::$ir);
                writer.push::<Value>(registers[lhs]);
                writer.push::<Reg>(rhs);
                addr_placeholder!(addr);
            } else if registers.named(rhs) {
                writer.push(Opcode::$ir);
                writer.push::<Value>(registers[rhs]);
                writer.push::<Reg>(lhs);
                addr_placeholder!(addr);
            } else {
                writer.push(Opcode::$rr);
                writer.push::<Reg>(lhs);
                writer.push::<Reg>(rhs);
                addr_placeholder!(addr);
            }
        }};
        (rr $op:tt $rr:ident $ir:ident $ri:ident) => {{
            let lhs = code.take::<Reg>().unwrap();
            let rhs = code.take::<Reg>().unwrap();
            let addr = code.take::<Addr>().unwrap();
            if registers.named(lhs) && registers.named(rhs) {
                if registers[lhs] $op registers[rhs] {
                    writer.push(Opcode::Jump);
                    addr_placeholder!(addr);
                }
            } else if registers.named(lhs) {
                writer.push(Opcode::$ir);
                writer.push::<Value>(registers[lhs]);
                writer.push::<Reg>(rhs);
                addr_placeholder!(addr);
            } else if registers.named(rhs) {
                writer.push(Opcode::$ri);
                writer.push::<Reg>(lhs);
                writer.push::<Value>(registers[rhs]);
                addr_placeholder!(addr);
            } else {
                writer.push(Opcode::$rr);
                writer.push::<Reg>(lhs);
                writer.push::<Reg>(rhs);
                addr_placeholder!(addr);
            }
        }};
        (ir $op:tt $ir:ident) => {{
            let lhs = code.take::<Int>().unwrap();
            let rhs = code.take::<Reg>().unwrap();
            let addr = code.take::<Addr>().unwrap();
            if registers.named(rhs) {
                if lhs $op registers[rhs] {
                    writer.push(Opcode::Jump);
                    addr_placeholder!(addr);
                }
            } else {
                writer.push(Opcode::$ir);
                writer.push::<Value>(Value::int_unchecked(lhs));
                writer.push::<Reg>(rhs);
                addr_placeholder!(addr);
            }
        }};
        (ri $op:tt $ri:ident) => {{
            let lhs = code.take::<Reg>().unwrap();
            let rhs = code.take::<Int>().unwrap();
            let addr = code.take::<Addr>().unwrap();
            if registers.named(lhs) {
                if registers[lhs] $op rhs {
                    writer.push(Opcode::Jump);
                    addr_placeholder!(addr);
                }
            } else {
                writer.push(Opcode::$ri);
                writer.push::<Reg>(lhs);
                writer.push::<Value>(Value::int_unchecked(rhs));
                addr_placeholder!(addr);
            }
        }};
    }

    macro_rules! call_op {
        ($op:ident $(, $num_args:literal)?) => {{
            let addr = code.take::<Addr>().unwrap();
            let args_start = code.offset();
            $(
            for i in 0..$num_args {
                let arg = code
                    .read_at::<Reg>(args_start + i * std::mem::size_of::<Reg>())
                    .unwrap();
                if registers.named(arg) {
                    writer.push(Opcode::Value);
                    writer.push(registers[arg]);
                    writer.push(arg);
                }
            }
            )?
            let ret = code
                .read_at::<Reg>(args_start $(+ $num_args * std::mem::size_of::<Reg>())?)
                .unwrap();
            registers.set_named(ret, false);
            writer.push(Opcode::$op);
            addr_placeholder!(addr);
            $(
            for _ in 0..$num_args {
                let arg = code.take::<Reg>().unwrap();
                writer.push(arg);
            }
            )?
            code.skip::<Reg>();
            writer.push::<Reg>(num_regs);
            writer.push::<Reg>(ret);
        }};
    }

    macro_rules! dcall_op {
        ($dop:ident, $op:ident $(, $num_args:literal)?) => {{
            let addr = code.take::<Reg>().unwrap();
            let args_start = code.offset();
            $(
            for i in 0..$num_args {
                let arg = code
                    .read_at::<Reg>(args_start + i * std::mem::size_of::<Reg>())
                    .unwrap();
                if registers.named(arg) {
                    writer.push(Opcode::Value);
                    writer.push(registers[arg]);
                    writer.push(arg);
                }
            }
            )?
            let ret = code
                .read_at::<Reg>(args_start $(+ $num_args * std::mem::size_of::<Reg>())?)
                .unwrap();

            if registers.named(addr) {
                writer.push(Opcode::$op);
                let addr = registers[addr].into_raw();
                addr_placeholder!(addr.try_into().unwrap());
            } else {
                writer.push(Opcode::$dop);
                writer.push(addr);
            }
            registers.set_named(ret, false);
            $(
            for _ in 0..$num_args {
                let arg = code.take::<Reg>().unwrap();
                writer.push(arg);
            }
            )?
            code.skip::<Reg>();
            writer.push(num_regs);
            writer.push(ret);
        }};
    }

    macro_rules! binary_op {
        (rr $op:tt $rr:ident $ir:ident) => {{
            let lhs = code.take::<Reg>().unwrap();
            let rhs = code.take::<Reg>().unwrap();
            let to = code.take::<Reg>().unwrap();
            if registers.named(lhs) && registers.named(rhs) {
                registers.set_named(to, true);
                registers[to] = registers[lhs] $op registers[rhs];
            } else if registers.named(lhs) {
                let lhs = registers[lhs];
                writer.push(Opcode::$ir);
                writer.push(lhs);
                writer.push(rhs);
                writer.push(to);
            } else if registers.named(rhs) {
                let rhs = registers[rhs];
                writer.push(Opcode::$ir);
                writer.push(rhs);
                writer.push(lhs);
                writer.push(to);
            } else {
                writer.push(Opcode::$rr);
                writer.push(lhs);
                writer.push(rhs);
                writer.push(to);
            }
        }};
        (rr $op:tt $rr:ident $ir:ident $ri:ident) => {{
            let lhs = code.take::<Reg>().unwrap();
            let rhs = code.take::<Reg>().unwrap();
            let to = code.take::<Reg>().unwrap();
            if registers.named(lhs) && registers.named(rhs) {
                registers.set_named(to, true);
                registers[to] = registers[lhs] $op registers[rhs];
            } else if registers.named(lhs) {
                let lhs = registers[lhs];
                writer.push(Opcode::$ir);
                writer.push(lhs);
                writer.push(rhs);
                writer.push(to);
            } else if registers.named(rhs) {
                let rhs = registers[rhs];
                writer.push(Opcode::$ri);
                writer.push(lhs);
                writer.push(rhs);
                writer.push(to);
            } else {
                writer.push(Opcode::$rr);
                writer.push(lhs);
                writer.push(rhs);
                writer.push(to);
            }
        }};
        (ir $op:tt $ir:ident) => {{
            let lhs = code.take::<Int>().unwrap();
            let rhs = code.take::<Reg>().unwrap();
            let to = code.take::<Reg>().unwrap();
            if registers.named(rhs) {
                registers.set_named(to, true);
                registers[to] = lhs $op registers[rhs];
            } else {
                writer.push(Opcode::$ir);
                writer.push::<Value>(Value::int(lhs));
                writer.push(rhs);
                writer.push(to);
            }
        }};
        (ri $op:tt $ri:ident) => {{
            let lhs = code.take::<Reg>().unwrap();
            let rhs = code.take::<Int>().unwrap();
            let to = code.take::<Reg>().unwrap();
            if registers.named(lhs) {
                registers.set_named(to, true);
                registers[to] = registers[lhs] $op rhs;
            } else {
                writer.push(Opcode::$ri);
                writer.push(lhs);
                writer.push::<Value>(Value::int(rhs));
                writer.push(to);
            }
        }};
    }

    while !code.is_at_end() {
        let start = code.offset();
        if jumps[start].contains(Jump::Out) || jumps[start].contains(Jump::In) {
            for i in 0..num_regs {
                if registers.named(i) {
                    let mut tmp = [0; 256];
                    if vm::register_is_used(&code, jumps, i, &mut tmp, 0) {
                        writer.push(Opcode::Value);
                        writer.push(registers[i]);
                        writer.push(i);
                    }
                }
            }
        }
        if jumps[start].contains(Jump::In) {
            for i in 0..num_regs {
                registers.set_named(i, false);
            }
        }

        locs.insert(start, writer.len());
        let opcode = code.take::<Bytecode>().unwrap();
        match opcode {
            Bytecode::Exit => {
                writer.push(Opcode::Exit);
            }
            Bytecode::Func => {
                for i in 0..num_regs {
                    registers.set_named(i, false);
                }
                let current_func_end = code.take::<Addr>().unwrap();
                num_regs = code.take::<Reg>().unwrap();
                registers.grow(num_regs);
                writer.push(Opcode::Func);
                addr_placeholder!(current_func_end);
                writer.push(num_regs);
            }
            Bytecode::Copy => {
                let (from, to) = take![Reg, Reg];
                if registers.named(from) {
                    registers.set_named(to, true);
                    registers[to] = registers[from];
                } else {
                    registers.set_named(to, false);
                    write![Opcode::CopyR, from, to];
                }
            }
            Bytecode::Jump => {
                let addr = code.take::<Addr>().unwrap();
                writer.push(Opcode::Jump);
                addr_placeholder!(addr);
            }
            Bytecode::JumpEz => {
                let (cond, addr) = take![Reg, Addr];
                if registers.named(cond) {
                    if registers[cond] == 0 {
                        writer.push(Opcode::Jump);
                        addr_placeholder!(addr);
                    }
                } else {
                    write![Opcode::JumpEz, cond];
                    addr_placeholder!(addr);
                }
            }
            Bytecode::JumpNz => {
                let (cond, addr) = take![Reg, Addr];
                if registers.named(cond) {
                    if registers[cond] != 0 {
                        writer.push(Opcode::Jump);
                        addr_placeholder!(addr);
                    }
                } else {
                    write![Opcode::JumpNz, cond];
                    addr_placeholder!(addr);
                }
            }
            Bytecode::JumpLtRR => binary_jump_op!(rr < JumpLtRR JumpLtIR JumpLtRI),
            Bytecode::JumpLtRI => binary_jump_op!(ri < JumpLtRI),
            Bytecode::JumpLtIR => binary_jump_op!(ir < JumpLtIR),
            Bytecode::JumpLeRR => binary_jump_op!(rr <= JumpLeRR JumpLeIR JumpLeRI),
            Bytecode::JumpLeRI => binary_jump_op!(ri <= JumpLeRI),
            Bytecode::JumpLeIR => binary_jump_op!(ir <= JumpLeIR),
            Bytecode::JumpEqRR => binary_jump_op!(rr == JumpEqRR JumpEqIR),
            Bytecode::JumpEqIR => binary_jump_op!(ir == JumpEqIR),
            Bytecode::JumpNeRR => binary_jump_op!(rr != JumpNeRR JumpNeIR),
            Bytecode::JumpNeIR => binary_jump_op!(ir != JumpNeIR),
            Bytecode::Call0 => call_op!(Call0),
            Bytecode::Call1 => call_op!(Call1, 1),
            Bytecode::Call2 => call_op!(Call2, 2),
            Bytecode::Call3 => call_op!(Call3, 3),
            Bytecode::Call4 => call_op!(Call4, 4),
            Bytecode::Call5 => call_op!(Call5, 5),
            Bytecode::Call6 => call_op!(Call6, 6),
            Bytecode::Call7 => call_op!(Call7, 7),
            Bytecode::Call8 => call_op!(Call8, 8),
            Bytecode::CallV => {
                let addr = code.take::<Addr>().unwrap();
                let num_args = {
                    let n = code.take::<Int>().unwrap();
                    usize::try_from(n).unwrap()
                };
                let args_start = code.offset();
                for i in 0..num_args {
                    let arg = code
                        .read_at::<Reg>(args_start + i * std::mem::size_of::<Reg>())
                        .unwrap();
                    if registers.named(arg) {
                        write![Opcode::Value, registers[arg], arg];
                    }
                }
                let ret = code
                    .read_at::<Reg>(args_start + num_args * std::mem::size_of::<Reg>())
                    .unwrap();
                registers.set_named(ret, false);
                writer.push(Opcode::CallV);
                addr_placeholder!(addr);
                for _ in 0..num_args {
                    let arg = code.take::<Reg>().unwrap();
                    writer.push(arg);
                }
                code.skip::<Reg>();
                write![num_regs, ret];
            }
            Bytecode::Addr => {
                let (addr, to) = take![Addr, Reg];
                writer.push(Opcode::CopyA);
                addr_placeholder!(addr);
                writer.push(to);
                registers.set_named(to, false);
            }
            Bytecode::DJump => {
                let addr = code.take::<Reg>().unwrap();
                if registers.named(addr) {
                    let addr = registers[addr].into_raw();
                    let addr = Addr::try_from(addr).unwrap();
                    writer.push(Opcode::Jump);
                    addr_placeholder!(addr);
                } else {
                    write![Opcode::DJump, addr];
                }
            }
            Bytecode::DCall0 => dcall_op!(DCall0, Call0),
            Bytecode::DCall1 => dcall_op!(DCall1, Call1, 1),
            Bytecode::DCall2 => dcall_op!(DCall2, Call2, 1),
            Bytecode::DCall3 => dcall_op!(DCall3, Call3, 1),
            Bytecode::DCall4 => dcall_op!(DCall4, Call4, 1),
            Bytecode::DCall5 => dcall_op!(DCall5, Call5, 1),
            Bytecode::DCall6 => dcall_op!(DCall6, Call6, 1),
            Bytecode::DCall7 => dcall_op!(DCall7, Call7, 1),
            Bytecode::DCall8 => dcall_op!(DCall8, Call8, 1),
            Bytecode::DCallV => {
                let addr = code.take::<Reg>().unwrap();
                let num_args = {
                    let n = code.take::<Int>().unwrap();
                    usize::try_from(n).unwrap()
                };
                let args_start = code.offset();
                for i in 0..num_args {
                    let arg = code
                        .read_at::<Reg>(args_start + i * std::mem::size_of::<Reg>())
                        .unwrap();
                    if registers.named(arg) {
                        write![Opcode::Value, registers[arg], arg];
                    }
                }
                let ret = code
                    .read_at::<Reg>(args_start + num_args * std::mem::size_of::<Reg>())
                    .unwrap();
                if registers.named(addr) {
                    writer.push(Opcode::CallV);
                    let addr = registers[addr].into_raw();
                    addr_placeholder!(addr.try_into().unwrap());
                } else {
                    write![Opcode::DCallV, addr];
                }
                registers.set_named(ret, false);
                for _ in 0..num_args {
                    let arg = code.take::<Reg>().unwrap();
                    writer.push(arg);
                }
                code.skip::<Reg>();
                write![num_regs, ret];
            }
            Bytecode::RetR => {
                let reg = code.take::<Reg>().unwrap();
                if registers.named(reg) {
                    write![Opcode::RetI, registers[reg]];
                } else {
                    write![Opcode::RetR, reg];
                }
            }
            Bytecode::RetI => {
                let int = code.take::<Int>().unwrap();
                write![Opcode::RetI, Value::int(int)];
            }
            Bytecode::Int => {
                let (int, to) = take![Int, Reg];
                registers.set_named(to, true);
                registers[to] = Value::int(int);
            }
            Bytecode::Str => {
                let num_chars = code.take::<Int>().unwrap();
                let len = num_chars.try_into().unwrap();
                let ptr = gc.alloc(len);
                for i in 0..usize::try_from(len).unwrap() {
                    let ch = code.take::<Int>().unwrap();
                    gc.array_mut(ptr)[i] = Value::int(ch);
                }
                let to = code.take::<Reg>().unwrap();
                write![Opcode::Value, Value::ptr_unchecked(ptr), to];
                registers.set_named(to, false);
            }
            Bytecode::AddRR => binary_op!(rr + AddRR AddIR),
            Bytecode::AddIR => binary_op!(ir + AddIR),
            Bytecode::SubRR => binary_op!(rr - SubRR SubIR SubRI),
            Bytecode::SubRI => binary_op!(ri - SubRI),
            Bytecode::SubIR => binary_op!(ir - SubIR),
            Bytecode::MulRR => binary_op!(rr * MulRR MulIR),
            Bytecode::MulIR => binary_op!(ir * MulIR),
            Bytecode::DivRR => binary_op!(rr / DivRR DivIR DivRI),
            Bytecode::DivRI => binary_op!(ri / DivRI),
            Bytecode::DivIR => binary_op!(ir / DivIR),
            Bytecode::ModRR => binary_op!(rr % ModRR ModIR ModRI),
            Bytecode::ModRI => binary_op!(ri % ModRI),
            Bytecode::ModIR => binary_op!(ir % ModIR),
            Bytecode::Neg => {
                let (rhs, to) = take![Reg, Reg];
                if registers.named(rhs) {
                    registers[to] = -registers[rhs];
                    registers.set_named(to, true);
                } else {
                    write![Opcode::Neg, rhs, to];
                }
            }
            Bytecode::Incr => {
                let reg = take![Reg];
                if registers.named(reg) {
                    registers[reg] += 1;
                } else {
                    write![Opcode::Incr, reg];
                }
            }
            Bytecode::Decr => {
                let reg = take![Reg];
                if registers.named(reg) {
                    registers[reg] -= 1;
                } else {
                    write![Opcode::Decr, reg];
                }
            }
            Bytecode::ArrR => {
                let (len, to) = take![Reg, Reg];
                if registers.named(len) {
                    write![Opcode::ArrI, registers[len].as_int_unchecked(), to];
                } else {
                    write![Opcode::ArrR, len, to];
                }
                registers.set_named(to, false);
            }
            Bytecode::ArrI => {
                let (len, to) = take![Int, Reg];
                write![Opcode::ArrI, len, to];
                registers.set_named(to, false);
            }
            Bytecode::GetR => {
                let [arr, idx, to] = code.take::<[Reg; 3]>().unwrap();
                if registers.named(idx) {
                    let idx = registers[idx];
                    write![Opcode::GetI, arr, idx.as_int_unchecked(), to];
                } else {
                    write![Opcode::GetR, arr, idx, to];
                }
            }
            Bytecode::GetI => {
                let (arr, idx, to) = take![Reg, Int, Reg];
                write![Opcode::GetI, arr, idx, to];
            }
            Bytecode::SetII => {
                let (arr, idx, val) = take![Reg, Int, Int];
                write![Opcode::SetII, arr, idx, val];
            }
            Bytecode::SetRR => {
                let (arr, idx, val) = take![Reg, Reg, Reg];
                if registers.named(idx) && registers.named(val) {
                    write![
                        Opcode::SetII,
                        arr,
                        registers[idx].as_int_unchecked(),
                        registers[val].as_int_unchecked(),
                    ];
                } else if registers.named(idx) {
                    write![Opcode::SetIR, arr, registers[idx].as_int_unchecked(), val,];
                } else if registers.named(val) {
                    write![Opcode::SetRI, arr, idx, registers[val].as_int_unchecked(),];
                } else {
                    write![Opcode::SetRR, arr, idx, val,];
                }
            }
            Bytecode::SetRI => {
                let (arr, idx, val) = take![Reg, Reg, Int];
                if registers.named(idx) {
                    write![Opcode::SetII, arr, registers[idx].as_int_unchecked(), val,];
                } else {
                    write![Opcode::SetRI, arr, idx, val,];
                }
            }
            Bytecode::SetIR => {
                let (arr, idx, val) = take![Reg, Int, Reg];
                if registers.named(val) {
                    write![Opcode::SetII, arr, idx, registers[val].as_int_unchecked(),];
                } else {
                    write![Opcode::SetIR, arr, idx, val,];
                }
            }
            Bytecode::Len => {
                let (arr, to) = take![Reg, Reg];
                write![Opcode::Len, arr, to];
            }
            Bytecode::Type => {
                let (obj, to) = take![Reg, Reg];
                if registers.named(obj) {
                    let kind = registers[obj].kind() as u8;
                    registers[to] = Value::int_unchecked(kind.into());
                    registers.set_named(to, true);
                } else {
                    write![Opcode::Type, obj, to];
                }
            }
            Bytecode::PutcR => {
                let reg = take![Reg];
                if registers.named(reg) {
                    let ch = registers[reg];
                    let ch = u32::try_from(ch.as_int_unchecked()).unwrap();
                    write![Opcode::PutcI, ch];
                } else {
                    write![Opcode::PutcR, reg];
                }
            }
            Bytecode::PutcI => {
                let int = take![Int];
                let ch = u32::try_from(int).unwrap();
                write![Opcode::PutcI, ch];
            }
            Bytecode::__MAX => unimplemented!(),
        }

        if jumps[start].contains(Jump::Out) {
            for i in 0..num_regs {
                registers.set_named(i, false);
            }
        }
    }

    locs.insert(code.offset(), writer.len());

    let mut editor = writer.as_mut();
    for (i, addr) in froms {
        let new_addr = locs[&usize::try_from(addr).unwrap()];
        // println!("inserting address {new_addr}")
        editor.set_offset(i);
        // dbg!(editor.read::<Addr>().unwrap());
        editor.write::<Addr>(new_addr.try_into().unwrap());
    }

    writer.into_inner()
}

impl FromBytes for Opcode {
    fn from_bytes<R: Read>(reader: &R) -> Option<Self> {
        reader.read_byte().and_then(Self::from_raw)
    }

    unsafe fn from_bytes_unchecked<R: Read>(reader: &R) -> Self {
        Self::from_raw_unchecked(reader.read_byte_unchecked())
    }
}

impl ToBytes for Opcode {
    fn to_bytes<W: Write>(self, writer: &mut W) -> Option<usize> {
        writer.write(self as u8)
    }
}

// TODO: Use a tagged u8 to represent opcodes. The layout would be:
// bbbbbbbb
//       ^^ argument type flags. 0 is register, 1 is value
// ^^^^^^   discriminant

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    // <op>
    Exit = 0,
    // <op> <end:Addr> <nregs:Reg>
    Func,
    // <op> <addr:Addr> <to:Reg>
    CopyA,
    // <op> <from:Reg> <to:Reg>
    CopyR,
    // <op> <addr:Addr>
    Jump,
    // <op> <cond:Reg> <addr:Addr>
    JumpEz,
    // <op> <cond:Reg> <addr:Addr>
    JumpNz,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpLtRR,
    // <op> <lhs:Reg> <rhs:Value> <addr:Addr>
    JumpLtRI,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpLtIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpLeRR,
    // <op> <lhs:Reg> <rhs:Value> <addr:Addr>
    JumpLeRI,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpLeIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpEqRR,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpEqIR,
    // <op> <lhs:Reg> <rhs:Reg> <addr:Addr>
    JumpNeRR,
    // <op> <lhs:Value> <rhs:Reg> <addr:Addr>
    JumpNeIR,
    // <op> <addr:Addr> <nregs:Reg> <to:Reg>
    Call0,
    // <op> <addr:Addr> <args:[Reg; 1]> <nregs:Reg> <to:Reg>
    Call1,
    // <op> <addr:Addr> <args:[Reg; 2]> <nregs:Reg> <to:Reg>
    Call2,
    // <op> <addr:Addr> <args:[Reg; 3]> <nregs:Reg> <to:Reg>
    Call3,
    // <op> <addr:Addr> <args:[Reg; 4]> <nregs:Reg> <to:Reg>
    Call4,
    // <op> <addr:Addr> <args:[Reg; 5]> <nregs:Reg> <to:Reg>
    Call5,
    // <op> <addr:Addr> <args:[Reg; 6]> <nregs:Reg> <to:Reg>
    Call6,
    // <op> <addr:Addr> <args:[Reg; 7]> <nregs:Reg> <to:Reg>
    Call7,
    // <op> <addr:Addr> <args:[Reg; 8]> <nregs:Reg> <to:Reg>
    Call8,
    // <op> <addr:Addr> <nargs:Int> <args:[Reg; nargs]> <nregs:Reg> <to:Reg>
    CallV,
    // <op> <addr:Reg>
    DJump,
    // <op> <addr:Reg> <nregs:Reg> <to:Reg>
    DCall0,
    // <op> <addr:Reg> <args:[Reg; 1]> <nregs:Reg> <to:Reg>
    DCall1,
    // <op> <addr:Reg> <args:[Reg; 2]> <nregs:Reg> <to:Reg>
    DCall2,
    // <op> <addr:Reg> <args:[Reg; 3]> <nregs:Reg> <to:Reg>
    DCall3,
    // <op> <addr:Reg> <args:[Reg; 4]> <nregs:Reg> <to:Reg>
    DCall4,
    // <op> <addr:Reg> <args:[Reg; 5]> <nregs:Reg> <to:Reg>
    DCall5,
    // <op> <addr:Reg> <args:[Reg; 6]> <nregs:Reg> <to:Reg>
    DCall6,
    // <op> <addr:Reg> <args:[Reg; 7]> <nregs:Reg> <to:Reg>
    DCall7,
    // <op> <addr:Reg> <args:[Reg; 8]> <nregs:Reg> <to:Reg>
    DCall8,
    // <op> <addr:Reg> <nargs:Int> <args:[Reg; nargs]> <nregs:Reg> <to:Reg>
    DCallV,
    // <op> <reg:Reg>
    RetR,
    // <op> <val:Value>
    RetI,
    // <op> <val:Value> <to:Reg>
    Value,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    AddRR,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    AddIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    SubRR,
    // <op> <lhs:Reg> <rhs:Value> <to:Reg>
    SubRI,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    SubIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    MulRR,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    MulIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    DivRR,
    // <op> <lhs:Reg> <rhs:Value> <to:Reg>
    DivRI,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    DivIR,
    // <op> <lhs:Reg> <rhs:Reg> <to:Reg>
    ModRR,
    // <op> <lhs:Reg> <rhs:Value> <to:Reg>
    ModRI,
    // <op> <lhs:Value> <rhs:Reg> <to:Reg>
    ModIR,
    // <op> <reg:Reg> <to:Reg>
    Neg,
    // <op> <reg:Reg>
    Incr,
    // <op> <reg:Reg>
    Decr,
    // <op> <len:Reg> <to:Reg>
    ArrR,
    // <op> <len:Int> <to:Reg>
    ArrI,
    // <op> <arr:Reg> <idx:Reg> <to:Reg>
    GetR,
    // <op> <arr:Reg> <idx:Int> <to:Reg>
    GetI,
    // <op> <arr:Reg> <idx:Int> <val:Value>
    SetII,
    // <op> <arr:Reg> <idx:Reg> <val:Reg>
    SetRR,
    // <op> <arr:Reg> <idx:Reg> <val:Value>
    SetRI,
    // <op> <arr:Reg> <idx:Int> <val:Reg>
    SetIR,
    // <op> <arr:Reg> <to:Reg>
    Len,
    // <op> <obj:Reg> <to:Reg>
    Type,
    // <op> <ch:Reg>
    PutcR,
    // <op> <ch:u32>
    PutcI,
    __MAX,
}

impl Opcode {
    pub const MAX: u8 = Self::__MAX as u8;

    pub fn from_raw(raw: u8) -> Option<Self> {
        if raw > Self::MAX {
            return None;
        }
        Some(unsafe { Self::from_raw_unchecked(raw) })
    }

    pub unsafe fn from_raw_unchecked(raw: u8) -> Self {
        std::mem::transmute(raw)
    }

    pub fn as_str(&self) -> &'static str {
        use Opcode::*;

        match self {
            Exit => "exit",
            Func => "func",
            CopyA => "copy_a",
            CopyR => "copy_r",
            Jump => "jump",
            JumpEz => "jumpez",
            JumpNz => "jumpnz",
            JumpLtRR => "jumplt_rr",
            JumpLtRI => "jumplt_ri",
            JumpLtIR => "jumplt_ir",
            JumpLeRR => "jumple_rr",
            JumpLeRI => "jumple_ri",
            JumpLeIR => "jumple_ir",
            JumpEqRR => "jumpeq_rr",
            JumpEqIR => "jumpeq_ir",
            JumpNeRR => "jumpne_rr",
            JumpNeIR => "jumpne_ir",
            Call0 => "call0",
            Call1 => "call1",
            Call2 => "call2",
            Call3 => "call3",
            Call4 => "call4",
            Call5 => "call5",
            Call6 => "call6",
            Call7 => "call7",
            Call8 => "call8",
            CallV => "callv",
            DJump => "djump",
            DCall0 => "dcall0",
            DCall1 => "dcall1",
            DCall2 => "dcall2",
            DCall3 => "dcall3",
            DCall4 => "dcall4",
            DCall5 => "dcall5",
            DCall6 => "dcall6",
            DCall7 => "dcall7",
            DCall8 => "dcall8",
            DCallV => "dcallv",
            RetR => "ret_r",
            RetI => "ret_i",
            Value => "value",
            AddRR => "add_rr",
            AddIR => "add_ir",
            SubRR => "sub_rr",
            SubRI => "sub_ri",
            SubIR => "sub_ir",
            MulRR => "mul_rr",
            MulIR => "mul_ir",
            DivRR => "div_rr",
            DivRI => "div_ri",
            DivIR => "div_ir",
            ModRR => "mod_rr",
            ModRI => "mod_ri",
            ModIR => "mod_ir",
            Neg => "neg",
            Incr => "incr",
            Decr => "decr",
            ArrR => "arr_r",
            ArrI => "arr_i",
            GetR => "get_r",
            GetI => "get_i",
            SetII => "set_ii",
            SetRR => "set_rr",
            SetRI => "set_ri",
            SetIR => "set_ir",
            Len => "len",
            Type => "type",
            PutcR => "putc_r",
            PutcI => "putc_i",
            __MAX => unimplemented!(),
        }
    }
}
