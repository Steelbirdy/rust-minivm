use crate::{
    common::{Addr, ByteReader, Int, Read, Reg},
    compile::Bytecode,
};

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Jump {
    In = 1 << 0,
    Out = 1 << 1,
    Init = 1 << 2,
    Reach = 1 << 3,
    Instr = 1 << 4,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct JumpSet(u8);

impl JumpSet {
    pub const EMPTY: JumpSet = JumpSet(u8::MIN);
    pub const ALL: JumpSet = JumpSet(0b11111);

    pub const fn contains(self, jump: Jump) -> bool {
        self.0 & (jump as u8) != 0
    }

    pub fn insert(&mut self, jump: Jump) {
        self.0 |= jump as u8;
    }
}

pub fn trace_jumps(code: ByteReader) -> Box<[JumpSet]> {
    let mut ret = Vec::with_capacity(code.len() + 1);
    ret.resize(ret.capacity(), JumpSet::EMPTY);
    let mut ret = ret.into_boxed_slice();

    code.at_start(|| {
        trace_jumps_base(&code, &mut ret);
        code.reset();
        trace_jumps_reachable(&code, &mut ret);
    });
    ret
}

#[cfg(debug_assertions)]
fn addr_to_usize(addr: Addr) -> usize {
    usize::try_from(addr).unwrap()
}

#[cfg(not(debug_assertions))]
fn addr_to_usize(addr: Addr) -> usize {
    addr as usize
}

// TODO: Merge `trace_jumps_base` and `trace_jumps_reachable` into one function
fn trace_jumps_base(code: &ByteReader, jumps: &mut [JumpSet]) {
    macro_rules! skip {
        [$($ty:ty),*] => {{
            $(code.skip::<$ty>();)*
        }};
    }

    while !code.is_at_end() {
        let op_flags = &mut jumps[code.offset()];
        op_flags.insert(Jump::Instr);

        macro_rules! jump_op {
            ($($ty:ty),*) => {{
                op_flags.insert(Jump::Out);
                skip![$($ty),*];
                let addr = code.take::<Addr>().unwrap();
                jumps[addr_to_usize(addr)].insert(Jump::In);
            }};
        }

        let opcode = code.take::<Bytecode>().unwrap();
        match opcode {
            Bytecode::Missing => {}
            Bytecode::Exit => {
                op_flags.insert(Jump::Out);
            }
            Bytecode::Func => {
                code.skip::<Addr>();
                code.skip::<Reg>();
                jumps[code.offset()].insert(Jump::Init);
            }
            Bytecode::Reg => skip![Reg, Reg],
            Bytecode::Jump => jump_op![],
            Bytecode::JumpEz | Bytecode::JumpNz => jump_op![Reg],
            Bytecode::JumpLtRR | Bytecode::JumpLeRR | Bytecode::JumpEqRR | Bytecode::JumpNeRR => {
                jump_op![Reg, Reg];
            }
            Bytecode::JumpLtRI
            | Bytecode::JumpLeRI
            | Bytecode::JumpLtIR
            | Bytecode::JumpLeIR
            | Bytecode::JumpEqIR
            | Bytecode::JumpNeIR => jump_op![Int, Reg],
            Bytecode::Call => {
                code.skip::<Addr>();
                let num_args = code.take::<Int>().unwrap();
                code.skipn::<Reg>(num_args.try_into().unwrap());
                code.skip::<Reg>();
            }
            Bytecode::Addr => skip![Addr, Reg],
            Bytecode::DJump => {
                op_flags.insert(Jump::Out);
                code.skip::<Reg>();
            }
            Bytecode::DCall => {
                code.skip::<Reg>();
                let num_args = code.take::<Int>().unwrap();
                code.skipn::<Reg>(num_args.try_into().unwrap());
                code.skip::<Reg>();
            }
            Bytecode::RetR => {
                op_flags.insert(Jump::Out);
                code.skip::<Reg>();
            }
            Bytecode::RetI => {
                op_flags.insert(Jump::Out);
                code.skip::<Int>();
            }
            Bytecode::Int => skip![Int, Reg],
            Bytecode::Str => {
                let num_chars = code.take::<Int>().unwrap();
                code.skipn::<Int>(num_chars.try_into().unwrap());
                code.skip::<Reg>();
            }
            Bytecode::AddRR
            | Bytecode::SubRR
            | Bytecode::MulRR
            | Bytecode::DivRR
            | Bytecode::ModRR => {
                skip![Reg, Reg, Reg];
            }
            Bytecode::AddIR
            | Bytecode::SubRI
            | Bytecode::SubIR
            | Bytecode::MulIR
            | Bytecode::DivRI
            | Bytecode::DivIR
            | Bytecode::ModRI
            | Bytecode::ModIR => skip![Reg, Int, Reg],
            Bytecode::Neg => skip![Reg, Reg],
            Bytecode::Incr => skip![Reg],
            Bytecode::Decr => skip![Reg],
            Bytecode::ArrR => skip![Reg, Reg],
            Bytecode::ArrI => skip![Int, Reg],
            Bytecode::GetR => skip![Reg, Reg, Reg],
            Bytecode::GetI => skip![Reg, Int, Reg],
            Bytecode::SetII => skip![Reg, Int, Int],
            Bytecode::SetRR => skip![Reg, Reg, Reg],
            Bytecode::SetRI | Bytecode::SetIR => skip![Reg, Int, Reg],
            Bytecode::Len => skip![Reg, Reg],
            Bytecode::Type => skip![Reg, Reg],
            Bytecode::PutcR => skip![Reg],
            Bytecode::PutcI => skip![Int],
            Bytecode::__MAX => unimplemented!(),
        }
    }
}

fn trace_jumps_reachable(code: &ByteReader, jumps: &mut [JumpSet]) {
    macro_rules! skip {
        [$($ty:ty),*] => {{
            $(code.skip::<$ty>();)*
        }};
    }

    macro_rules! jump_op {
        ($($ty:ty),*) => {{
            skip![$($ty),*];
            let addr = code.take::<Addr>().unwrap();
            let offset = addr_to_usize(addr);
            code.at_offset(offset, || {
                trace_jumps_reachable(code, jumps);
            });
        }};
    }

    if jumps[code.offset()].contains(Jump::Reach) {
        return;
    }
    while !code.is_at_end() {
        let op_jumps = &mut jumps[code.offset()];
        op_jumps.insert(Jump::Reach);
        let opcode = code.take::<Bytecode>().unwrap();
        match opcode {
            // Can't make any assumptions about missing code
            Bytecode::Missing => return,
            Bytecode::Exit => return,
            Bytecode::Func => skip![Addr, Reg],
            Bytecode::Reg => skip![Reg, Reg],
            Bytecode::Jump => jump_op![],
            Bytecode::JumpEz | Bytecode::JumpNz => jump_op![Reg],
            Bytecode::JumpLtRR | Bytecode::JumpLeRR | Bytecode::JumpEqRR | Bytecode::JumpNeRR => {
                jump_op![Reg, Reg];
            }
            Bytecode::JumpLtRI
            | Bytecode::JumpLeRI
            | Bytecode::JumpLtIR
            | Bytecode::JumpLeIR
            | Bytecode::JumpEqIR
            | Bytecode::JumpNeIR => jump_op![Int, Reg],
            Bytecode::Call => {
                let addr = code.take::<Addr>().unwrap();
                let offset = addr_to_usize(addr);
                let num_args = code.take::<Int>().unwrap();
                code.skipn::<Reg>(num_args.try_into().unwrap());
                code.at_offset(offset, || {
                    trace_jumps_reachable(code, jumps);
                });
            }
            Bytecode::Addr => {
                let addr = code.take::<Addr>().unwrap();
                let offset = addr_to_usize(addr);
                code.skip::<Reg>();
                code.at_offset(offset, || {
                    trace_jumps_reachable(code, jumps);
                });
            }
            Bytecode::DJump => skip![Reg],
            Bytecode::DCall => {
                code.skip::<Reg>();
                let num_args = code.take::<Int>().unwrap();
                code.skipn::<Reg>(num_args.try_into().unwrap());
                code.skip::<Reg>();
            }
            Bytecode::RetR => skip![Reg],
            Bytecode::RetI => skip![Int],
            Bytecode::Int => skip![Int, Reg],
            Bytecode::Str => {
                let num_chars = code.take::<Int>().unwrap();
                code.skipn::<Int>(num_chars.try_into().unwrap());
                code.skip::<Reg>();
            }
            Bytecode::AddRR
            | Bytecode::SubRR
            | Bytecode::MulRR
            | Bytecode::DivRR
            | Bytecode::ModRR => {
                skip![Reg, Reg, Reg];
            }
            Bytecode::AddIR
            | Bytecode::SubRI
            | Bytecode::SubIR
            | Bytecode::MulIR
            | Bytecode::DivRI
            | Bytecode::DivIR
            | Bytecode::ModRI
            | Bytecode::ModIR => skip![Reg, Int, Reg],
            Bytecode::Neg => skip![Reg, Reg],
            Bytecode::Incr => skip![Reg],
            Bytecode::Decr => skip![Reg],
            Bytecode::ArrR => skip![Reg, Reg],
            Bytecode::ArrI => skip![Int, Reg],
            Bytecode::GetR => skip![Reg, Reg, Reg],
            Bytecode::GetI => skip![Reg, Int, Reg],
            Bytecode::SetII => skip![Reg, Int, Int],
            Bytecode::SetRR => skip![Reg, Reg, Reg],
            Bytecode::SetRI | Bytecode::SetIR => skip![Reg, Int, Reg],
            Bytecode::Len => skip![Reg, Reg],
            Bytecode::Type => skip![Reg, Reg],
            Bytecode::PutcR => skip![Reg],
            Bytecode::PutcI => skip![Int],
            Bytecode::__MAX => unimplemented!(),
        }
    }
}

pub fn register_is_used(
    code: &ByteReader,
    jumps: &[JumpSet],
    reg: Reg,
    buf: &mut [usize],
    head: usize,
) -> bool {
    code.at_checkpoint(|| register_is_used_impl(code, jumps, reg, buf, head))
}

fn register_is_used_impl(
    code: &ByteReader,
    jumps: &[JumpSet],
    reg: Reg,
    buf: &mut [usize],
    head: usize,
) -> bool {
    macro_rules! skip {
        [$($ty:ty),*] => {{
            $(code.skip::<$ty>();)*
        }};
    }

    macro_rules! check_reg {
        ($($ret:literal),+) => {{
            $(
            let my_reg = code.take::<Reg>().unwrap();
            if my_reg == reg {
                return $ret;
            }
            )+
        }};
    }

    macro_rules! return_used_at_addr {
        ($($skip:ty),*) => {{
            skip![$($skip),*];
            let addr = code.take::<Addr>().unwrap();
            code.set_offset(addr_to_usize(addr));
            return register_is_used(code, jumps, reg, buf, head + 1);
        }};
    }

    let offset = code.offset();
    if head == buf.len() {
        return true;
    }
    if buf[0..head].iter().any(|&x| x == offset) {
        return false;
    }
    buf[head] = offset;

    while !code.is_at_end() {
        let opcode = code.take::<Bytecode>().unwrap();
        let start = code.offset();
        match opcode {
            // Can't make any assumptions about missing code
            Bytecode::Missing => return false,
            Bytecode::Exit => return false,
            Bytecode::Func | Bytecode::Jump => return_used_at_addr!(),
            Bytecode::Reg | Bytecode::Len | Bytecode::Type => check_reg!(true, false),
            Bytecode::JumpEz | Bytecode::JumpNz => {
                check_reg!(true);
                return_used_at_addr!();
            }
            Bytecode::JumpLtRR | Bytecode::JumpLeRR | Bytecode::JumpEqRR | Bytecode::JumpNeRR => {
                check_reg!(true, true);
                return_used_at_addr!();
            }
            Bytecode::JumpLtRI | Bytecode::JumpLeRI => {
                check_reg!(true);
                return_used_at_addr![Int];
            }
            Bytecode::JumpLtIR | Bytecode::JumpLeIR | Bytecode::JumpEqIR | Bytecode::JumpNeIR => {
                skip![Int];
                check_reg!(true);
                return_used_at_addr!();
            }
            Bytecode::Call => {
                skip![Addr];
                let num_args = code.take::<Int>().unwrap();
                for _ in 0..num_args {
                    check_reg!(true);
                }
                check_reg!(false);
            }
            Bytecode::Addr => {}
            Bytecode::DJump => check_reg!(true),
            Bytecode::DCall => {
                check_reg!(true);
                let num_args = code.take::<Int>().unwrap();
                for _ in 0..num_args {
                    check_reg!(true);
                }
                check_reg!(false);
            }
            Bytecode::RetR => return code.take::<Reg>().unwrap() == reg,
            Bytecode::RetI => return false,
            Bytecode::Int => {
                skip![Int];
                check_reg!(false);
            }
            Bytecode::Str => {}
            Bytecode::AddRR
            | Bytecode::SubRR
            | Bytecode::MulRR
            | Bytecode::DivRR
            | Bytecode::ModRR => {
                check_reg!(true, true, false);
            }
            Bytecode::AddIR
            | Bytecode::SubIR
            | Bytecode::MulIR
            | Bytecode::DivIR
            | Bytecode::ModIR => {
                skip![Int];
                check_reg!(true, false);
            }
            Bytecode::SubRI | Bytecode::DivRI | Bytecode::ModRI => {
                check_reg!(true);
                skip![Int];
                check_reg!(false);
            }
            Bytecode::Neg => check_reg!(true, false),
            Bytecode::Incr => check_reg!(true),
            Bytecode::Decr => check_reg!(true),
            Bytecode::ArrR => check_reg!(true, false),
            Bytecode::ArrI => {
                skip![Int];
                check_reg!(false);
            }
            Bytecode::GetR => check_reg!(true, true, false),
            Bytecode::GetI => {
                check_reg!(true);
                skip![Int];
                check_reg!(false);
            }
            Bytecode::SetII => check_reg!(true),
            Bytecode::SetRR => check_reg!(true, true, true),
            Bytecode::SetRI => check_reg!(true, true),
            Bytecode::SetIR => {
                check_reg!(true);
                skip![Int];
                check_reg!(true);
            }
            Bytecode::PutcR => check_reg!(true),
            Bytecode::PutcI => skip![Int],
            Bytecode::__MAX => unimplemented!(),
        }

        code.set_offset(start);
        while !code.is_at_end() && !jumps[code.offset()].contains(Jump::Instr) {
            code.advance(1);
        }
    }

    false
}
