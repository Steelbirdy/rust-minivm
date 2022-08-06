#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]

use crate::{
    common::{
        config::{self, RunConfig},
        Addr, AsBytes, BytecodeReader, Int, Reg,
    },
    vm::{disassemble_instruction, Gc, Opcode, Value},
};
use std::io::{BufWriter, StdoutLock, Write};
use std::mem;

pub struct Vm<'a> {
    code: BytecodeReader<'a>,
    gc: Gc,
    call_stack: [usize; config::NUM_FRAMES],
    stack_ptr: usize,
    registers: [Value; config::NUM_REGISTERS],
    frame_start: usize,
    stdout: BufWriter<StdoutLock<'static>>,
    char_buf: [u8; 4],
    buf: String,
    config: &'a RunConfig,
}

impl<'a> Vm<'a> {
    pub fn new(code: BytecodeReader<'a>, gc: Gc, config: &'a RunConfig) -> Self {
        Self {
            code,
            gc,
            call_stack: [0; config::NUM_FRAMES],
            stack_ptr: 0,
            registers: [Value::int_unchecked(0); config::NUM_REGISTERS],
            frame_start: 0,
            stdout: BufWriter::new(std::io::stdout().lock()),
            char_buf: [0; 4],
            buf: String::new(),
            config,
        }
    }

    #[inline]
    fn read_at<T: AsBytes>(&self, index: usize) -> T {
        self.code.read_at(index)
    }

    #[inline]
    fn read<T: AsBytes>(&self) -> T {
        self.code.read()
    }

    #[inline]
    fn take<T: AsBytes>(&mut self) -> T {
        self.code.take()
    }

    pub fn run(&mut self) -> Value {
        while !self.code.is_at_end() {
            #[cfg(feature = "trace-execution")]
            self.debug_next();

            let opcode = self.take::<Opcode>();
            match opcode {
                Opcode::Exit => break,
                Opcode::Func => func(self),
                Opcode::RegA => reg_a(self),
                Opcode::RegR => reg_r(self),
                Opcode::BranchNz => branchnz(self),
                Opcode::BranchEqRR => brancheq_rr(self),
                Opcode::BranchEqIR => brancheq_ir(self),
                Opcode::BranchLtRR => branchlt_rr(self),
                Opcode::BranchLtRI => branchlt_ri(self),
                Opcode::BranchLtIR => branchlt_ir(self),
                Opcode::Jump => jump(self),
                Opcode::JumpEz => jumpez(self),
                Opcode::JumpNz => jumpnz(self),
                Opcode::JumpLtRR => jumplt_rr(self),
                Opcode::JumpLtRI => jumplt_ri(self),
                Opcode::JumpLtIR => jumplt_ir(self),
                Opcode::JumpLeRR => jumple_rr(self),
                Opcode::JumpLeRI => jumple_ri(self),
                Opcode::JumpLeIR => jumple_ir(self),
                Opcode::JumpEqRR => jumpeq_rr(self),
                Opcode::JumpEqIR => jumpeq_ir(self),
                Opcode::JumpNeRR => jumpne_rr(self),
                Opcode::JumpNeIR => jumpne_ir(self),
                Opcode::Call0 => call0(self),
                Opcode::Call1 => call1(self),
                Opcode::Call2 => call2(self),
                Opcode::Call3 => call3(self),
                Opcode::Call4 => call4(self),
                Opcode::Call5 => call5(self),
                Opcode::Call6 => call6(self),
                Opcode::Call7 => call7(self),
                Opcode::Call8 => call8(self),
                Opcode::CallV => callv(self),
                Opcode::DJump => djump(self),
                Opcode::DCall0 => dcall0(self),
                Opcode::DCall1 => dcall1(self),
                Opcode::DCall2 => dcall2(self),
                Opcode::DCall3 => dcall3(self),
                Opcode::DCall4 => dcall4(self),
                Opcode::DCall5 => dcall5(self),
                Opcode::DCall6 => dcall6(self),
                Opcode::DCall7 => dcall7(self),
                Opcode::DCall8 => dcall8(self),
                Opcode::DCallV => dcallv(self),
                Opcode::RetR => ret_r(self),
                Opcode::RetI => ret_i(self),
                Opcode::Value => value(self),
                Opcode::AddRR => add_rr(self),
                Opcode::AddIR => add_ir(self),
                Opcode::SubRR => sub_rr(self),
                Opcode::SubRI => sub_ri(self),
                Opcode::SubIR => sub_ir(self),
                Opcode::MulRR => mul_rr(self),
                Opcode::MulIR => mul_ir(self),
                Opcode::DivRR => div_rr(self),
                Opcode::DivRI => div_ri(self),
                Opcode::DivIR => div_ir(self),
                Opcode::ModRR => mod_rr(self),
                Opcode::ModRI => mod_ri(self),
                Opcode::ModIR => mod_ir(self),
                Opcode::Neg => neg(self),
                Opcode::Incr => incr(self),
                Opcode::Decr => decr(self),
                Opcode::ArrR => arr_r(self),
                Opcode::ArrI => arr_i(self),
                Opcode::GetR => get_r(self),
                Opcode::GetI => get_i(self),
                Opcode::SetII => set_ii(self),
                Opcode::SetRR => set_rr(self),
                Opcode::SetRI => set_ri(self),
                Opcode::SetIR => set_ir(self),
                Opcode::Len => len(self),
                Opcode::Type => r#type(self),
                Opcode::PutcR => putc_r(self),
                Opcode::PutcI => putc_i(self),
                Opcode::__MAX => unreachable!(),
            }
        }
        self.registers[self.frame_start]
    }

    fn debug_next(&mut self) {
        self.code
            .at_checkpoint(|code| disassemble_instruction(code, &mut self.buf));
        print!("{}", self.buf);
        self.buf.clear();
    }

    fn write(&mut self, ch: u32) {
        let ch = char::from_u32(ch).unwrap();
        let slice = ch.encode_utf8(&mut self.char_buf);
        drop(self.stdout.write(slice.as_bytes()));
        if ch == '\n' && !self.config.trace_execution {
            drop(self.stdout.flush());
        }
    }

    fn push_frame(&mut self, num_regs: Reg, addr: usize) {
        self.frame_start += num_regs as usize;
        self.call_stack[self.stack_ptr] = self.code.offset();
        self.stack_ptr += 1;
        self.code.set_offset(addr);
    }

    fn pop_frame(&mut self) {
        self.stack_ptr -= 1;
        self.code.set_offset(self.call_stack[self.stack_ptr]);
        let num_regs = self.read_at::<Reg>(self.code.offset() - mem::size_of::<Reg>());
        self.frame_start -= num_regs as usize;
    }
}

macro_rules! take {
    ($vm:ident, $ty:ty) => {{
        let ret = $vm.take::<$ty>();
        ret
    }};
}

macro_rules! skip {
    ($vm:ident, $ty:ty) => {
        $vm.code.skip::<$ty>()
    };
}

macro_rules! load {
    (mut $vm:ident) => {{
        let idx = take!($vm, Reg);
        &mut $vm.registers[idx as usize + $vm.frame_start]
    }};
    ($vm:ident) => {{
        let idx = take!($vm, Reg);
        $vm.registers[idx as usize + $vm.frame_start]
    }};
}

fn func(vm: &mut Vm) {
    jump(vm);
}

fn reg_a(vm: &mut Vm) {
    let addr = take!(vm, Addr);
    *load!(mut vm) = Value::int_unchecked(addr.into());
}

fn reg_r(vm: &mut Vm) {
    *load!(mut vm) = load!(vm);
}

fn branchnz(vm: &mut Vm) {
    let cond = load!(vm);
    let addrs = take!(vm, [Addr; 2]);
    vm.code.set_offset(addrs[(cond != 0) as usize] as _);
}

fn brancheq_rr(vm: &mut Vm) {
    let (lhs, rhs) = (load!(vm), load!(vm));
    let addrs = take!(vm, [Addr; 2]);
    vm.code.set_offset(addrs[(lhs == rhs) as usize] as _);
}

fn brancheq_ir(vm: &mut Vm) {
    let (lhs, rhs) = (take!(vm, Value), load!(vm));
    let addrs = take!(vm, [Addr; 2]);
    vm.code.set_offset(addrs[(lhs == rhs) as usize] as _);
}

fn branchlt_rr(vm: &mut Vm) {
    let (lhs, rhs) = (load!(vm), load!(vm));
    let addrs = take!(vm, [Addr; 2]);
    vm.code.set_offset(addrs[(lhs < rhs) as usize] as _);
}

fn branchlt_ri(vm: &mut Vm) {
    let (lhs, rhs) = (load!(vm), take!(vm, Value));
    let addrs = take!(vm, [Addr; 2]);
    vm.code.set_offset(addrs[(lhs < rhs) as usize] as _);
}

fn branchlt_ir(vm: &mut Vm) {
    let (lhs, rhs) = (take!(vm, Value), load!(vm));
    let addrs = take!(vm, [Addr; 2]);
    vm.code.set_offset(addrs[(lhs < rhs) as usize] as _);
}

fn jump(vm: &mut Vm) {
    let addr = take!(vm, Addr);
    vm.code.set_offset(addr as _);
}

fn jumpez(vm: &mut Vm) {
    if load!(vm) == 0 {
        let offset = take!(vm, Addr);
        vm.code.set_offset(offset as _);
    } else {
        skip!(vm, Addr);
    }
}

#[allow(clippy::if_not_else)]
fn jumpnz(vm: &mut Vm) {
    if load!(vm) != 0 {
        let offset = take!(vm, Addr);
        vm.code.set_offset(offset as _);
    } else {
        skip!(vm, Addr);
    }
}

macro_rules! binary_jump_op {
    (rr $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            if load!(vm) $op load!(vm) {
                let offset = take!(vm, Addr);
                vm.code.set_offset(offset as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
    (ri $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            if load!(vm) $op take!(vm, Value) {
                let offset = take!(vm, Addr);
                vm.code.set_offset(offset as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
    (ir $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            if take!(vm, Value) $op load!(vm) {
                let offset = take!(vm, Addr);
                vm.code.set_offset(offset as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
}

binary_jump_op!(rr jumplt_rr <);
binary_jump_op!(ri jumplt_ri <);
binary_jump_op!(ir jumplt_ir <);
binary_jump_op!(rr jumple_rr <=);
binary_jump_op!(ri jumple_ri <=);
binary_jump_op!(ir jumple_ir <=);
binary_jump_op!(rr jumpeq_rr ==);
binary_jump_op!(ir jumpeq_ir ==);
binary_jump_op!(rr jumpne_rr !=);
binary_jump_op!(ir jumpne_ir !=);

macro_rules! call_ops {
    ($($name:ident $($i:literal $arg:ident),*;)+) => {
        $(
        fn $name(vm: &mut Vm) {
            let addr = take!(vm, Addr);

            $(
            let $arg = load!(vm);
            )*

            let num_regs = take!(vm, Reg);
            vm.push_frame(num_regs, addr as usize);

            $(
            vm.registers[$i as usize + vm.frame_start] = $arg;
            )*
        }
        )+
    };
}

call_ops![
    call0;
    call1 1 arg1;
    call2 1 arg1, 2 arg2;
    call3 1 arg1, 2 arg2, 3 arg3;
    call4 1 arg1, 2 arg2, 3 arg3, 4 arg4;
    call5 1 arg1, 2 arg2, 3 arg3, 4 arg4, 5 arg5;
    call6 1 arg1, 2 arg2, 3 arg3, 4 arg4, 5 arg5, 6 arg6;
    call7 1 arg1, 2 arg2, 3 arg3, 4 arg4, 5 arg5, 6 arg6, 7 arg7;
    call8 1 arg1, 2 arg2, 3 arg3, 4 arg4, 5 arg5, 6 arg6, 7 arg7, 8 arg8;
];

fn callv(vm: &mut Vm) {
    let addr = take!(vm, Addr);
    let num_args = take!(vm, Int);
    let mut args = Vec::with_capacity(num_args as usize);
    for _ in 0..num_args {
        args.push(load!(vm));
    }
    let num_regs = take!(vm, Reg);
    vm.push_frame(num_regs, addr as usize);

    for i in 0..num_args {
        vm.registers[i as usize + 1 + vm.frame_start] = args[i as usize];
    }
}

fn djump(vm: &mut Vm) {
    let addr = load!(vm).as_int_unchecked();
    debug_assert!(addr >= 0);
    vm.code.set_offset(addr as usize);
}

macro_rules! dcall_ops {
    ($($name:ident $($num_args:literal)?;)+) => {
        $(
        fn $name(vm: &mut Vm) {
            let addr = load!(vm);
            $(let args = [(); $num_args].map(|_| load!(vm));)?
            let num_regs = take!(vm, Reg);
            vm.push_frame(num_regs, addr.as_int_unchecked() as usize);

            $(
            for i in 0..$num_args {
                vm.registers[i as usize + 1 + vm.frame_start] = args[i as usize];
            }
            )?
        }
        )+
    };
}

dcall_ops![
    dcall0;
    dcall1 1;
    dcall2 2;
    dcall3 3;
    dcall4 4;
    dcall5 5;
    dcall6 6;
    dcall7 7;
    dcall8 8;
];

fn dcallv(vm: &mut Vm) {
    let addr = load!(vm);
    let num_args = take!(vm, Int);
    let mut args = Vec::with_capacity(num_args as usize);
    for _ in 0..num_args {
        args.push(load!(vm));
    }
    let num_regs = take!(vm, Reg);
    vm.push_frame(num_regs, addr.as_int_unchecked() as usize);

    for i in 0..num_args {
        vm.registers[i as usize + 1 + vm.frame_start] = args[i as usize];
    }
}

fn ret_r(vm: &mut Vm) {
    let ret = load!(vm);
    vm.pop_frame();
    *load!(mut vm) = ret;
}

fn ret_i(vm: &mut Vm) {
    let ret = take!(vm, Value);
    vm.pop_frame();
    *load!(mut vm) = ret;
}

fn value(vm: &mut Vm) {
    let val = take!(vm, Value);
    *load!(mut vm) = val;
}

macro_rules! binary_op {
    (rr $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            let lhs = load!(vm);
            let rhs = load!(vm);
            *load!(mut vm) = lhs $op rhs;
        }
    };
    (ri $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            let lhs = load!(vm);
            let rhs = take!(vm, Value);
            *load!(mut vm) = lhs $op rhs;
        }
    };
    (ir $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            let lhs = take!(vm, Value);
            let rhs = load!(vm);
            *load!(mut vm) = lhs $op rhs;
        }
    };
}

binary_op!(rr add_rr +);
binary_op!(ir add_ir +);
binary_op!(rr sub_rr -);
binary_op!(ri sub_ri -);
binary_op!(ir sub_ir -);
binary_op!(rr mul_rr *);
binary_op!(ir mul_ir *);
binary_op!(rr div_rr /);
binary_op!(ri div_ri /);
binary_op!(ir div_ir /);
binary_op!(rr mod_rr %);
binary_op!(ri mod_ri %);
binary_op!(ir mod_ir %);

fn neg(vm: &mut Vm) {
    *load!(mut vm) = load!(vm);
}

fn incr(vm: &mut Vm) {
    *load!(mut vm) += 1;
}

fn decr(vm: &mut Vm) {
    *load!(mut vm) -= 1;
}

fn arr_r(vm: &mut Vm) {
    let len = load!(vm);
    assert!(len >= 0);
    *load!(mut vm) = Value::ptr_unchecked(vm.gc.alloc(len.as_int_unchecked() as _));
    vm.gc.run(&mut vm.registers);
}

fn arr_i(vm: &mut Vm) {
    let len = take!(vm, Int);
    assert!(len >= 0);
    *load!(mut vm) = Value::ptr_unchecked(vm.gc.alloc(len as _));
    vm.gc.run(&mut vm.registers);
}

fn get_r(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    // TODO: passing -1 to this causes an overflow
    *load!(mut vm) = vm.gc.get(arr, idx as usize);
}

fn get_i(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
    // TODO: passing -1 to this causes an overflow
    *load!(mut vm) = vm.gc.get(arr, idx as usize);
}

fn set_ii(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
    let val = take!(vm, Value);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn set_rr(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    let val = load!(vm);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn set_ri(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    let val = take!(vm, Value);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn set_ir(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
    let val = take!(vm, Value);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn len(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    *load!(mut vm) = Value::int_unchecked(vm.gc.array_len(arr).into());
}

fn r#type(vm: &mut Vm) {
    let kind = load!(vm).kind() as u8;
    *load!(mut vm) = Value::int_unchecked(kind.into());
}

fn putc_r(vm: &mut Vm) {
    let ch = load!(vm).as_int_unchecked();
    vm.write(ch.try_into().unwrap());
}

fn putc_i(vm: &mut Vm) {
    let ch = take!(vm, u32);
    vm.write(ch);
}
