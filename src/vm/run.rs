#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]

use crate::{
    common::{
        config::{self, RunConfig},
        Addr, Int, Read, Reg,
    },
    vm::{disassemble_instruction, Gc, Opcode, Stack, Value},
};
use std::io::{BufWriter, StdoutLock, Write};

pub struct Vm<'a, R: Read> {
    code: R,
    gc: Gc,
    call_stack: Stack<usize, { config::NUM_FRAMES }>,
    stdout: BufWriter<StdoutLock<'static>>,
    char_buf: [u8; 4],
    buf: String,
    config: &'a RunConfig,
}

impl<'a, R: Read> Vm<'a, R> {
    pub fn new(code: R, gc: Gc, config: &'a RunConfig) -> Self {
        Self {
            code,
            gc,
            call_stack: Stack::new(),
            stdout: BufWriter::new(std::io::stdout().lock()),
            char_buf: [0; 4],
            buf: String::new(),
            config,
        }
    }

    pub fn run(&mut self) -> Value {
        const OUT: Reg = 0;
        while !self.code.is_at_end() {
            if self.config.trace_execution {
                self.debug_next();
            }

            #[cfg(feature = "unsafe")]
            let opcode = unsafe { self.code.take_unchecked::<Opcode>() };

            #[cfg(not(feature = "unsafe"))]
            let opcode = self.code.take::<Opcode>().unwrap();

            match opcode {
                Opcode::Exit => break,
                Opcode::Func => func(self),
                Opcode::CopyA => copy_a(self),
                Opcode::CopyR => copy_r(self),
                Opcode::Jump => jump(self),
                Opcode::JumpEz => jump_ez(self),
                Opcode::JumpNz => jump_nz(self),
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
        self.gc[OUT]
    }

    fn debug_next(&mut self) {
        let offset = self.code.offset();
        disassemble_instruction(&self.code, &mut self.buf);
        self.code.set_offset(offset);
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
        self.gc.push_frame(num_regs);
        self.call_stack.push(self.code.offset());
        self.code.set_offset(addr);
    }

    #[cfg(feature = "unsafe")]
    fn pop_frame(&mut self) {
        let offset = if let Some(offset) = self.call_stack.pop() {
            self.code.set_offset(offset);
            offset - std::mem::size_of::<Reg>()
        } else {
            return;
        };
        let num_regs = unsafe { self.code.read_at_unchecked::<Reg>(offset) };
        self.gc.pop_frame(num_regs);
    }

    #[cfg(not(feature = "unsafe"))]
    fn pop_frame(&mut self) {
        let offset = if let Some(offset) = self.call_stack.pop() {
            self.code.set_offset(offset);
            offset - std::mem::size_of::<Reg>()
        } else {
            return;
        };
        let num_regs = self.code.read_at::<Reg>(offset).unwrap();
        self.gc.pop_frame(num_regs);
    }
}

#[cfg(feature = "unsafe")]
macro_rules! take {
    ($vm:ident, $ty:ty) => {
        unsafe { $vm.code.take_unchecked::<$ty>() }
    };
}

#[cfg(not(feature = "unsafe"))]
macro_rules! take {
    ($vm:ident, $ty:ty) => {
        $vm.code.take::<$ty>().unwrap()
    };
}

macro_rules! skip {
    ($vm:ident, $ty:ty) => {
        $vm.code.skip::<$ty>()
    };
}

macro_rules! load {
    (mut $vm:ident) => {
        &mut $vm.gc[take!($vm, Reg)]
    };
    ($vm:ident) => {
        $vm.gc[take!($vm, Reg)]
    };
}

fn func<R: Read>(vm: &mut Vm<R>) {
    jump(vm);
}

fn copy_a<R: Read>(vm: &mut Vm<R>) {
    let addr = take!(vm, Addr);
    *load!(mut vm) = Value::int_unchecked(addr.into());
}

fn copy_r<R: Read>(vm: &mut Vm<R>) {
    *load!(mut vm) = load!(vm);
}

fn jump<R: Read>(vm: &mut Vm<R>) {
    let addr = take!(vm, Addr);
    vm.code.set_offset(addr as _);
}

fn jump_ez<R: Read>(vm: &mut Vm<R>) {
    if load!(vm) == 0 {
        vm.code.set_offset(take!(vm, Addr) as _);
    } else {
        skip!(vm, Addr);
    }
}

#[allow(clippy::if_not_else)]
fn jump_nz<R: Read>(vm: &mut Vm<R>) {
    if load!(vm) != 0 {
        vm.code.set_offset(take!(vm, Addr) as _);
    } else {
        skip!(vm, Addr);
    }
}

macro_rules! binary_jump_op {
    (rr $name:ident $op:tt) => {
        fn $name<R: Read>(vm: &mut Vm<R>) {
            if load!(vm) $op load!(vm) {
                vm.code.set_offset(take!(vm, Addr) as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
    (ri $name:ident $op:tt) => {
        fn $name<R: Read>(vm: &mut Vm<R>) {
            if load!(vm) $op take!(vm, Value) {
                vm.code.set_offset(take!(vm, Addr) as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
    (ir $name:ident $op:tt) => {
        fn $name<R: Read>(vm: &mut Vm<R>) {
            if take!(vm, Value) $op load!(vm) {
                vm.code.set_offset(take!(vm, Addr) as _);
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
        fn $name<R: Read>(vm: &mut Vm<R>) {
            let addr = take!(vm, Addr);

            $(
            let $arg = load!(vm);
            )*

            let num_regs = take!(vm, Reg);
            vm.push_frame(num_regs, addr as usize);

            $(
            vm.gc[$i as Reg] = $arg;
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

fn callv<R: Read>(vm: &mut Vm<R>) {
    let addr = take!(vm, Addr);
    let num_args = take!(vm, Int);
    let mut args = Vec::with_capacity(num_args as usize);
    for _ in 0..num_args {
        args.push(load!(vm));
    }
    let num_regs = take!(vm, Reg);
    vm.push_frame(num_regs, addr as usize);

    for i in 0..num_args {
        vm.gc[i as Reg + 1] = args[i as usize];
    }
}

fn djump<R: Read>(vm: &mut Vm<R>) {
    let addr = load!(vm).as_int_unchecked();
    debug_assert!(addr >= 0);
    vm.code.set_offset(addr as usize);
}

macro_rules! dcall_ops {
    ($($name:ident $($num_args:literal)?;)+) => {
        $(
        fn $name<R: Read>(vm: &mut Vm<R>) {
            let addr = load!(vm);
            $(let args = [(); $num_args].map(|_| load!(vm));)?
            let num_regs = take!(vm, Reg);
            vm.push_frame(num_regs, addr.as_int_unchecked() as usize);

            $(
            for i in 0..$num_args {
                vm.gc[i as Reg + 1] = args[i];
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

fn dcallv<R: Read>(vm: &mut Vm<R>) {
    let addr = load!(vm);
    let num_args = take!(vm, Int);
    let mut args = Vec::with_capacity(num_args as usize);
    for _ in 0..num_args {
        args.push(load!(vm));
    }
    let num_regs = take!(vm, Reg);
    vm.push_frame(num_regs, addr.as_int_unchecked() as usize);

    for i in 0..num_args {
        vm.gc[i as Reg + 1] = args[i as usize];
    }
}

fn ret_r<R: Read>(vm: &mut Vm<R>) {
    let ret = load!(vm);
    vm.pop_frame();
    *load!(mut vm) = ret;
}

fn ret_i<R: Read>(vm: &mut Vm<R>) {
    let ret = take!(vm, Value);
    vm.pop_frame();
    *load!(mut vm) = ret;
}

fn value<R: Read>(vm: &mut Vm<R>) {
    let val = take!(vm, Value);
    *load!(mut vm) = val;
}

macro_rules! binary_op {
    (rr $name:ident $op:tt) => {
        fn $name<R: Read>(vm: &mut Vm<R>) {
            let lhs = load!(vm);
            let rhs = load!(vm);
            *load!(mut vm) = lhs $op rhs;
        }
    };
    (ri $name:ident $op:tt) => {
        fn $name<R: Read>(vm: &mut Vm<R>) {
            let lhs = load!(vm);
            let rhs = take!(vm, Value);
            *load!(mut vm) = lhs $op rhs;
        }
    };
    (ir $name:ident $op:tt) => {
        fn $name<R: Read>(vm: &mut Vm<R>) {
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

fn neg<R: Read>(vm: &mut Vm<R>) {
    *load!(mut vm) = load!(vm);
}

fn incr<R: Read>(vm: &mut Vm<R>) {
    *load!(mut vm) += 1;
}

fn decr<R: Read>(vm: &mut Vm<R>) {
    *load!(mut vm) -= 1;
}

fn arr_r<R: Read>(vm: &mut Vm<R>) {
    let len = load!(vm);
    assert!(len >= 0);
    *load!(mut vm) = Value::ptr_unchecked(vm.gc.alloc(len.as_int_unchecked() as _));
    vm.gc.run();
}

fn arr_i<R: Read>(vm: &mut Vm<R>) {
    let len = take!(vm, Int);
    assert!(len >= 0);
    *load!(mut vm) = Value::ptr_unchecked(vm.gc.alloc(len as _));
    vm.gc.run();
}

fn get_r<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    *load!(mut vm) = vm.gc.get(arr, idx as usize);
}

fn get_i<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
    *load!(mut vm) = vm.gc.get(arr, idx as usize);
}

fn set_ii<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
    let val = take!(vm, Value);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn set_rr<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    let val = load!(vm);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn set_ri<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    let val = take!(vm, Value);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn set_ir<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
    let val = take!(vm, Value);
    *vm.gc.get_mut(arr, idx as usize) = val;
}

fn len<R: Read>(vm: &mut Vm<R>) {
    let arr = load!(vm).as_ptr_unchecked();
    *load!(mut vm) = Value::int_unchecked(vm.gc.array_len(arr).into());
}

fn r#type<R: Read>(vm: &mut Vm<R>) {
    let kind = load!(vm).kind() as u8;
    *load!(mut vm) = Value::int_unchecked(kind.into());
}

fn putc_r<R: Read>(vm: &mut Vm<R>) {
    let ch = load!(vm).as_int_unchecked();
    vm.write(ch.try_into().unwrap());
}

fn putc_i<R: Read>(vm: &mut Vm<R>) {
    let ch = take!(vm, u32);
    vm.write(ch);
}
