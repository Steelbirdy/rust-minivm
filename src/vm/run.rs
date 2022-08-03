#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]

use crate::{
    common::{
        config::{self, RunConfig},
        Addr, ByteReader, Int, Read, Reg,
    },
    vm::{disassemble_instruction, Gc, Opcode, Stack, Value},
};
use std::io::{BufWriter, StdoutLock, Write};

pub struct Vm<'a> {
    code: ByteReader<'a>,
    gc: &'a mut Gc,
    call_stack: Stack<usize, { config::NUM_FRAMES }>,
    stop: bool,
    stdout: BufWriter<StdoutLock<'static>>,
    char_buf: [u8; 4],
    buf: String,
    config: &'a RunConfig,
}

impl<'a> Vm<'a> {
    pub fn new(code: ByteReader<'a>, gc: &'a mut Gc, config: &'a RunConfig) -> Self {
        Self {
            code,
            gc,
            call_stack: Stack::new(),
            stop: false,
            stdout: BufWriter::new(std::io::stdout().lock()),
            char_buf: [0; 4],
            buf: String::new(),
            config,
        }
    }

    pub fn run(&mut self) -> Value {
        const OUT: Reg = 0;
        while !self.code.is_at_end() && !self.stop {
            self.next();
        }
        self.gc[OUT]
    }

    #[cfg(feature = "unsafe")]
    fn next(&mut self) {
        if self.config.trace_execution {
            self.debug_next();
        }
        unsafe {
            let opcode = self.code.take_unchecked::<Opcode>();
            let func = OPCODE_FN_PTRS.get_unchecked(opcode as usize);
            func(self);
        }
    }

    #[cfg(not(feature = "unsafe"))]
    fn next(&mut self) {
        if self.config.trace_execution {
            self.debug_next();
        }
        let opcode = self.code.take::<Opcode>().unwrap();
        let func = OPCODE_FN_PTRS[opcode as usize];
        func(self);
    }

    fn debug_next(&mut self) {
        self.code
            .at_checkpoint(|| disassemble_instruction(&self.code, &mut self.buf));
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
            self.stop = true;
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
            self.stop = true;
            return;
        };
        let num_regs = self.code.read_at::<Reg>(offset).unwrap();
        self.gc.pop_frame(num_regs);
    }
}

type OpcodeFn = fn(&mut Vm);

static OPCODE_FN_PTRS: &[OpcodeFn] = &[
    exit, func, copy_a, copy_r, jump, jump_ez, jump_nz, jumplt_rr, jumplt_ri, jumplt_ir, jumple_rr,
    jumple_ri, jumple_ir, jumpeq_rr, jumpeq_ir, jumpne_rr, jumpne_ir, call0, call1, call2, call3,
    call4, call5, call6, call7, call8, callv, djump, dcall0, dcall1, dcall2, dcall3, dcall4,
    dcall5, dcall6, dcall7, dcall8, dcallv, ret_r, ret_i, value, add_rr, add_ir, sub_rr, sub_ri,
    sub_ir, mul_rr, mul_ir, div_rr, div_ri, div_ir, mod_rr, mod_ri, mod_ir, neg, incr, decr, arr_r,
    arr_i, get_r, get_i, set_ii, set_rr, set_ri, set_ir, len, r#type, putc_r, putc_i, __max,
];

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

fn exit(vm: &mut Vm) {
    vm.stop = true;
}

fn func(vm: &mut Vm) {
    jump(vm);
}

fn copy_a(vm: &mut Vm) {
    let addr = take!(vm, Addr);
    *load!(mut vm) = Value::int_unchecked(addr.into());
}

fn copy_r(vm: &mut Vm) {
    *load!(mut vm) = load!(vm);
}

fn jump(vm: &mut Vm) {
    let addr = take!(vm, Addr);
    vm.code.set_offset(addr as _);
}

fn jump_ez(vm: &mut Vm) {
    if load!(vm) == 0 {
        vm.code.set_offset(take!(vm, Addr) as _);
    } else {
        skip!(vm, Addr);
    }
}

#[allow(clippy::if_not_else)]
fn jump_nz(vm: &mut Vm) {
    if load!(vm) != 0 {
        vm.code.set_offset(take!(vm, Addr) as _);
    } else {
        skip!(vm, Addr);
    }
}

macro_rules! binary_jump_op {
    (rr $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            if load!(vm) $op load!(vm) {
                vm.code.set_offset(take!(vm, Addr) as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
    (ri $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
            if load!(vm) $op take!(vm, Value) {
                vm.code.set_offset(take!(vm, Addr) as _);
            } else {
                skip!(vm, Addr);
            }
        }
    };
    (ir $name:ident $op:tt) => {
        fn $name(vm: &mut Vm) {
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
        fn $name(vm: &mut Vm) {
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
        vm.gc[i as Reg + 1] = args[i as usize];
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
        vm.gc[i as Reg + 1] = args[i as usize];
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
    vm.gc.run();
}

fn arr_i(vm: &mut Vm) {
    let len = take!(vm, Int);
    assert!(len >= 0);
    *load!(mut vm) = Value::ptr_unchecked(vm.gc.alloc(len as _));
    vm.gc.run();
}

fn get_r(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = load!(vm).as_int_unchecked();
    *load!(mut vm) = vm.gc.get(arr, idx as usize);
}

fn get_i(vm: &mut Vm) {
    let arr = load!(vm).as_ptr_unchecked();
    let idx = take!(vm, Int);
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

fn __max(_vm: &mut Vm) {
    unimplemented!()
}
