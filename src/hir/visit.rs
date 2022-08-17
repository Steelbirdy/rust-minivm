use crate::{
    common::{Int, Reg},
    hir::{
        self, BinaryArgs, BinaryOp, BranchKind, JumpKind, KeyWithRange, SetArgs, UnaryArg, UnaryOp,
    },
    vm::Ptr,
};

visit_trait![hir::InstructionKind:
    Exit => visit_exit();
    Label => visit_label(name: KeyWithRange);
    Reg => visit_reg(to: Reg, from: Reg);
    Branch => visit_branch(kind: BranchKind, lbl_false: KeyWithRange, lbl_true: KeyWithRange);
    Jump => visit_jump(kind: JumpKind, lbl: KeyWithRange);
    Call => visit_call(to: Reg, func: KeyWithRange, [&] args: &[Reg]);
    Addr => visit_addr(to: Reg, lbl: KeyWithRange);
    DJump => visit_djump(kind: JumpKind, lbl: Reg);
    DCall => visit_dcall(to: Reg, func: Reg, [&] args: &[Reg]);
    Ret => visit_ret(arg: UnaryArg);
    Int => visit_int(to: Reg, int: Int);
    Str => visit_str(to: Reg, str: KeyWithRange);
    Ptr => visit_ptr(to: Reg, ptr: Ptr);
    Binary => visit_binary(op: BinaryOp, to: Reg, args: BinaryArgs);
    Unary => visit_unary(op: UnaryOp, to: Reg, arg: Reg);
    Incr => visit_incr(reg: Reg);
    Decr => visit_decr(reg: Reg);
    Arr => visit_arr(to: Reg, len: UnaryArg);
    Get => visit_get(to: Reg, arr: Reg, idx: UnaryArg);
    Set => visit_set(arr: Reg, idx_and_val: SetArgs);
    Len => visit_len(to: Reg, arr: Reg);
    Type => visit_type(to: Reg, obj: Reg);
    Putc => visit_putc(arg: UnaryArg);
];
