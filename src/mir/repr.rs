use crate::{
    common::{Int, Reg},
    vm::Value,
};
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Exit,
    Func {
        end: usize,
        num_regs: Reg,
    },
    RegA {
        addr: usize,
        to: Reg,
    },
    RegR {
        from: Reg,
        to: Reg,
    },
    BranchNz {
        cond: Reg,
        addr_f: usize,
        addr_t: usize,
    },
    BranchEqRR {
        lhs: Reg,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    },
    BranchEqIR {
        lhs: Int,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    },
    BranchLtRR {
        lhs: Reg,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    },
    BranchLtRI {
        lhs: Reg,
        rhs: Int,
        addr_f: usize,
        addr_t: usize,
    },
    BranchLtIR {
        lhs: Int,
        rhs: Reg,
        addr_f: usize,
        addr_t: usize,
    },
    Jump {
        addr: usize,
    },
    JumpEz {
        cond: Reg,
        addr: usize,
    },
    JumpNz {
        cond: Reg,
        addr: usize,
    },
    JumpLtRR {
        lhs: Reg,
        rhs: Reg,
        addr: usize,
    },
    JumpLtRI {
        lhs: Reg,
        rhs: Value,
        addr: usize,
    },
    JumpLtIR {
        lhs: Value,
        rhs: Reg,
        addr: usize,
    },
    JumpLeRR {
        lhs: Reg,
        rhs: Reg,
        addr: usize,
    },
    JumpLeRI {
        lhs: Reg,
        rhs: Value,
        addr: usize,
    },
    JumpLeIR {
        lhs: Value,
        rhs: Reg,
        addr: usize,
    },
    JumpEqRR {
        lhs: Reg,
        rhs: Reg,
        addr: usize,
    },
    JumpEqIR {
        lhs: Value,
        rhs: Reg,
        addr: usize,
    },
    JumpNeRR {
        lhs: Reg,
        rhs: Reg,
        addr: usize,
    },
    JumpNeIR {
        lhs: Value,
        rhs: Reg,
        addr: usize,
    },
    Call0 {
        addr: usize,
        num_regs: Reg,
        to: Reg,
    },
    Call1 {
        addr: usize,
        args: [Reg; 1],
        num_regs: Reg,
        to: Reg,
    },
    Call2 {
        addr: usize,
        args: [Reg; 2],
        num_regs: Reg,
        to: Reg,
    },
    Call3 {
        addr: usize,
        args: [Reg; 3],
        num_regs: Reg,
        to: Reg,
    },
    Call4 {
        addr: usize,
        args: [Reg; 4],
        num_regs: Reg,
        to: Reg,
    },
    Call5 {
        addr: usize,
        args: [Reg; 5],
        num_regs: Reg,
        to: Reg,
    },
    Call6 {
        addr: usize,
        args: [Reg; 6],
        num_regs: Reg,
        to: Reg,
    },
    Call7 {
        addr: usize,
        args: [Reg; 7],
        num_regs: Reg,
        to: Reg,
    },
    Call8 {
        addr: usize,
        args: [Reg; 8],
        num_regs: Reg,
        to: Reg,
    },
    CallL {
        addr: usize,
        num_args: u32,
        args: Vec<Reg>,
        num_regs: Reg,
        to: Reg,
    },
    DJump {
        addr: Reg,
    },
    DCall0 {
        addr: Reg,
        num_regs: Reg,
        to: Reg,
    },
    DCall1 {
        addr: Reg,
        args: [Reg; 1],
        num_regs: Reg,
        to: Reg,
    },
    DCall2 {
        addr: Reg,
        args: [Reg; 2],
        num_regs: Reg,
        to: Reg,
    },
    DCall3 {
        addr: Reg,
        args: [Reg; 3],
        num_regs: Reg,
        to: Reg,
    },
    DCall4 {
        addr: Reg,
        args: [Reg; 4],
        num_regs: Reg,
        to: Reg,
    },
    DCall5 {
        addr: Reg,
        args: [Reg; 5],
        num_regs: Reg,
        to: Reg,
    },
    DCall6 {
        addr: Reg,
        args: [Reg; 6],
        num_regs: Reg,
        to: Reg,
    },
    DCall7 {
        addr: Reg,
        args: [Reg; 7],
        num_regs: Reg,
        to: Reg,
    },
    DCall8 {
        addr: Reg,
        args: [Reg; 8],
        num_regs: Reg,
        to: Reg,
    },
    DCallL {
        addr: Reg,
        num_args: u32,
        args: Vec<Reg>,
        num_regs: Reg,
        to: Reg,
    },
    RetR {
        reg: Reg,
    },
    RetV {
        val: Value,
    },
    Value {
        val: Value,
        to: Reg,
    },
    AddRR {
        lhs: Reg,
        rhs: Reg,
        to: Reg,
    },
    AddIR {
        lhs: Int,
        rhs: Reg,
        to: Reg,
    },
    SubRR {
        lhs: Reg,
        rhs: Reg,
        to: Reg,
    },
    SubRI {
        lhs: Reg,
        rhs: Int,
        to: Reg,
    },
    SubIR {
        lhs: Int,
        rhs: Reg,
        to: Reg,
    },
    MulRR {
        lhs: Reg,
        rhs: Reg,
        to: Reg,
    },
    MulIR {
        lhs: Int,
        rhs: Reg,
        to: Reg,
    },
    DivRR {
        lhs: Reg,
        rhs: Reg,
        to: Reg,
    },
    DivRI {
        lhs: Reg,
        rhs: Int,
        to: Reg,
    },
    DivIR {
        lhs: Int,
        rhs: Reg,
        to: Reg,
    },
    ModRR {
        lhs: Reg,
        rhs: Reg,
        to: Reg,
    },
    ModRI {
        lhs: Reg,
        rhs: Int,
        to: Reg,
    },
    ModIR {
        lhs: Int,
        rhs: Reg,
        to: Reg,
    },
    Neg {
        reg: Reg,
        to: Reg,
    },
    Incr {
        reg: Reg,
    },
    Decr {
        reg: Reg,
    },
    ArrR {
        len: Reg,
        to: Reg,
    },
    ArrI {
        len: Int,
        to: Reg,
    },
    GetR {
        arr: Reg,
        idx: Reg,
        to: Reg,
    },
    GetI {
        arr: Reg,
        idx: Int,
        to: Reg,
    },
    SetII {
        arr: Reg,
        idx: Int,
        val: Value,
    },
    SetRR {
        arr: Reg,
        idx: Reg,
        val: Reg,
    },
    SetRI {
        arr: Reg,
        idx: Reg,
        val: Value,
    },
    SetIR {
        arr: Reg,
        idx: Int,
        val: Reg,
    },
    Len {
        arr: Reg,
        to: Reg,
    },
    Type {
        obj: Reg,
        to: Reg,
    },
    PutcR {
        ch: Reg,
    },
    PutcI {
        ch: u32,
    },
}

impl Instruction {
    pub fn call(addr: usize, args: &[Reg], num_regs: Reg, to: Reg) -> Self {
        match args.len() {
            0 => Self::Call0 { addr, num_regs, to },
            1 => Self::Call1 {
                addr,
                args: [args[0]],
                num_regs,
                to,
            },
            2 => Self::Call2 {
                addr,
                args: [args[0], args[1]],
                num_regs,
                to,
            },
            3 => Self::Call3 {
                addr,
                args: [args[0], args[1], args[2]],
                num_regs,
                to,
            },
            4 => Self::Call4 {
                addr,
                args: [args[0], args[1], args[2], args[3]],
                num_regs,
                to,
            },
            5 => Self::Call5 {
                addr,
                args: [args[0], args[1], args[2], args[3], args[4]],
                num_regs,
                to,
            },
            6 => Self::Call6 {
                addr,
                args: [args[0], args[1], args[2], args[3], args[4], args[5]],
                num_regs,
                to,
            },
            7 => Self::Call7 {
                addr,
                args: [
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6],
                ],
                num_regs,
                to,
            },
            8 => Self::Call8 {
                addr,
                args: [
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
                ],
                num_regs,
                to,
            },
            _ => {
                let num_args: u32 = args.len().try_into().unwrap();
                Self::CallL {
                    addr,
                    num_args,
                    args: args.to_vec(),
                    num_regs,
                    to,
                }
            }
        }
    }

    pub fn dcall(addr: Reg, args: &[Reg], num_regs: Reg, to: Reg) -> Self {
        match args.len() {
            0 => Self::DCall0 { addr, num_regs, to },
            1 => Self::DCall1 {
                addr,
                args: [args[0]],
                num_regs,
                to,
            },
            2 => Self::DCall2 {
                addr,
                args: [args[0], args[1]],
                num_regs,
                to,
            },
            3 => Self::DCall3 {
                addr,
                args: [args[0], args[1], args[2]],
                num_regs,
                to,
            },
            4 => Self::DCall4 {
                addr,
                args: [args[0], args[1], args[2], args[3]],
                num_regs,
                to,
            },
            5 => Self::DCall5 {
                addr,
                args: [args[0], args[1], args[2], args[3], args[4]],
                num_regs,
                to,
            },
            6 => Self::DCall6 {
                addr,
                args: [args[0], args[1], args[2], args[3], args[4], args[5]],
                num_regs,
                to,
            },
            7 => Self::DCall7 {
                addr,
                args: [
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6],
                ],
                num_regs,
                to,
            },
            8 => Self::DCall8 {
                addr,
                args: [
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
                ],
                num_regs,
                to,
            },
            _ => {
                let num_args: u32 = args.len().try_into().unwrap();
                Self::DCallL {
                    addr,
                    num_args,
                    args: args.to_vec(),
                    num_regs,
                    to,
                }
            }
        }
    }

    pub fn debug(&self, buf: &mut String) -> std::fmt::Result {
        use Instruction::*;

        const LONGEST_NAME_LEN: usize = "jumpeq_rr".len();

        match self {
            Exit => {
                write!(buf, "exit")
            }
            Func { end, num_regs } => {
                write!(
                    buf,
                    "{:<.*} [@{end}] [{num_regs}]",
                    LONGEST_NAME_LEN, "func"
                )
            }
            RegA { addr, to } => {
                write!(buf, "{:<.*} [@{addr}] [r{to}]", LONGEST_NAME_LEN, "reg_a")
            }
            RegR { from, to } => {
                write!(buf, "{:<.*} [r{from}] [r{to}]", LONGEST_NAME_LEN, "reg_r")
            }
            BranchNz {
                cond,
                addr_f,
                addr_t,
            } => {
                write!(
                    buf,
                    "{:<.*} [r{cond}] [@{addr_f}] [@{addr_t}]",
                    LONGEST_NAME_LEN, "bb"
                )
            }
            BranchEqRR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [@{addr_f}] [@{addr_t}]",
                    LONGEST_NAME_LEN, "beq_rr"
                )
            }
            BranchEqIR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(
                    buf,
                    "{:<.*} [{lhs}] [r{rhs}] [@{addr_f}] [@{addr_t}]",
                    LONGEST_NAME_LEN, "beq_ir"
                )
            }
            BranchLtRR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [@{addr_f}] [@{addr_t}]",
                    LONGEST_NAME_LEN, "blt_rr"
                )
            }
            BranchLtRI {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [{rhs}] [@{addr_f}] [@{addr_t}]",
                    LONGEST_NAME_LEN, "blt_ri"
                )
            }
            BranchLtIR {
                lhs,
                rhs,
                addr_f,
                addr_t,
            } => {
                write!(
                    buf,
                    "{:<.*} [{lhs}] [r{rhs}] [@{addr_f}] [@{addr_t}]",
                    LONGEST_NAME_LEN, "blt_ir"
                )
            }
            Jump { addr } => {
                write!(buf, "{:<.*} [@{addr}]", LONGEST_NAME_LEN, "jump")
            }
            JumpEz { cond, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{cond}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumpez"
                )
            }
            JumpNz { cond, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{cond}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumpnz"
                )
            }
            JumpLtRR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumplt_rr"
                )
            }
            JumpLtRI { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [{rhs:?}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumplt_ri"
                )
            }
            JumpLtIR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumplt_ir"
                )
            }
            JumpLeRR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumple_rr"
                )
            }
            JumpLeRI { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [{rhs:?}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumple_ri"
                )
            }
            JumpLeIR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumple_ir"
                )
            }
            JumpEqRR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumpeq_rr"
                )
            }
            JumpEqIR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumpeq_ir"
                )
            }
            JumpNeRR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumpne_rr"
                )
            }
            JumpNeIR { lhs, rhs, addr } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [@{addr}]",
                    LONGEST_NAME_LEN, "jumpne_ir"
                )
            }
            Call0 { addr, num_regs, to } => {
                write!(
                    buf,
                    "{:<.*} [@{addr}] [{num_regs}] [r{to}]",
                    LONGEST_NAME_LEN, "call0"
                )
            }
            Call1 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call1")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call2 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call2")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call3 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call3")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call4 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call4")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call5 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call5")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call6 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call6")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call7 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call7")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            Call8 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [@{addr}] [", LONGEST_NAME_LEN, "call8")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            CallL {
                addr,
                num_args,
                args,
                num_regs,
                to,
            } => {
                write!(
                    buf,
                    "{:<.*} [@{addr}] [{num_args}] [",
                    LONGEST_NAME_LEN, "callL"
                )?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DJump { addr } => {
                write!(buf, "{:<.*} [@{addr}]", LONGEST_NAME_LEN, "djump")
            }
            DCall0 { addr, num_regs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{addr}] [{num_regs}] [r{to}]",
                    LONGEST_NAME_LEN, "dcall0"
                )
            }
            DCall1 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall1")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall2 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall2")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall3 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall3")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall4 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall4")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall5 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall5")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall6 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall6")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall7 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall7")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCall8 {
                addr,
                args,
                num_regs,
                to,
            } => {
                write!(buf, "{:<.*} [r{addr}] [", LONGEST_NAME_LEN, "dcall8")?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            DCallL {
                addr,
                num_args,
                args,
                num_regs,
                to,
            } => {
                write!(
                    buf,
                    "{:<.*} [r{addr}] [{num_args}] [",
                    LONGEST_NAME_LEN, "dcall1"
                )?;
                for &arg in &args[..args.len() - 1] {
                    write!(buf, "r{arg}, ")?;
                }
                write!(buf, "r{}] [{num_regs}] [r{to}]", args[args.len() - 1])
            }
            RetR { reg } => {
                write!(buf, "{:<.*} [r{reg}]", LONGEST_NAME_LEN, "ret_r")
            }
            RetV { val } => {
                write!(buf, "{:<.*} [{val:?}]", LONGEST_NAME_LEN, "ret_v")
            }
            Value { val, to } => {
                write!(buf, "{:<.*} [{val:?}] [r{to}]", LONGEST_NAME_LEN, "value")
            }
            AddRR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "add_rr"
                )
            }
            AddIR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "add_ir"
                )
            }
            SubRR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "sub_rr"
                )
            }
            SubRI { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [{rhs:?}] [r{to}]",
                    LONGEST_NAME_LEN, "sub_ri"
                )
            }
            SubIR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "sub_ir"
                )
            }
            MulRR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "mul_rr"
                )
            }
            MulIR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "mul_ir"
                )
            }
            DivRR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "div_rr"
                )
            }
            DivRI { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [{rhs:?}] [r{to}]",
                    LONGEST_NAME_LEN, "div_ri"
                )
            }
            DivIR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "div_ir"
                )
            }
            ModRR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "mod_rr"
                )
            }
            ModRI { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [r{lhs}] [{rhs:?}] [r{to}]",
                    LONGEST_NAME_LEN, "mod_ri"
                )
            }
            ModIR { lhs, rhs, to } => {
                write!(
                    buf,
                    "{:<.*} [{lhs:?}] [r{rhs}] [r{to}]",
                    LONGEST_NAME_LEN, "mod_ir"
                )
            }
            Neg { reg, to } => {
                write!(buf, "{:<.*} [r{reg}] [r{to}]", LONGEST_NAME_LEN, "neg")
            }
            Incr { reg } => {
                write!(buf, "{:<.*} [r{reg}]", LONGEST_NAME_LEN, "incr")
            }
            Decr { reg } => {
                write!(buf, "{:<.*} [r{reg}]", LONGEST_NAME_LEN, "decr")
            }
            ArrR { len, to } => {
                write!(buf, "{:<.*} [r{len}] [r{to}]", LONGEST_NAME_LEN, "arr_r")
            }
            ArrI { len, to } => {
                write!(buf, "{:<.*} [{len}] [r{to}]", LONGEST_NAME_LEN, "arr_i")
            }
            GetR { arr, idx, to } => {
                write!(
                    buf,
                    "{:<.*} [r{arr}] [r{idx}] [r{to}]",
                    LONGEST_NAME_LEN, "get_r"
                )
            }
            GetI { arr, idx, to } => {
                write!(
                    buf,
                    "{:<.*} [r{arr}] [{idx}] [r{to}]",
                    LONGEST_NAME_LEN, "get_i"
                )
            }
            SetII { arr, idx, val } => {
                write!(
                    buf,
                    "{:<.*} [r{arr}] [{idx}] [{val:?}]",
                    LONGEST_NAME_LEN, "set_ii"
                )
            }
            SetRR { arr, idx, val } => {
                write!(
                    buf,
                    "{:<.*} [r{arr}] [r{idx}] [r{val}]",
                    LONGEST_NAME_LEN, "set_rr"
                )
            }
            SetRI { arr, idx, val } => {
                write!(
                    buf,
                    "{:<.*} [r{arr}] [r{idx}] [{val:?}]",
                    LONGEST_NAME_LEN, "set_ri"
                )
            }
            SetIR { arr, idx, val } => {
                write!(
                    buf,
                    "{:<.*} [r{arr}] [{idx}] [r{val}]",
                    LONGEST_NAME_LEN, "set_ir"
                )
            }
            Len { arr, to } => {
                write!(buf, "{:<.*} [r{arr}] [r{to}]", LONGEST_NAME_LEN, "len")
            }
            Type { obj, to } => {
                write!(buf, "{:<.*} [r{obj}] [r{to}]", LONGEST_NAME_LEN, "type")
            }
            PutcR { ch } => {
                write!(buf, "{:<.*} [r{ch}]", LONGEST_NAME_LEN, "putc_r")
            }
            PutcI { ch } => {
                write!(
                    buf,
                    "{:<.*} [{:?}]",
                    LONGEST_NAME_LEN,
                    "putc_i",
                    char::from_u32(*ch).unwrap()
                )
            }
        }
    }
}
