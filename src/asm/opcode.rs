pub type OpCodeRepr = u32;

/// Notes
/// * The `val` type is an alias for `int|array`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u32)]
pub enum OpCode {
    // === Instruction Size 1 ===
    /// exit
    Exit,

    // === Instruction Size 2 ===
    /// func <dest(func)> (synthetic)
    Function,
    /// jump <dest(label)>
    Jump,
    /// incr <to(reg)>
    Increment,
    /// decr <to(reg)>
    Decrement,
    /// putc <char(reg)>
    PutChar,
    /// ret <obj(reg)>
    Return,

    // === Instruction Size 3 ===
    /// <to(reg)> <- copy <from(reg)>
    Copy,
    /// <to(reg)> <- addr <name(func)>
    AddressOf,
    /// <to(reg)> <- int <value(int)>
    Int,
    /// <to(reg)> <- arr <len(reg|int)>
    Array,
    /// <to(reg)> <- len <arr(reg)>
    ArrayLen,
    /// <to(reg)> <- typ <obj(reg)>
    Type,
    /// <to(reg)> <- neg <obj(reg|val)>
    Neg,
    /// jumpz <obj(reg)> <dest(label)>
    JumpZero,
    /// jumpnz <obj(reg)> <dest(label)>
    JumpNotZero,

    // === Instruction Size 4 ===
    /// <to(reg)> <- add <lhs:(reg|val)> <rhs(reg|val)>
    Add,
    /// <to(reg)> <- sub <lhs:(reg|val)> <rhs(reg|val)>
    Sub,
    /// <to(reg)> <- mul <lhs:(reg|val)> <rhs(reg|val)>
    Mul,
    /// <to(reg)> <- div <lhs:(reg|val)> <rhs(reg|val)>
    Div,
    /// <to(reg)> <- rem <lhs:(reg|val)> <rhs(reg|val)>
    Rem,
    /// jumplt <lhs(reg|val)> <rhs(reg|val)> <dest(label)>
    JumpLt,
    /// jumpgt <lhs(reg|val)> <rhs(reg|val)> <dest:(label)>
    JumpGt,
    /// jumple <lhs(reg|val)> <rhs(reg|val)> <dest(label)>
    JumpLe,
    /// jumpge <lhs(reg|val)> <rhs(reg|val)> <dest(label)>
    JumpGe,
    /// jumpeq <lhs(reg|val)> <rhs(reg|val)> <dest(label)>
    JumpEq,
    /// jumpne <lhs(reg|val)> <rhs(reg|val)> <dest(label)>
    JumpNe,
    /// set <arr(reg)> <idx(reg|val)> <value(reg|val)>
    SetArr,
    /// <to(reg)> <- get <arr(reg)> <idx(reg|val)>
    GetArr,

    /// === Variable-Length Instructions ===
    /// <to(reg)> <- call <name(func)> \[args...(reg|val)]
    /// =>
    /// <<Call>> <to> <name=>(addr)> <len\[args]> \[args...]
    Call,
    /// <to(reg)> <- dcall <addr(reg)> \[args...(reg|val)]
    DynCall,
    /// <to(reg)> <- str :<text(string)>
    String,
}

impl PartialEq<u32> for OpCode {
    fn eq(&self, other: &u32) -> bool {
        (*self as u32) == *other
    }
}
