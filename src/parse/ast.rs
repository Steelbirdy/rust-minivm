use crate::{common::ENTRY_POINT_LBL, parse::ParseError};
use eventree_wrapper::{
    ast_node, ast_token,
    eventree::{SyntaxKind, SyntaxTree},
    syntax_tree::{AstNode, AstToken},
};

pub fn validate(tree: &SyntaxTree<Cfg>) -> Result<(), Vec<ParseError>> {
    let root = Root::cast(tree.root(), tree).unwrap();
    let mut errors = Vec::new();
    if root.entry_point(tree).is_none() {
        errors.push(ParseError::NoEntryPoint);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum NodeKind {
    Root,
    Function,
    LabelDef,
    InstrAdd,
    InstrAddr,
    InstrArr,
    InstrCall,
    InstrCopy,
    InstrDecr,
    InstrDiv,
    InstrExit,
    InstrGet,
    InstrIncr,
    InstrInt,
    InstrJump,
    InstrJumpEq,
    InstrJumpEz,
    InstrJumpGe,
    InstrJumpGt,
    InstrJumpLe,
    InstrJumpLt,
    InstrJumpNe,
    InstrJumpNz,
    InstrLen,
    InstrMod,
    InstrMul,
    InstrNeg,
    InstrPutc,
    InstrRet,
    InstrSet,
    InstrStr,
    InstrSub,
    InstrType,
    Error,
}

#[allow(unsafe_code)]
#[allow(clippy::cast_possible_truncation)]
unsafe impl SyntaxKind for NodeKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        std::mem::transmute(raw as u8)
    }
}

type Cfg = crate::parse::ParseConfig;

ast_node! { <Cfg> pub Root
    fn functions = nodes(Function);
}
impl Root {
    pub fn entry_point(self, tree: &SyntaxTree<Cfg>) -> Option<Function> {
        self.functions(tree).find(|f| {
            f.name(tree)
                .map_or(false, |lbl| lbl.text(tree) == ENTRY_POINT_LBL)
        })
    }
}
ast_node! { <Cfg> pub Function
    fn name = token(Label);
    fn instructions = nodes(Instruction);
}
ast_node! { <Cfg> pub Instruction => [
    Label(LabelDef),
    Add(InstrAdd),
    Addr(InstrAddr),
    Arr(InstrArr),
    Call(InstrCall),
    Copy(InstrCopy),
    Decr(InstrDecr),
    Div(InstrDiv),
    Exit(InstrExit),
    Get(InstrGet),
    Incr(InstrIncr),
    Int(InstrInt),
    Jump(InstrJump),
    JumpEq(InstrJumpEq),
    JumpEz(InstrJumpEz),
    JumpGe(InstrJumpGe),
    JumpGt(InstrJumpGt),
    JumpLe(InstrJumpLe),
    JumpLt(InstrJumpLt),
    JumpNe(InstrJumpNe),
    JumpNz(InstrJumpNz),
    Len(InstrLen),
    Mod(InstrMod),
    Mul(InstrMul),
    Neg(InstrNeg),
    Putc(InstrPutc),
    Ret(InstrRet),
    Set(InstrSet),
    Str(InstrStr),
    Sub(InstrSub),
    Type(InstrType),
] }
ast_node! { <Cfg> pub LabelDef
    fn name = token(Label);
}
ast_node! { <Cfg> pub InstrAdd
    fn to = token(Register);
    fn lhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn rhs = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrAddr
    fn to = token(Register);
    fn label = token(Label);
}
ast_node! { <Cfg> pub InstrArr
    fn to = token(Register);
    fn len = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrCall
    fn to = token(Register);
    fn func = tokens(LblOrReg).nth(1) -> Option<LblOrReg>;
}
impl InstrCall {
    pub fn args(self, tree: &SyntaxTree<Cfg>) -> impl Iterator<Item = Register> + '_ {
        let skip = if let Some(LblOrReg::Reg(_)) = self.func(tree) {
            2
        } else {
            1
        };
        self.0
            .child_tokens(tree)
            .filter_map(|x| Register::cast(x, tree))
            .skip(skip)
    }
}
ast_node! { <Cfg> pub InstrCopy
    fn to = token(Register);
    fn from = tokens(Register).nth(1) -> Option<Register>;
}
ast_node! { <Cfg> pub InstrDecr
    fn reg = token(Register);
}
ast_node! { <Cfg> pub InstrDiv
    fn to = token(Register);
    fn lhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn rhs = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrExit }
ast_node! { <Cfg> pub InstrGet
    fn to = token(Register);
    fn arr = tokens(Register).nth(1) -> Option<Register>;
    fn idx = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrIncr
    fn reg = token(Register);
}
ast_node! { <Cfg> pub InstrInt
    fn to = token(Register);
    fn value = token(CharOrInt);
}
ast_node! { <Cfg> pub InstrJump
    fn addr = token(LblOrReg);
}
ast_node! { <Cfg> pub InstrJumpEq
    fn lhs = token(IntOrReg);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpEz
    fn cond = token(IntOrReg);
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpGe
    fn lhs = token(IntOrReg);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpGt
    fn lhs = token(IntOrReg);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpLe
    fn lhs = token(IntOrReg);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpLt
    fn lhs = token(IntOrReg);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpNe
    fn lhs = token(IntOrReg);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrJumpNz
    fn cond = token(IntOrReg);
    fn addr = token(Label);
}
ast_node! { <Cfg> pub InstrLen
    fn to = token(Register);
    fn arr = tokens(Register).nth(1) -> Option<Register>;
}
ast_node! { <Cfg> pub InstrMod
    fn to = token(Register);
    fn lhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn rhs = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrMul
    fn to = token(Register);
    fn lhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn rhs = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrNeg
    fn to = token(Register);
    fn rhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrPutc
    fn char = token(IntOrReg);
}
ast_node! { <Cfg> pub InstrRet
    fn ret = token(IntOrReg);
}
ast_node! { <Cfg> pub InstrSet
    fn arr = token(Register);
    fn idx = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn val = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrStr
    fn to = token(Register);
    fn string = token(StringLiteral);
}
ast_node! { <Cfg> pub InstrSub
    fn to = token(Register);
    fn lhs = tokens(IntOrReg).nth(1) -> Option<IntOrReg>;
    fn rhs = tokens(IntOrReg).nth(2) -> Option<IntOrReg>;
}
ast_node! { <Cfg> pub InstrType
    fn to = token(Register);
    fn obj = tokens(Register).nth(1) -> Option<Register>;
}

ast_token! { <Cfg> pub CharOrInt => [Char(CharLiteral), Int(IntLiteral)] }
ast_token! { <Cfg> pub IntOrReg => [Char(CharLiteral), Int(IntLiteral), Reg(Register)] }
ast_token! { <Cfg> pub LblOrReg => [Lbl(Label), Reg(Register)] }
ast_token! { <Cfg> pub Label }
ast_token! { <Cfg> pub IntLiteral }
ast_token! { <Cfg> pub StringLiteral }
ast_token! { <Cfg> pub CharLiteral }
ast_token! { <Cfg> pub Register }
