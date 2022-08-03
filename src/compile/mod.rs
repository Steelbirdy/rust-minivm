mod assembler;
mod bytecode;
mod debug;

pub use assembler::Assembler;
pub use bytecode::Bytecode;
pub use debug::disassemble;

use crate::{
    common::{Interner, OwnedByteBuffer},
    parse::SyntaxTree,
};

pub fn assemble(tree: &SyntaxTree, interner: &mut Interner) -> OwnedByteBuffer {
    OwnedByteBuffer::new(Assembler::new(tree, interner).finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::ByteReader;
    use expect_test::{expect, Expect};

    fn check(source: &str, expect: Expect) {
        let mut interner = Interner::new();
        let result = crate::parse::parse(source);
        let bytecode = Assembler::new(&result.syntax_tree(), &mut interner).finish();
        let reader = ByteReader::new(&bytecode);
        let actual = disassemble(reader);
        expect.assert_eq(&actual);
    }

    #[test]
    fn empty() {
        check(
            "\
func __entry
end",
            expect![
                "\
0000 | jump      [@12]
0005 | func      [@12, r1]"
            ],
        );
    }

    #[test]
    fn int_literal() {
        check(
            "\
func __entry
    r0 <- int 10
    r0 <- int -10
end",
            expect![
                "\
0000 | jump      [@12]
0005 | func      [@34, r1]
0012 | int       [10, r0]
0023 | int       [-10, r0]"
            ],
        );
    }

    #[test]
    fn string_literal() {
        check(
            "\
func __entry
    r0 <- str :hello
end",
            expect![
                "\
0000 | jump      [@12]
0005 | func      [@63, r1]
0012 | str       [5, 'h', 'e', 'l', 'l', 'o', r0]"
            ],
        );
    }

    #[test]
    fn labels_and_jumps() {
        check(
            "\
func __entry
@start
    jump start
end",
            expect![
                "\
0000 | jump      [@12]
0005 | func      [@17, r1]
0012 | jump      [@12]"
            ],
        )
    }

    #[test]
    fn call_without_args() {
        check(
            "\
func __entry
    r0 <- call main
    exit
end

func main
    r2 <- int 0
    ret r2
end",
            expect![
                "\
0000 | jump      [@12]
0005 | func      [@28, r1]
0012 | call      [@35, 0, r0]
0027 | exit      []
0028 | func      [@49, r3]
0035 | int       [0, r2]
0046 | ret_r     [r2]"
            ],
        )
    }

    #[test]
    fn binary_op_folding() {
        check(
            "\
func __entry
    r0 <- add 2 1
    r0 <- sub r0 0
    r0 <- mul r0 1
    r0 <- div r0 1
    r0 <- mod r0 1
end",
            expect![
                "\
0000 | jump      [@12]
0005 | func      [@34, r1]
0012 | int       [3, r0]
0023 | int       [0, r0]"
            ],
        )
    }
}
