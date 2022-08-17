mod utils;

use expect_test::expect;

const SOURCE: &str = r#"
func __entry
    r0 <- int 35
    r0 <- call fib r0
    exit
end

func fib
    jumplt r1 2 fib.ret
    decr r1
    r0 <- sub r1 1
    r0 <- call fib r0
    r1 <- call fib r1
    r1 <- add r0 r1
@fib.ret
    ret r1
end"#;

#[test]
fn fib35() {
    utils::check(SOURCE, expect!["9227465"])
}
