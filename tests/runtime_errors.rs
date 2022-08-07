mod utils;

use expect_test::expect;

#[test]
fn stack_overflow() {
    utils::check("\
func __entry
  r0 <- call __entry
end", expect!["error: stack overflow

"]);
}

#[test]
fn out_of_bounds() {
    utils::check("\
func __entry
  r0 <- arr 8
  r1 <- get r0 8
end",
          expect!["error: index out of bounds: index was 8 but array at 0x1 has length 8

"])
}
