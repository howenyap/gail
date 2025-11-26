mod common;

use common::run;

#[test]
fn test_fibonacci() {
    let input = r#"
    let fib = fn(n) {
      if (n == 0) {
        return 0;
      } else {
        if (n == 1) {
          return 1;
        } else {
          return fib(n - 1) + fib(n - 2);
        }
      }
    };
    fib(10);
    "#;
    let received = run(input);
    let expected = "55";

    assert_eq!(expected, received);
}

#[test]
fn test_factorial() {
    let input = r#"
    let factorial = fn(n) {
      if (n == 0) {
        return 1;
      } else {
        return n * factorial(n - 1);
      }
    };
    factorial(10);
    "#;
    let received = run(input);
    let expected = "3628800";

    assert_eq!(expected, received);
}
