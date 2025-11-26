mod common;

use common::run;

#[test]
fn test_fibonacci() {
    let input = r#"
    let fib = fn(n) {
      if (n <= 1) {
        n;
      } else {
        fib(n - 1) + fib(n - 2);
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
        1;
      } else {
        n * factorial(n - 1);
      }
    };

    factorial(10);
    "#;
    let received = run(input);
    let expected = "3628800";

    assert_eq!(expected, received);
}

#[test]
fn test_reverse_array() {
    let input = r#"
    let reverse_array = fn(arr) {
      if (len(arr) == 0) {
        arr;
      } else {
        reverse_array(rest(arr)) + slice(arr, 0, 1);
      }
    };

    reverse_array([1, 2, 3, 4, 5]);
    "#;

    let received = run(input);
    let expected = "[5, 4, 3, 2, 1]";
    assert_eq!(expected, received);
}

#[test]
fn test_map_array_double() {
    let input = r#"
    let map = fn(arr, f) {
      let iter = fn(arr, acc) {
        if (len(arr) == 0) {
          acc;
        } else {
          iter(rest(arr), acc + [f(first(arr))]);
        }
      };

      iter(arr, []);
    };

    let double = fn(x) { x * 2 };
    map([1, 2, 3], double);
    "#;

    let received = run(input);
    let expected = "[2, 4, 6]";
    assert_eq!(expected, received);
}

#[test]
fn test_reduce_array_sum() {
    let input = r#"
    let reduce = fn(arr, f, acc) {
      let iter = fn(arr, acc) {
        if (len(arr) == 0) {
          acc;
        } else {
          iter(rest(arr), f(acc, first(arr)));
        }
      };

      iter(arr, acc);
    };

    let add = fn(acc, x) { acc + x };
    reduce([1, 2, 3], add, 0);
  "#;

    let received = run(input);
    let expected = "6";
    assert_eq!(expected, received);
}

#[test]
fn test_reduce_array_product() {
    let input = r#"
    let reduce = fn(arr, f, acc) {
      let iter = fn(arr, acc) {
        if (len(arr) == 0) {
          acc;
        } else {
          iter(rest(arr), f(acc, first(arr)));
        }
      };

      iter(arr, acc);
    };

    let mul = fn(acc, x) { acc * x };
    reduce([1, 2, 3], mul, 1);
  "#;

    let received = run(input);
    let expected = "6";
    assert_eq!(expected, received);
}

#[test]
fn test_merge_sort() {
    let input = r#"
    let merge = fn(left, right) {
      if (len(left) == 0) {
        return right;
      } 

      if (len(right) == 0) {
        return left;
      }

      let l= first(left);
      let r = first(right);

      if (l < r) {
        [l] + merge(rest(left), right);
      } else {
        [r] + merge(left, rest(right));
      }
    };

    let merge_sort = fn(arr) {
      if (len(arr) <= 1) {
        arr;
      } else {
        let n = len(arr);
        let mid = n / 2;
        let left = slice(arr, 0, mid);
        let right = slice(arr, mid, n);

        merge(merge_sort(left), merge_sort(right));
      }
    };

    merge_sort([5, 4, 3, 2, 1]);
  "#;

    let received = run(input);
    let expected = "[1, 2, 3, 4, 5]";
    assert_eq!(expected, received);
}
