# Introduction

Here's my interpreter based on Thorsten Ball's [Writing an Interpreter in Go](https://interpreterbook.com/), except that it's written in Rust instead. I've done the Go version some time back as well, but found Rust more expressive and fun to work with.

I've had a lot of fun working on this for the past month or so, and plan to continue with [writing the compiler](https://compilerbook.com/) next.

# Quick Start

## Installation

Make sure you have Rust and Cargo installed. Instructions can be found [here](https://www.rust-lang.org/tools/install).

Then, clone the repository as such:

```
git clone https://github.com/howenyap/gail.git
```

## Usage

### Running the REPL

```
cargo run repl
```

### Running a file

```
cargo run <filepath>
```

# The Language

Gail is a dynamically typed language that looks a lot like Python right now. Primitives like integers, booleans, strings, and the null value (bad idea maybe) are supported. Data structures like arrays and hashmaps are available.

The interpreter operates on statements and expressions at it's core, you can differentiate the two as such:

-   a statement does something but does not return a value
-   an expression evaluates to a value

For example, the statement `let x = 1;` assigns the value `1` to the variable `x`, whereas the expression `x` evaluates to the value `1`.

In gail, you can store expressions in arguments, pass them to functions, or store them in another data structure. This allows features like higher-order functions to be possible:

```
fn times(f, acc, n) {
  let iter = fn(f, acc, n) {
    if (n == 0) {
      acc;
    } else {
      iter(f, f(acc), n - 1);
    }
  };

  iter(f, acc, n);
}

let mul_three = fn(x) {
  return x * 3;
};

times(mul_three, 2, 3);
```

Here, we define a function `times` that takes:

-   a function `f`,
-   an accumulator `acc`
-   a count `n`.

It returns the result of applying the function `f` to the accumulator `acc` `n` times. It's considered a higher-order function because it takes a function as an argument.

Then, we define the function `mul_three` which multiplies its argument by 3.

Calling `times(mul_three, 2, 3)` is equivalent to:

```
mul_three(mul_three(mul_three(2)))

// which can be broken down to

let a = mul_three(2);
let b = mul_three(a);
let c = mul_three(b);

// evaluates to 54
c;
```

Since gail also supports recursion, you can even do [merge sort](https://github.com/howenyap/gail/blob/main/tests/integration_tests.rs#L134-L175).

# Syntax

Let's have a look at what you can do with Gail!

## Assignments

```

let x = 1;
let y = true;
let z = "hello";

x; // 1
y; // true
z; // "hello"

```

## Prefix Operators

```

!true; // false
!false; // true

let x = 1;
-x; // -1

```

## Infix Operators

### Arithmetic

```

1 + 1; // 2
1 - 1; // 0
3 \* 2; // 6
10 / 2; // 5

```

### Comparison

```

1 > 0; // true
1 < 0; // false
1 >= 0; // true
1 <= 0; // false

```

### Equality

```

1 == 1; // true
1 != 0; // true
1 == 0; // false
1 != 1; // false

```

## Conditionals

```

if (true) {
  return "true"
}

if (2 > 1) {
  return "bigger"
} else {
  return "smaller"
}

```

## Functions

```

fn add(x, y) {
return x + y;
}

add(1, 2); // 3

```

## Strings

```

"hello" + " " + "world"; // "hello world"
"good " + "mornin" + 3 \* "g" // "good morninggg"

```

## Arrays

```

let array = [1, 2];
array[0]; // 1
array[1]; // 2

[1] + [2]; // [1, 2]
3 \* [1] // [1, 1, 1]

```

## Hashmaps

```

let map = { "chicken": "rice" };
map["chicken"]; // "rice"

```

# Builtin Functions

## len

Returns the length of the string, array, or hashmap.

```

len("hello world"); // 11
len([1, 2, 3]); // 3
len({ "key": "value" }); // 1

```

## push

Appends an element to the end of the array.

```

let array = [];
push(array, 1); // [1]
push(array, 2); // [1, 2]
push(array, 3); // [1, 2, 3]
array; // [1, 2, 3]

```

## first

Returns the first element of the array, errors on empty arrays.

```

first([1, 2, 3]); // 1

```

## last

Returns the last element of the array, errors on empty arrays.

```

last([1, 2, 3]); // 3

```

## rest

Returns the tail of the array, errors on empty arrays.

```

rest([1, 2, 3]); // [2, 3]

```

## slice

Returns a slice of the array.

```

slice([1, 2, 3], 0, 2); // [1, 2]
slice([1, 2, 3], 1, 3); // [2, 3]
slice([1, 2, 3], 0, 3); // [1, 2, 3]

```

## print

```

print("hello world");
// "hello world" gets printed to the console

```
