![diamondback](./diamondback.jpeg)

# Week 5: Diamondback, Due Wednesday, Nov 5

In this assignment you'll implement a compiler for a language called Diamondback,
which has top-level function definitions.

Get the assignment at <https://classroom.github.com/a/FZ2iwp42> This will make
a private-to-you copy of the repository hosted within the course's
organization.  You can also access the public test code
<https://github.com/ucsd-cse131-fa25/cobra-test> if you don't have
or prefer not to use a Github account. This is the same test code for Cobra.

Note: the repository has no real code, just a basic project structure. Feel free to add files and modify them, or even replace them entirely and start from your Cobra code. Make sure your code can do `cargo build` and `cargo test` on ieng6 (Rust version `1.75`).

- **Part 1**: _AOT_ compilation of Diamondback files with a generated assembly file. This is with the `-c` compile flag. The optional argument is only given to the executable `.run` file.
- **Part 2**: _JIT_ compilation of Diamondback files with an evaluation at runtime. This is with the `-e` eval flag with an optional argument.
  - The `-g` flag does both (with an optional argument).
- **Part 3**: A REPL for Diamondback with the `-i` interactive flag.

```
cargo run -- -c tests/test1.snek tests/test1.s
cargo run -- -e tests/test1.snek <optionalArg>
cargo run -- -g tests/test1.snek tests/test1.s <optionalArg>
cargo run -- -i
```

## Part 1 & 2: The Diamondback Language

### Concrete Syntax

The concrete syntax of Diamondback has a significant change from past
languages. It distinguishes _top-level declarations_ from expressions.  The new
parts are function definitions, function calls, and the `print` unary operator.

```
<prog> := <defn>* <expr>                (new!)
<defn> := (fun (<name> <name>*) <expr>) (new!)
<expr> :=
  | <number>
  | true
  | false
  | input
  | <identifier>
  | (let (<binding>+) <expr>)
  | (<op1> <expr>)
  | (<op2> <expr> <expr>)
  | (set! <name> <expr>)
  | (if <expr> <expr> <expr>)
  | (block <expr>+)
  | (loop <expr>)
  | (break <expr>)
  | (<name> <expr>*)                    (new!)

<op1> := add1 | sub1 | isnum | isbool | print (new!)
<op2> := + | - | * | < | > | >= | <= | =

<binding> := (<identifier> <expr>)
```


### Abstract Syntax

You can choose the abstract syntax you use for Diamondback.

### Semantics

A Diamondback program always evaluates to a single integer, a single boolean,
or ends with an error. When ending with an error, it should print a message to
_standard error_ (`eprintln!` in Rust works well for this) and a non-zero exit
code (`std::process::exit(N)` for nonzero `N` in Rust works well for this).

A Diamondback program starts by evaluating the `<expr>` at the end of the
`<prog>`. The new expressions have the following semantics:

- `(<name> <expr>*)` is a _function call_. It first evaluates the expressions
  to values. Then it evaluates the body of the corresponding function
  definition (the one with the given `<name>`) with the values bound to each of
  the parameter names in that definition.
- `(print <expr>)` evaluates the expression to a value and prints it to
  standard output followed by a `\n` character. The `print` expression itself should
  evaluate to the given value.

There are several examples further down to make this concrete.

The _compiler_ should stop and report an error if:

* There is a call to a function name that doesn't exist
* Multiple functions are defined with the same name
* A function's parameter list has a duplicate name, generating an error with string `Duplicate` in the error
* There is a call to a function with the wrong number of arguments
* `input` is used in a function definition (rather than in the expression at
  the end). It's worth thinking of that final expression as the `main` function
  or method

If there are multiple errors, the compiler can report any non-empty subset of
them.

Here are some examples of Diamondback programs.

#### Example 1

```scheme
(fun (fact n)
  (let
    ((i 1) (acc 1))
    (loop
      (if (> i n)
        (break acc)
        (block
          (set! acc (* acc i))
          (set! i (+ i 1))
        )
      )
    )
  )
)
(fact input)
```

#### Example 2

```scheme
(fun (isodd n)
  (if (< n 0)
      (isodd (- 0 n))
      (if (= n 0)
          false
          (iseven (sub1 n))
      )
  )
)

(fun (iseven n)
  (if (= n 0)
      true
      (isodd (sub1 n))
  )
)

(block
  (print input)
  (print (iseven input))
)
```

### Implementing a Compiler for Diamondback

The main new feature in Diamondback is functions. You should choose and
implement a calling convention for these. You're welcome to use the
“standard” x86_64 sysv as a convention, or use some of what we discussed in
class, or choose something else entirely. Remember that when calling _runtime_
functions in Rust, the generated code needs to respect that calling convention.

A compiler for Diamondback does not need guarantee safe-for-space tail calls,
but they are allowed.

Have a plan of a calling convention before you start coding. Do you need to preallocate stack space for local variables? How will you pass arguments? How will you return to your caller?

### Allocate space for local variables in my stack frame?

If your chosen calling convention needs it, you can make a function that precomputes the stack frame size for each function based on the number of local variables. This function can calculate the maximum _depth_ of your stack pointer usage in a body (of a function or of your main `our_code_starts_here`). For example, a `Expr::Number` will have a stack depth of 0, while a `Expr::UnOp(expr)` will have a stack depth of its `expr`.

### How to branch in Dynasm?

There are many ways to branch in dynasm. You can generate a label like this:
```rust
let cool_label = ops.new_dynamic_label();
```
where ops is our dynasm assembler. Then, you can use it to create branches like:
```rust
; =>cool_label
; jmp => cool_label
```

### Calling external functions?
We can call external functions (like if we have a Rust `snek_error`) like:
```
; mov rax, QWORD snek_error as _
; call rax
```
OR
```rust
let snek_error_ptr = runtime::snek_error as *const ();
let snek_error_addr = unsafe { mem::transmute::<* const (), fn() -> i32>(snek_error_ptr) } as i64;
```
And then calling it like:
```
; mov rax, QWORD snek_error_addr
; call rax
```

### Running and Testing

Running and testing are as for Cobra, there is no new infrastructure.

## Reference interpreter

You may check the behavior of programs using this interpreter.

<div id=embed></div>
<script src="diamondback.js" type=text/javascript></script>
<script src="diamondback-embed.js" type=text/javascript></script>


## Part 3: The REPL

### Add Function Definitions to the REPL

Add the ability to define functions to the REPL. Entries should be a definition
(which could be `define` or `fun`) or an expression.

Functions should be able to use global variables `define`d in earlier entries
in the function body. Function definitions should be able to use other
functions defined earlier in the REPL session. 

Make sure to not repeat machine code compiled for earlier functions. This can be done by simply calling the already-compiled functions from new machine code that you generate.

### Defines in the function body
Make sure that `define`d variables in the function body will not be known (or inlined) when compiling the function. This is because we can use this function for later entries which may be using a different `define`d value for the same variable, and your function itself can even modify those variables! Therefore, these functions are not [_pure_](https://en.wikipedia.org/wiki/Pure_function). Could we possibly use a technique we already used for `set!` on a `define`d variable in Cobra?

There are many things to consider here:

1. Our function uses a variable that is `define`d in the REPL body.
2. Our function `set!` a variable that is `define`d in the REPL body.
3. Our function calls another function that may or may not `define`d a variable (or that function keeps calling other functions...)

For simplicity, if we call any function, we do not know if it modifies the `define`d variables in the function call site, therefore, we cannot inline, and we must simply use a pointer to the heap for all `define` d variables. This is similar to how we handled `set!` for `define`d variables. Now also when we call any function in a prompt, we must use the heap. We can think of calling a function as something _unknown_, as in, we do not know what effects it can have on any `define`d variable.
  
### Extension:
If you want to go a more complicated route with more optimization, you can have multiple preprocessing passes that check what global _effects_ our function calls have. If a `define`d variable is not impacted in the call site, we can inline the value! Now we might have multiple helper functions that _preprocess_ our Exprs (for finding if we `set!` a var, if we call a `Expr::Fun`, if we simply use an `Expr::Id`, and so on). It might be nice to use a singular helper function that traverses our expr instead of rewriting the same traversal logic. This helper function can be called by other, more specific helper functions that try to match on the desired `Expr` we are looking for. The function header can look like:
```rust
fn traverse_expr(expr: &Expr, f: &mut dyn FnMut(&Expr))
```
Here we can pass in our `Expr` we want to traverse. We also pass a unique function `f` (which can be anonymous) that can be applied to something within our traverse_expr environment (perhaps called on the `expr` itself...). This function is called a closure, which can capture/close over variables from their environment. Capturing, in this case, means matching to the desired `Expr`!

### Duplicate Label Errors in the REPL

When defining functions in the REPL, you may run into duplicate label errors from dynasm. This is because dynasm labels are global to the entire assembly being generated (if you are reusing `ops`), and if you define a label with the same name twice, such as `label_error`, you might accidentally generate the same label twice. 

You can either append a unique identifier to each function label, or let dynasm handle it with `ops.new_dynamic_label()` (even if the label names are the same, you can initalize a unique DynamicLabel to them). Remember that some labels are referenced outside of your current running prompt, (such as `label_error` or functions), so these DynamicLabels stay the same across prompts. So this means there are two types of labels:
1. Labels that are global and persist across prompts.
2. Labels that are local to the current prompt and can be deleted/recreated/ignored for future prompts.

## Grading

As with the previous assignment, a lot of the credit you get will be based on
us running autograded tests on your submission. You'll be able to see the
result of some of these on while the assignment is out, but we may have more
that we don't show results for until after assignments are all submitted.

We'll combine that with some amount of manual grading involving looking at your
testing and implementation strategy. You should have your own thorough test
suite (it's not unreasonable to write many dozens of tests; you probably don't
need hundreds), and you need to have recognizably implemented a compiler. For
example, you _could_ try to calculate the answer for these programs and
generate a single `mov` instruction: don't do that, it doesn't demonstrate the
learning outcomes we care about.

## Extension: Proper Tail Calls

Implement safe-for-space tail calls for Diamondback. Test with deeply-nested
recursion. To make sure you've tested _proper tail calls_ and not just _tail
recursion_, test deeply-nested mutual recursion between functions with
different numbers of arguments.