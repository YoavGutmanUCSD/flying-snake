![forest-flame](https://upload.wikimedia.org/wikipedia/commons/4/40/Chrysopelea_taprobanica.jpg)

# Week 8-10: Flying Snake

**Due Thursday, December 4, 11:59pm**

This week, you will _take off_ (get it, flying snake) in your own direction
implementing optimizations for Eastern Diamondback (both traditional syntactic
optimizations, and type- and JIT-based ones).

There are some significant format differences from the other assignments:

- You can work alone, or in groups of 2 or 3. You _must_ pre-register your
  groups by Friday, November 21 via [FILL form](https://docs.google.com/forms/d/e/1FAIpQLSdVoOHutmf_3Koe0bFhyZ3P51qIZwzZMOkBTYLX2sHG9dVOIw/viewform?usp=dialog).
- Github Classroom for groups: [classroom](https://classroom.github.com/a/Qkx1hh6U)
- There are no new language features – the specification of the language is
  exactly the same as in Eastern Diamondback, with the exception of a special
  type rule shown below.
- There is no autograder. You will submit a PDF report along with your code.
- There are **5** points available rather than 4, and the points are broken out
  by functionality so you can target specific goals.

## Type Checking and Dead Code

Consider this function:

```
(fun (asnum n m)
  (let ((n (if (isbool n) (if (= n true) 1 -1) n)))
    (* m n)))
```

A few things are true about this function:
- It never results in a runtime error no matter the argument value
- It always returns a number no matter the argument value
- It will not type-check with any possible set of annotations in our type
  system

It's also a nice representation of what “legacy code” might look like in a
language like JavaScript or Python, which often allow this kind of type-based
overloading of functions.

It would be nice if this function could type-check for our JIT. To accommodate
this we will add some type-based optimization, and have four special if rules:

```
Γ (if (isbool v) e2 e3) : T
  when Γ e2 : T
   and Γ v : Bool

Γ (if (isbool v) e2 e3) : T
  when Γ e3 : T
   and Γ v : Num

Γ (if (isnum v) e2 e3) : T
  when Γ e3 : T
   and Γ v : Bool

Γ (if (isnum v) e2 e3) : T
  when Γ e2 : T
   and Γ v : Num

where v is a Value or Id
```

These rules _ignore a branch_ of the if expression if the condition will
definitely evaluate to `true` or `false` based on known type information.

So, if we type-check the above function with `(n : Num)` and `(m : Num)`, we
will not consider the `(if (= n true) 1 -1)` expression in the type-checker.
Similarly, with `(n : Bool)` and `(m : Num)` we will not consider the `n`
sub-expression in the else branch. In both cases the relevant branch
type-checks, and the type of the let-bound `n` is `Num`.


## AOT Type-directed Optimizations (2 points)

Update your compiler to generate more efficient code if the type-checker is
successful on a program. This could mean:

- Skipping tag checks on binary operations
- Simplifying `(isbool e)` and `(isnum e)`  to `true` or `false` if the type of
  `e` is known to be `Num` or `Bool`.
- Skipping the condition and unreachable branch in `(if (isbool e) e1 e2)` and `(if
  (isnum e) e1 e2)` when the type of `e` is known
- Reducing `(cast T e)` to `e` if the type of `e` is `≤ T`
- You're free to add other opportunities you see!

[This latex file](./report-tex/report.tex) shows an example of typesetting optimization that is possible. When the type checker knows both operands are numbers at compile time, the specific runtime safety checks within the function `add` become unnecessary.

In your PDF report, include the following:

1. Write one or more programs that, among them, trigger _all_ the specific
optimizations listed above. For each of the programs, run it with `-g` and `-tg`
and show:
  - That the output of the program is correct and the same
  - That the assembly is different, and shorter, when run with `-t`

2. Consider the program below.

   ```
   (fun (sumrange start stop step)
     (let ((step (if (isbool step) (if (= step true) 1 -1) step))
           (res 0)
           (curr start))
      (loop
        (if (if (> step 0) (>= curr stop) (<= curr stop)) (break res)
            (block
              (set! res (+ res curr))
              (set! curr (+ curr step)))))))
   (block
    (print (sumrange 3 10 1))
    (print (sumrange 10 3 false))
    (print (sumrange 10 3 -1))
    (print (sumrange true false 1)))
   ```
    
   - Run it with the `-g` and `-tg` options. Show in the report the test
     program, the assembly output and answer from the `-g` case, and the type
     error in the `-tg` case.
   - Make it type-check by adding only cast expressions (leave the function
     un-annotated). Show the resulting program and run it with the `-g` and
     `-tg` cases, highlighting specific parts of the program that were
     optimizationed, and where the cast expression appears.


## JIT Optimizations: eval and REPL (-te and -ti) (1 point)

A program like `(+ input 5)` cannot type-check with `-tc` because the type of
`input` is unknown.  However, when running with `-te` (or `-tg`), the type of
`input` _is_ known before compilation; in Eastern Diamondback we saw that we can
type-check the program using these assumptions.

Similarly, at the REPL we can use information about previous `define`d variables
to type-check future REPL entries and optimize them if they are typable.

For this assignment, make sure that your updates work to optimize the code
generated for `input`-using programs and at the REPL.

**For Your Report**:

- Choose one of your example programs from above that uses `input` and show how
the generated code differs with `-tg` from `-g` with these optimizations
enabled.
- Show how your implementation is able to optimize a function that refers to a
variable defined earlier in the REPL session run with `-ti`. You can add code to
print the generated assembly at the REPL; include the REPL trace in your report.

## JIT-based Optimizations: Functions (2 points)

The type-based optimizations are ineffective on un-annotated functions without
casts, especially when we can't infer anything about functions' types from
their call sites, which we may only find out about later.

(_For the computational model you should have in mind, consider a web page,
where dynamically-loaded scripts may calculate the type of their arguments to a
function only in response to user input, or a computational notebook where
types may only be known once a CSV file is read and its columns parsed. The REPL
and `input` are our proxies for the unknowns in these situations._)

Implement a _just-in-time_ compilation for (un-type-checked) functions that
optimizes them when they are _first called_ based on the types of the arguments
in that call. When run in non-type-checking mode:

- Each function should compile to a “stub” (and potentially a “slow” version)
- The stub should call back into the compiler with (a) an id for the funciton
  itself and (b) the given arguments from the first call.
- The compiler should type-check the function based on the types of these
  arguments. If type-checking is successful, it should generate an optimized
  version of the function (as above), and reconfigure the stub to call the
  optimized version. If type-checking is not successful, it should reconfigure
  the stub to call the slow version always.
- Future calls to the function should check the arguments' tags. If there is a
  fast version that matches those tags, it should be called, otherwise the slow
  version is called.

Here is a _recommended_ (but not required!) strategy for this. Focus on `-e`/`-g`:

Write a new Rust function called `compile_me`, and add generated code at the top
of every function to call back into `compile_me` with enough information to
locate that function's AST. In `compile_me` for now, just print out information
about the function, its name and argument names for example, to make sure you
know how to access it. This will require a global functions dictionary, or
putting the program's AST in a global variable in Rust, or some other solution
to access the function information from `compile_me`. Leave the existing
behavior of your function compilation alone for now! Just add this extra call
that prints information and returns back.

Next, update the information you have available for each function to
contain a few (mutable) heap-allocated pieces of configurable metadata. This
should be created when the function is compiled. In class
we talked about two that should be sufficient:

  - one to use as a three-valued flag for “not compiled yet”, “fast version
  available”, and “only slow version available”.
  - one to hold the address of the fast version once generated
Make it so your `compile_me` function can print and update these. You should be
able to do things like print out `"First call to f!"` and `"Called f again!"`
using just this state.

Next, make it so you can pass and access the arguments to the function in
`compile_me`. You may find it useful to just do this for 0-2 arguments first and
worry about the full-arity cases later. Then, add code to `compile_me` to
typecheck the body of the function using the environment generated from the tags
of the arguments. For now, just print the values of the arguments and whether or
not type-checking succeeded in `compile_me` to make sure this is working.

Next, figure out how to make an `Assembler` (what we've often called `ops`)
accessible as a global variable in the Rust file. There are a few approaches for
this; it's a good thing to search around about and ask an LLM to help with. Make
that change, make sure you know how to refer to and change it as a global, and
just commit that. Try to not change your existing function signatures, but still
preserve the ability to access and mutate ops from `compile_me`.

Now you're ready to generate code in the middle of a function call! Update `compile_me` to:

- If type-checking fails, return 0
- If type-checking succeeds:
  - Generate the code for the type-checked version and add it to the global `Assembler`
  - Commit the generated code
  - Save the address of the generated code to the metadata you set up
  - Return the address of the generated code back to the caller

Then in the generated code, add logic to continue with the “slow” version if the
return from `compile_me` is `0`, or jump immediately to the returned value if it
is nonzero (indicating successful type-checking). At this point you should be
able to see the “fast” generated code execute, but `compile_me` will be called
every time.

Finally, add generated code at the top of each function to check the metadata
about the function and take the appropriate action of either doing the first
compile attempt, jumping straight to the slow version, or jumping to the fast
version.

There are a few details up to you here – how to check the tags before or in the
fast function and, if they don't match the fast types, jump back to the slow
version. The representation of the metadata, functions dictionary, and precise
signature of `compile_me` is also up to you. But this scaffolding should give
you ways to incrementally work through the steps needed and check your work
along the way.

When you're done, you should be able to demonstrate (with `-e`), the `sumrange`
program from above, which should:

- Optimize based on all-number arguments (the first call)
- Call the slow version for the call with two numbers and a boolean
- Call the fast version for the second call with three numbers
- Have the correct error on the last call (where a boolean is given for a number)

**In your report**: Show the `sumrange` program running and how the sequence of
calls works by adding appropriate prints of assembly code and output, or showing
steps in a debugger, or otherwise demonstrating that your compiler can run an
optimized version and a slow version of the same function.

**In your report**: Show also any other interesting tests you write along the
way – even if you don't get `sumrange` working perfectly, can you show the
optimization running on a simple function that just adds its arguments? Where
does it break? Show examples of what cases your compiler can and cannot cover.

## Handin

Hand in your project to Gradescope as **two** submissions:

- A PDF of your group's report to `pa7-report`
- Your group's code to `pa7-code`

Happy hacking!