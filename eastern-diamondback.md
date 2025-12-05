![egg-eater](./eastern-diamondback.jpg)

# Week 6: Eastern Diamondback

**Due 11:59pm on Monday, November 17.**

In this assignment you'll implement a _static type checker_ for Diamondback
(with a few extensions). The GitHub classroom is at: https://classroom.github.com/a/fFC2jNSB

The test infrastructure is: https://github.com/ucsd-cse131-fa25/cobra-test/tree/main
(it is new, so make sure to look at it again!)

## Language

Eastern Diamondback has two additions atop regular Diamondback. The language is
extended with _annotated functions_ and _casts_:

```
<type> := Num | Bool | Nothing | Any
<defn> := (fun (<name> (<name> : <type>)*) -> <type> <expr>)
       | ... as before ...
<expr> := (cast <type> <expr>)
       | ... as before ...
<prog> := ... as before ...
<repl> := ... as before ...
```

### (Dynamic) Semantics

The dynamic semantics of `(fun (<name> (<name> : <type>)*) <expr>)` are
_exactly_ the same as the dynamic semantics of `(fun (<name> <name>*) <expr>)`.
That is, the dynamic semantics are the same as ignoring the types and compiling
the corresponding un-annotated function.

The dynamic semantics of `(cast <type> <expr>)` are:

- Evaluate `<expr>` to a value `v` then:
  - If `<type>` is `Num` and `v` is a number, evaluate to `v`
  - If `<type>` is `Num` and `v` is a boolean, error with a string containing `"bad cast"`
  - If `<type>` is `Bool` and `v` is a number, error with t a string containing `"bad cast"`
  - If `<type>` is `Bool` and `v` is a boolean, evaluate to `v`
  - If `<type>` is `Nothing`, error with a string containing `"bad cast"`
  - If `<type>` is `Any`, evaluate to `v`

### (Static) Semantics

The static semantics—that is, the potential compiler errors—of Eastern
Diamondback are where most of its interesting behavior is found. This language
has a type-checked mode where many errors that would have been previously
reported dynamically are reported at compile time.

The following are the _type rules_ for the expressions in the language. There
are a few notational conventions:

- `Γ e : T` means “expression `e` has type `T` in environment Γ”
- `Γ e ≤ T` means `Γ e : T'` and `T' ≤ T`
- `T1 ≤ T2` means “T1 is a subtype of T2”
- `≮` means “is not a subtype of”
- `Γ` is a type environment of the shape `[x1 : T1][x2 : T2]...`, and `Γ(x)` means “look up x in Γ”

```
Γ <number> : Num

Γ true : Bool

Γ false : Bool

Γ input : Any

Γ x : T
  when Γ(x) = T
  
Γ (let ((x1 e1) (x2 e2) ...) e) : T
  when Γ e1 : T1, Γ[x1 : T1] e2 : T2, ...
  and Γ[x1 : T1][x2 : T2]... e : T

Γ (add1 e) : Num
  when Γ e ≤ Num

Γ (op e1 e2) : Num
  when Γ e1 ≤ Num and Γ e2 ≤ Num
  and op is +, -, *

Γ (op e1 e2) : Bool
  when Γ e1 ≤ Num and Γ e2 ≤ Num
  and op is <, >, <=, >=

Γ (= e1 e2) : Bool
  when Γ e1 ≤ Num and Γ e2 ≤ Num

Γ (= e1 e2) : Bool
  when Γ e1 ≤ Bool and Γ e2 ≤ Bool

Γ (set! x e) : T
  when e : T
   and T ≤ Γ(x)
   
Γ (if e1 e2 e3) : T1 ∪ T2
  when Γ e2 : T1
   and Γ e3 : T2
   and Γ e1 : Bool
 
Γ (block e1 e2 ... en) : Tn
  when Γ e1 : T1, Γ e2 : T2, ..., Γ en : Tn
 
Γ (loop e) : T1 ∪ T2 ∪ ... ∪ Tn
  when Γ1 e1 : T1, Γ2 e2 : T2, ... Γn en : Tn
   and e1, e2, ... en are (break e) subexpressions of e not nested in another break
   and Γ1, Γ2, ..., Γn are the environments for the corresponding expressions

Γ (break e) : Nothing
  when Γ e : T

Γ (f e1 e2 ...) : Any
  when (fun (f x1 x2 ...) e) is defined (an unannotated function)
   and Γ e1 : T1, Γ e2 : T2, ...

Γ (f e1 e2 ...) : T
  when (fun (f (x1 : T1) (x2 : T2) ...) -> T e) is defined (an annotated function)
   and e1 ≤ T1, e2 ≤ T2, ...

Γ (cast T e) : T
  when Γ e : T'
```

Functions don't have a type calculated for them like expressions, but need to be
checked. For these we just write `ok` if they successfully type-check.

```
(fun (f x1 x2 ...) e) : ok        # case for un-annotated functions
  when [x1 : Any][x2 : Any]... e : Any

(fun (f (x1 : T1) (x2 : T2) ...) -> T e) # case for annotated functions
  when [x1 : T1][x2 : T2]... e : T
```

These rules refer to union `∪` and subtyping `≤`, which are defined as:

```
∀ T . T ∪ Any = Any
∀ T . Any ∪ T = Any
∀ T . T ∪ T = T
∀ T . T ∪ Nothing = T
∀ T . Nothing ∪ T = T
Num ∪ Bool = Any
Bool ∪ Num = Any
```

```
∀ T . T ≤ T = true
∀ T . T ≤ Any = true
∀ T . Nothing ≤ T = true
```

### Implementation

Your task is to add a new _mode_ to your compiler for type checking, following
the typing rules above, and to add syntax and semantics for `cast` expressions
and functions with annotations.

Each of the existing options can have `t` added:

- `-tc <prog>.snek <prog>.s` should run the type checker, report any type errors as compile errors,
  and generate the compiled output if there were no compile errors
- `-tg <prog>.snek <prog>.s <input>` and `-te <prog>.snek <input>` should behave
  like `-e` and `-g`, except they should use the
  _provided value of `input`_ to calculate `input`'s type. That is they should
  use an updated rule for `input` that `Γ input : Num` and/or `Γ input : Bool`
  depending on what was provided
- `-ti` should behave like `-i`, but all REPL entries should be type-checked and
  only evaluated if they have no type errors
- Finally, `-t <prog>.snek` should _just_ typecheck the program and report
  errors (if any), and if the program type-checks successfully it should print
  its type (one of `Num`, `Bool`, `Any`, `Nothing`)

We suggest some error messages above, but for type errors we **only** require
that the printed message say “Type error” somewhere. It can be complicated to
define which error should be reported first in large programs, etc, so we are
defining things such that if a program type-checks, it calculates the type for
main, and if it does not, it has _some_ static error that says “Type error”.

Note that you should _not_ need to change any existing cases in code generation,
just add 2 (straightforward) cases for annotated definitions and for cast. The
vast majority of the work should just be for implementing the type checker.

### Examples

When not in type-checking mode, all programs should behave exactly the same as
they did on Diamondback.

The following examples all asssume _type-checking mode is on_.


- ```
  # No type errors, produces 25:
  (fun (f (x : Num) (y : Num)) -> Num
    (+ (* x x) (* y y)))
  (f 3 4)
  ```
  
  
- ```
  # No type errors, produces 25
  (fun (f x y)
    (let ((x (cast Num x)) (y (cast Num y)))
      (+ (* x x) (* y y))))
  (f 3 4)
  ```
  
- ```
  # Type error because `x` has type `Any` in `(* x x)` which is not allowed
  (fun (f x y)
    (+ (* x x) (* y y)))
  (f 3 4)
  ```
  
- ```
  # Type-checks with type Nothing
  (loop 0)
  ```

- ```
  # Type-checks with type Anything
  (loop (block (break 1) (break true)))
  ```

- ```
  # Type-checks with type Num
  (loop (let (x 100) (if (> (cast Num input) x) (break x) (break 100))))
  ```

- ```
  # Type-checks with type Anything
  (if (isnum input) input false)
  ```

- ```
  # Type error
  (+ input 3)
  ```

- ```
  # Type-checks with type Num 
  (+ (cast Num input) 3)
  ```

### Refactoring and Cleanup

As part of this assignment, you should also do refactorings and cleanups to your
compiler. We want you to make **2** commits that are _solely for improving your
compiler's code quality_ without adding new features. These commits should do
one of:

- Reduce the number of lines of code by removing duplication or simplifying repeated patterns
- Move code into places that you find more logical (could be separate files, or could just be organization in the file)
- Add comments or change formatting of the generated assembly to make it easier to debug
- Add (conditional) print statements or other information-gathering code to your compiler to help with debugging

Make the commits, and then show them with `git show <commit-hash-here>`, and put
that output into files `improvement-commit1.txt` and `improvement-commit2.txt`, e.g.:

```
git show 435ccbcbecfb60339c4cb7011cad2c965325d869 > improvement-commit1.txt
git show 9dec3506e902ca32f80bd536b5cd79679ec729d0 > improvement-commit2.txt
```

Include these files with your submission.