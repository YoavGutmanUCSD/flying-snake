# flying-snake

[![linting](https://github.com/YoavGutmanUCSD/flying-snake/actions/workflows/linting.yaml/badge.svg)](https://github.com/YoavGutmanUCSD/flying-snake/actions/workflows/linting.yaml)
[![testing](https://github.com/YoavGutmanUCSD/flying-snake/actions/workflows/testing.yaml/badge.svg)](https://github.com/YoavGutmanUCSD/flying-snake/actions/workflows/testing.yaml)

`flying-snake` is a compiler for the Snek language with both JIT and AOT back ends. The compiler lowers validated ASTs into a small instruction IR (`src/instr.rs`) that is either assembled into native code (`src/jit.rs`) or emitted as NASM.

## Calling Convention

User programs ultimately expose a single entry point named `our_code_starts_here`. The Rust runtime in `runtime/start.rs` calls this symbol using the standard System V AMD64 ABI, passing a pointer in `rdi` to a contiguous `i64` buffer (slot 0 holds the optional CLI input). Everything that follows is a custom convention implemented in `src/compile.rs`.

### Tagged values

- Integers are stored as `(value << 1)` so their tag bit is `0`.
- Booleans use the odd immediates `1` (false) and `3` (true).
- Runtime errors are returned as immediates whose low three bits are `0b111` (see `TYPE_ERROR`, `OVERFLOW_ERROR`, and `CAST_ERROR` in `src/instr.rs`).

### Register contract

- `rax` always holds the result of the most recently evaluated expression and the eventual function return value.
- `rsp` is the real hardware stack pointer, but the compiler also tracks a *virtual* offset (`CompilerContext::si`) that represents the next free 8-byte slot below the current `rsp`.
- `rbp` is set to the entry `rsp` exactly once in `our_code_starts_here`; it never changes afterward so the error stubs can restore the stack with `mov rsp, rbp`.
- `rdi` permanently points at the heap-allocated environment (input plus REPL `define`s). When calling `snek_print` we save/restore `rdi` explicitly because the System V ABI treats it as the first argument register.
- `rcx` is treated as a scratch register for conditional moves when materializing booleans.

### Stack discipline

The compiler never emits a traditional function prologue. Instead it simulates stack growth by writing temporaries to `[rsp + si]` where `si` starts at `-16` for the top level (see `CompilerContext::new`) and `-8` for function bodies (`main.rs`, lines where `fn_context` is created). When a call or host interop requires a real frame, the code synchronizes the hardware stack pointer:

1. Compute `original_context = -context.si` before reserving any new slots.
2. Store the synthetic return address label into `[rsp + si]`, then decrement `si`.
3. Evaluate arguments left-to-right, storing each tagged value into the next slot (decrementing `si` by 8 each time).
4. Issue `sub rsp, original_context` so that `[rsp]` now contains the return address and `[rsp - 8*k]` holds the `k`-th argument.
5. `jmp` to the callee label. The callee finishes by placing its result in `rax` and executing `ret`.
6. Control resumes at the caller’s `_end_fncall_*` label, after which we run `add rsp, original_context - 8` to undo the temporary frame (the `ret` already consumed the 8-byte return slot).

Because callers `jmp` instead of `call`, every activation record has the following layout when the callee starts executing:

```
rsp -> return address written by the caller
rsp-8 -> parameter 0
rsp-16 -> parameter 1
...
```

`compile_validated_fn` relies on this layout and maps each parameter symbol to these fixed offsets. Local bindings, loop temporaries, and intermediate values simply keep lowering `si` so they live further below the current `rsp` without touching the hardware pointer.

### Interacting with the runtime

- **Program entry:** `main.rs` prepends `mov rbp, rsp` to the top-level instruction stream before any user code executes so the error paths know how to unwind.
- **Input:** if the program has an `input`, the compiler seeds the value map with `Val::Place(Loc::Offset(RDI, 0))`, so reads become loads from the environment pointer passed by `runtime/start.rs`.
- **Printing:** the `print` expression saves `rdi` into the next virtual slot, moves the value-to-print into `rdi`, syncs the stack by executing `sub rsp, (-si + 8)` (guaranteeing 16-byte alignment), calls `snek_print` via the `CallPrint` pseudo-instruction, and finally restores both `rdi` and `rsp`.
- **Runtime errors:** the labeled blocks emitted from `TYPE_ERROR`, `OVERFLOW_ERROR`, and `CAST_ERROR` reset `rsp` to `rbp`, load the tagged error code into `rax`, and `ret` back into the Rust host, which then formats the message in `snek_print`.

This arrangement lets the compiler reuse the standard ABI boundary only once (at `our_code_starts_here`), while keeping recursive user calls lightweight and completely under compiler control.
