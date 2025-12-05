// You can add as many files as you want

mod ast;
mod checks;
mod compile;
mod context;
mod dynasm;
mod errors;
mod instr;
mod jit;
mod optimize;
mod parse;
mod repl;
mod tracing;
mod types;
mod validate;

use ::tracing::debug;
use ast::{Expr, SnekFn};
use compile::{compile_validated_expr, compile_validated_fn};
use context::FnDefs;
use context::{CompilerContext, LabelNumGenerator, SharedContext};
use errors::{CompileError, ParseError, RuntimeError, TypeError};
use instr::{Instr, Loc, OpCode, Val, CAST_ERROR, OVERFLOW_ERROR, TYPE_ERROR};
use jit::{snek_end, EaterOfWords, ReturnValue};
use lazy_static::lazy_static;
use optimize::strictify_expr;
use parse::{parse_expr, parse_fn};
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::Error;
use std::ops::ControlFlow;
use std::sync::{Arc, RwLock};
use types::Type;
use validate::ast::{BindingSymbol, StackVar, ValidatedExpr};
use validate::{validate_expr, validate_function_body, ValidatedFunction, ValidationInputs};

use dynasmrt::x64::Rq;

use crate::repl::repl_new;
use crate::tracing::init_tracing;

lazy_static! {
    static ref label_gen: LabelNumGenerator = LabelNumGenerator::new();
    static ref emitter: RwLock<EaterOfWords> = RwLock::new(EaterOfWords::new());
    static ref stub_space: RwLock<Vec<JitCompilerSlot>> = RwLock::new(vec![]);
}

struct JitCompilerSlot {
    args: Vec<i64>,
    fm: SnekFn,
    validated_fn: ValidatedFunction,
    start_symbol_env: im::HashMap<StackVar, Type>,
    fn_context: CompilerContext,
    tentative_functions: FnDefs,
}

enum ExprKind {
    Normal(Expr),
    Define(String, Expr),
    Function(SnekFn),
}

fn usage_message(filename: &String) {
    println!("Usage: {} [OPTIONS] FILE OPTIONAL_FILE", filename);
    println!("\nOptions:");
    println!("    -c: compile FILE, write results to OPTIONAL_FILE");
    println!("    -e: evaluate code in FILE, write to stdout");
    println!("    -g: evaluate code in FILE, write to stdout. Then, ");
    println!("        compile FILE and write results to OPTIONAL_FILE.");
}

fn parse_repl(s: &Sexp) -> Result<ExprKind, ParseError> {
    match s {
        sexp @ Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op_name)), Sexp::Atom(S(id)), e] if op_name == "define" => {
                Ok(ExprKind::Define(id.to_string(), parse_expr(e)?))
            }
            [Sexp::Atom(S(fun)), ..] if fun == "fun" => {
                let snekfn = parse_fn(sexp)?;
                Ok(ExprKind::Function(snekfn))
            }
            _ => Ok(ExprKind::Normal(parse_expr(sexp)?)),
        },
        e => Ok(ExprKind::Normal(parse_expr(e)?)),
    }
}

fn read_file(file: &mut File) -> Result<String, std::io::Error> {
    let mut in_contents = String::new();
    let _ = file.read_to_string(&mut in_contents);
    Ok(in_contents)
}

// returns std::io::Error because i am not able to make my own From<sexp::Error> for std::io::Error
// this is fine!
fn parse_sexp(s: String) -> Result<Sexp, std::io::Error> {
    match parse(&s) {
        Ok(sexp) => Ok(sexp),
        Err(e) => Err(Error::other(format!("sexp error: {:?}", e))),
    }
}

// returns std::io::Error because i'm not sure what to do with this error yet.
// TODO: better error reporting from JIT failure. future commit.
fn consume_dynasm(name: String, mut instrs: Vec<Instr>) -> Result<(), std::io::Error> {
    instrs.push(Instr::Ret);
    emitter
        .write()
        .unwrap()
        .consume_slice(name, &instrs)
        .ok_or_else(|| Error::other("Dynasm failed.".to_string()))
}

fn exert(input: Option<i64>) -> Result<ReturnValue, RuntimeError> {
    let entry = {
        let mut emitter_guard = emitter.write().unwrap();
        emitter_guard.digest();
        emitter_guard.current_offset()
    };

    let res = match input {
        Some(i) => {
            let mut tagged = i;
            let pi: *mut i64 = &mut tagged;
            snek_end(invoke_entry(entry, Some(pi)))
        }
        None => snek_end(invoke_entry(entry, None)),
    };

    emitter.write().unwrap().discard();
    res
}

fn snek_format(val: ReturnValue) -> Result<(), std::io::Error> {
    println!("{}", val);
    Ok(())
}

fn create_block(name: &str, instrs: &[Instr]) -> String {
    let code: String = instrs
        .iter()
        .map(Instr::to_string)
        .collect::<Vec<String>>()
        .join("\n");
    format!("\n{}:\n{}", name, code)
}

fn exert_repl(vals: &mut Vec<i64>) -> Result<ReturnValue, RuntimeError> {
    let entry = {
        let mut emitter_guard = emitter.write().unwrap();
        emitter_guard.digest();
        emitter_guard.current_offset()
    };

    let res = snek_end(invoke_entry(entry, Some(vals.as_mut_ptr())));
    emitter.write().unwrap().discard();
    res
}

fn invoke_entry(entry: *const u8, arg: Option<*mut i64>) -> i64 {
    unsafe {
        match arg {
            Some(ptr) => {
                let jitted_fn: extern "C" fn(*mut i64) -> i64 = std::mem::transmute(entry);
                jitted_fn(ptr)
            }
            None => {
                let jitted_fn: extern "C" fn() -> i64 = std::mem::transmute(entry);
                jitted_fn()
            }
        }
    }
}

fn create_fn_tc_env(
    params: &[BindingSymbol],
    defs: &[(String, Type)],
) -> im::HashMap<StackVar, Type> {
    params
        .iter()
        .zip(defs.iter())
        .fold(im::HashMap::new(), |acc, (symbol, (_, t))| {
            acc.update(symbol.id, *t)
        })
}

#[derive(Copy, Clone, PartialEq)]
enum Mode {
    Repl,
    Jit,
    Aot,
    Both,
    Typecheck,
}

fn parse_input_opt(input: &String) -> Option<i64> {
    match &**input {
        "true" => Some(3),
        "false" => Some(1),
        e => match e.parse::<i64>() {
            Ok(i) => Some(i << 1),
            _ => None,
        },
    }
}

fn write_aot(
    functions: &[(String, Vec<Instr>)],
    our_code: &[Instr],
    mut file: File,
) -> Result<(), std::io::Error> {
    let overflow_err = create_block("overflow_error", &OVERFLOW_ERROR[..]);
    let type_err = create_block("type_error", &TYPE_ERROR[..]);
    let cast_err = create_block("cast_error", &CAST_ERROR[..]);
    const BASE: &str = "section .text\nextern snek_print\nglobal our_code_starts_here";

    let fn_blocks = functions
        .iter()
        .map(|(name, body)| create_block(name, &body[..]));
    let ourcode_block = create_block("our_code_starts_here", our_code);

    file.write_all(BASE.as_bytes())?;
    file.write_all(overflow_err.as_bytes())?;
    file.write_all(type_err.as_bytes())?;
    file.write_all(cast_err.as_bytes())?;
    fn_blocks
        .into_iter()
        .try_for_each(|e| file.write_all(e.as_bytes()))?;
    file.write_all(ourcode_block.as_bytes())
}

fn run_jit(
    functions: &[(String, Vec<Instr>)],
    our_code: &[Instr],
    input: Option<i64>,
) -> Result<ReturnValue, RuntimeError> {
    let mut emitter_guard = emitter.write().unwrap();

    // 0. consume errors
    emitter_guard.consume_slice("type_error".to_string(), &TYPE_ERROR[..]);
    emitter_guard.consume_slice("overflow_error".to_string(), &OVERFLOW_ERROR[..]);
    emitter_guard.consume_slice("cast_error".to_string(), &CAST_ERROR[..]);

    // 1. consume functions
    for (name, body) in functions.iter() {
        emitter_guard.consume_slice(name.clone(), body);
    }
    emitter_guard.discard();

    // 2. consume our_code
    emitter_guard.consume_slice("our_code_starts_here".to_string(), our_code);

    // 3. run
    exert(input)
}

pub fn main_tc(
    e: &ValidatedExpr,
    env: &im::HashMap<StackVar, Type>,
    fn_env: &FnDefs,
    input_type: Option<Type>,
) -> Result<Type, TypeError> {
    let typed = strictify_expr(e.clone(), env, fn_env, input_type)?;
    Ok(typed.type_())
}

fn main() -> std::io::Result<()> {
    init_tracing();

    let args: Vec<String> = env::args().collect();
    if args.get(1).is_none() {
        usage_message(&args[0]);
        return Ok(());
    }

    debug!("Executing in mode: {}", args[1]);

    // mode
    let (mode, is_typed): (Mode, bool) = match args[1].as_ref() {
        "-i" => (Mode::Repl, false),
        "-e" => (Mode::Jit, false),
        "-c" => (Mode::Aot, false),
        "-g" => (Mode::Both, false),
        "-ti" => (Mode::Repl, true),
        "-te" => (Mode::Jit, true),
        "-tc" => (Mode::Aot, true),
        "-tg" => (Mode::Both, true),
        "-t" => (Mode::Typecheck, true),
        _ => {
            usage_message(&args[1]);
            return Err(Error::other("Invalid mode."));
        }
    };

    // input parsing
    let input: Option<i64> = match mode {
        Mode::Repl => None,
        Mode::Jit => args.get(3),
        Mode::Aot => args.get(4),
        Mode::Both => args.get(4),
        Mode::Typecheck => None,
    }
    .and_then(parse_input_opt);

    // files to write to (or not!)
    let (in_file, out_file) = match (mode, args.get(2), args.get(3)) {
        (Mode::Aot, Some(f1), Some(f2)) => (Some(File::open(f1)?), Some(File::create(f2)?)),
        (Mode::Jit, Some(f1), _) => (Some(File::open(f1)?), None),
        (Mode::Both, Some(f1), Some(f2)) => (Some(File::open(f1)?), Some(File::create(f2)?)),
        (Mode::Repl, None, None) => (None, None),
        (Mode::Typecheck, Some(f1), None) => (Some(File::open(f1)?), None),
        _ => {
            usage_message(&args[1]);
            return Err(Error::other("Invalid arguments."));
        }
    };

    let has_input = input.is_some() || mode == Mode::Aot;

    // REPL dispatch
    if let Mode::Repl = mode {
        let mut emitter_guard = emitter.write().unwrap();

        let shared_context = SharedContext::default(&label_gen, im::HashMap::new());
        let top_lvl_context = CompilerContext::new(Arc::new(shared_context), has_input);
        emitter_guard.consume_slice("type_error".to_string(), &TYPE_ERROR[..]);
        emitter_guard.consume_slice("overflow_error".to_string(), &OVERFLOW_ERROR[..]);
        emitter_guard.consume_slice("cast_error".to_string(), &CAST_ERROR[..]);
        emitter_guard.discard(); // badly named -- this just advances the start pointer
        drop(emitter_guard);

        repl_new(top_lvl_context, is_typed);

        return Ok(());
    }

    /*this code is shared for all but REPL. might as well not duplicate it.
     */

    let mut program = read_file(&mut in_file.unwrap())
        .map(|s| "(".to_string() + &s + ")")
        .and_then(parse_sexp)
        .and_then(|s| match s {
            Sexp::List(vec) => Ok(vec),
            _ => Err(Error::other(
                "Could not create a proper list of s-expressions.",
            )),
        })?;

    let top_lvl = program.pop().unwrap();

    let parsed_fns = program
        .iter()
        .map(parse_fn)
        .collect::<Result<Vec<SnekFn>, ParseError>>()?;

    let function_definitions: FnDefs =
        parsed_fns
            .iter()
            .try_fold(im::HashMap::new(), |acc, snekfn| {
                let SnekFn {
                    name,
                    args,
                    body: _,
                    fn_type: t,
                } = snekfn;
                if acc.contains_key(name) {
                    return Err(CompileError::FnDup(name.clone()));
                }

                // TODO: move this check somewhere else.
                args.iter().try_fold(im::HashSet::new(), |acc, (a, _)| {
                    if acc.contains(a) {
                        Err(CompileError::FnDupArg(name.to_string(), a.to_string()))
                    } else {
                        Ok(acc.update(a.to_string()))
                    }
                })?;

                emitter.write().unwrap().prep_fn(name.clone());

                Ok(acc.update(name.to_string(), (args.clone(), *t)))
            })?;

    let top_lvl_expr = parse_expr(&top_lvl)?;

    let mut validated_fns: Vec<(SnekFn, ValidatedFunction)> = Vec::with_capacity(parsed_fns.len());
    for snekfn in parsed_fns.into_iter() {
        let validated_body = validate_function_body(
            &snekfn.body,
            &function_definitions,
            &snekfn.args,
            &snekfn.name,
        )?;
        validated_fns.push((snekfn, validated_body));
    }

    let allow_input_for_validation = has_input || matches!(mode, Mode::Typecheck);
    let mut validated_top_expr = validate_expr(
        &top_lvl_expr,
        ValidationInputs {
            fn_defs: &function_definitions,
            allow_input: allow_input_for_validation,
            in_function: None,
        },
    )?;

    // do typecheck here. short circuit on failure.
    if is_typed {
        // 1. typecheck all functions and replace their validated bodies with strictified output
        for (snekfn, validated_fn) in validated_fns.iter_mut() {
            let (defs, _) = &function_definitions[&*snekfn.name];
            let fn_env = create_fn_tc_env(&validated_fn.params, defs);
            let typed_body = strictify_expr(
                validated_fn.body.clone(),
                &fn_env,
                &function_definitions,
                None,
            )?;
            if !typed_body.type_().is_subtype_of(snekfn.fn_type) {
                return Err(TypeError::TypeMismatch(snekfn.fn_type, typed_body.type_()).into());
            }
            validated_fn.body = typed_body.into();
        }
        // 2. typecheck top level (keep type) and use strictified output for compilation
        let input_type = match input {
            _ if mode == Mode::Aot || mode == Mode::Typecheck => Type::Any,
            None => Type::Nothing,
            Some(i) if i & 1 == 1 => Type::Bool,
            _ => Type::Num,
        };
        let empty_env: im::HashMap<StackVar, Type> = im::HashMap::new();
        let typed_top = strictify_expr(
            validated_top_expr.clone(),
            &empty_env,
            &function_definitions,
            Some(input_type),
        )?;
        let top_tc = typed_top.type_();
        validated_top_expr = typed_top.into();

        // 3. print type if -t
        if mode == Mode::Typecheck {
            println!("Program will evaluate to: {}", top_tc);
        }
    }

    let shared_context = Arc::new(SharedContext::default(&label_gen, function_definitions));
    let top_lvl_context = CompilerContext::new(shared_context.clone(), has_input);

    let fn_context = CompilerContext {
        si: -8, // at 0, we have the return pointer.
        ..top_lvl_context.clone()
    };

    // compile all functions into blocks that end with 'ret'
    let mut fns = Vec::with_capacity(validated_fns.len());
    for (snekfn, validated_body) in validated_fns.into_iter() {
        let new_context = CompilerContext {
            shared: shared_context.clone(),
            ..fn_context.clone()
        };
        let fn_res = compile_validated_fn(snekfn, &validated_body, new_context)?;
        fns.push(fn_res);
    }

    // "freeze" fns here, so future code can't accidentally modify this.
    let fns = fns;

    // compile top level expression
    let first_instr = vec![Instr::TwoArg(
        OpCode::Mov,
        Loc::Reg(Rq::RBP),
        Val::Place(Loc::Reg(Rq::RSP)),
    )];
    let mut our_code_instrs: Vec<Instr> =
        compile_validated_expr(&validated_top_expr, top_lvl_context, first_instr)?;
    our_code_instrs.push(Instr::Ret);

    // once again "freeze" our_code_instrs so i can't accidentally mutate it later
    let our_code_instrs = our_code_instrs;

    // REST dispatch, using files
    match mode {
        Mode::Typecheck => Ok(()), // do nothing
        Mode::Repl => {
            panic!("this shouldn't happen")
        }
        Mode::Jit => snek_format(run_jit(&fns[..], &our_code_instrs[..], input)?),
        Mode::Aot => {
            let dst_file = out_file.unwrap();
            write_aot(&fns[..], &our_code_instrs[..], dst_file)
        }
        Mode::Both => {
            // do aot
            write_aot(&fns[..], &our_code_instrs[..], out_file.unwrap())?;

            // do jit
            snek_format(run_jit(&fns[..], &our_code_instrs[..], input)?)
        }
    }
}

#[warn(unused_results)]
fn compile_fun(
    f: SnekFn,
    functions: &mut FnDefs,
    local_context: &CompilerContext,
    start_symbol_env: &im::HashMap<StackVar, Type>,
    is_typed: bool,
) -> ControlFlow<()> {
    // another disgusting clone
    let fn_defs = &f.args;
    let fn_type = f.fn_type;
    let f_args = f.args.clone();
    // let f_args: im::HashSet<String> = f.args.iter().fold(im::HashSet::new(),
    //     |acc, a| acc.update(a.to_string()));
    let tentative_functions = functions.update(f.name.clone(), (f_args, f.fn_type));

    let validated_fn = match validate_function_body(&f.body, &tentative_functions, &f.args, &f.name)
    {
        Ok(body) => body,
        Err(e) => {
            eprintln!("{}", std::io::Error::from(e));
            return ControlFlow::Continue(());
        }
    };

    // create fn context
    let shared_fn_context = SharedContext {
        function_definitions: functions.clone(),
        ..local_context.shared.as_ref().clone()
    };
    let fn_context = CompilerContext {
        si: -8,
        shared: Arc::new(shared_fn_context),
        ..local_context.clone()
    };

    if is_typed {
        // typecheck environment using function args
        let args_env = create_fn_tc_env(&validated_fn.params, fn_defs);

        // union of args_env and global bindings
        let mut fn_env = args_env;
        for (symbol, t) in start_symbol_env.iter() {
            fn_env = fn_env.update(*symbol, *t);
        }

        match main_tc(&validated_fn.body, &fn_env, &tentative_functions, None) {
            Ok(e_type) => {
                if !e_type.is_subtype_of(fn_type) {
                    let err = TypeError::TypeMismatch(fn_type, e_type);
                    eprintln!("{}", std::io::Error::from(err));
                    return ControlFlow::Break(());
                } else {
                    // println!("Function will evaluate to: {}", e_type.to_string());
                }
            }
            Err(e) => {
                eprintln!("{}", std::io::Error::from(e));
                return ControlFlow::Break(());
            }
        }

        // save tentative functions, now that it is good
        *functions = tentative_functions.as_ref().clone();

        // compile function and send to emitter
        let res = compile_validated_fn(f, &validated_fn, fn_context)
            .map_err(std::io::Error::from) // enter the io ecosystem
            .and_then(|(name, instrs)| consume_dynasm(name, instrs));
        if let Err(e) = res {
            eprintln!("{}", e);
            ControlFlow::Break(())
        } else {
            emitter.write().unwrap().discard();
            ControlFlow::Continue(())
        }
    } else {
        // No type checking: compile stub

        let mut stub_space_guard = stub_space.write().unwrap();
        let args = vec![0i64; f.args.len()];

        let stub_idx = stub_space_guard.len() as i64;
        stub_space_guard.push(JitCompilerSlot {
            args,
            fm: f.clone(),
            validated_fn: validated_fn.clone(),
            start_symbol_env: start_symbol_env.clone(),
            fn_context: fn_context.clone(),
            tentative_functions: tentative_functions.clone(),
        });

        let args = &mut stub_space_guard.last_mut().unwrap().args;

        let _ = consume_dynasm(f.name.clone(), {
            let mut instrs = vec![];

            // move dynamic arguments to the stub space, which will become inputs to `dispatcher_stub()`
            for i in 0..args.len() {
                // the pointer to the arg storage
                let arg_ptr = args.as_ptr() as usize + 8 * i;
                let dyn_arg_loc = Loc::Offset(Rq::RSP, -8 - 8 * i as i32);
                instrs.push(Instr::MovRegImm64(Rq::RCX, arg_ptr as i64));
                instrs.push(Instr::TwoArg(
                    OpCode::Mov,
                    Loc::Reg(Rq::RAX),
                    Val::Place(dyn_arg_loc),
                ));
                instrs.push(Instr::TwoArg(
                    OpCode::Mov,
                    Loc::Offset(Rq::RCX, 0),
                    Val::Place(Loc::Reg(Rq::RAX)),
                ));
            }

            // stash rdi to [rsp - 8] (arg0), now that we've copied that over
            instrs.push(Instr::TwoArg(
                OpCode::Mov,
                Loc::Offset(Rq::RSP, -8),
                Val::Place(Loc::Reg(Rq::RDI)),
            ));

            // prepare to call `dispatcher_stub()`
            {
                // arg 0 (rdi): stub_index
                instrs.push(Instr::TwoArg(
                    OpCode::Mov,
                    Loc::Reg(Rq::RDI),
                    Val::Imm(stub_idx),
                ));

                instrs.push(Instr::MovRegImm64(Rq::RAX, dispatcher_stub as usize as i64));
                instrs.push(Instr::TwoArg(OpCode::Sub, Loc::Reg(Rq::RSP), Val::Imm(8)));
                instrs.push(Instr::CallRax);
                instrs.push(Instr::TwoArg(OpCode::Add, Loc::Reg(Rq::RSP), Val::Imm(8)));
            }

            instrs
        });

        // save tentative functions, now that it is good
        *functions = tentative_functions.as_ref().clone();

        ControlFlow::Continue(())
    }
}

extern "C" fn dispatcher_stub(i: usize) -> *const u8 {
    debug!("stub {i}");

    let JitCompilerSlot {
        args,
        fm,
        validated_fn,
        start_symbol_env,
        fn_context,
        tentative_functions,
    } = &mut stub_space.write().unwrap()[i];

    debug!("invoked with args: {args:#?}");

    let arg_types = {
        let mut types = Vec::with_capacity(args.len());
        for i in args {
            let i = *i;
            let type_ = match i & 7 {
                0b111 => {
                    panic!("Error value encountered in function call")
                }
                0b011 | 0b001 => Type::Bool,
                _ if i & 1 == 0 => Type::Num,
                _ => panic!("Error value encountered in function call"),
            };
            types.push(type_);
        }
        types
    };

    let fn_defs = fm
        .args
        .iter()
        .zip(arg_types)
        .map(|((fun_name, _), type_)| (fun_name.clone(), type_))
        .collect::<Vec<_>>();

    // typecheck environment using function args
    let args_env = create_fn_tc_env(&validated_fn.params, &fn_defs);

    // union of args_env and global bindings
    let mut fn_env = args_env;
    for (symbol, t) in start_symbol_env.iter() {
        fn_env = fn_env.update(*symbol, *t);
    }

    let texpr = strictify_expr(
        validated_fn.body.clone(),
        &fn_env,
        tentative_functions,
        None,
    )
    .unwrap_or_else(|error| panic!("Encountered type error {error} during JIT compilation"));

    let new_fm = SnekFn {
        args: fn_defs,
        ..fm.clone()
    };
    let fast_fn = ValidatedFunction {
        body: texpr.into(),
        params: validated_fn.params.clone(),
    };

    let fast_fn_offset = emitter.read().unwrap().current_offset();

    compile_validated_fn(new_fm, &fast_fn, fn_context.clone())
        .map_err(std::io::Error::from)
        .and_then(|(name, instrs)| consume_dynasm(name, instrs))
        .unwrap();

    emitter.write().unwrap().discard();

    fast_fn_offset
}
