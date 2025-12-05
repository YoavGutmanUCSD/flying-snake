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
mod types;
mod validate;

use ast::{Expr, SnekFn};
use compile::{compile_validated_expr, compile_validated_fn};
use context::FnDefs;
use context::{CompilerContext, LabelNumGenerator, SharedContext};
use errors::{CompileError, HomogenousBind, ParseError, RuntimeError, TypeError};
use instr::{Instr, Loc, OpCode, Val, CAST_ERROR, OVERFLOW_ERROR, TYPE_ERROR};
use jit::{EaterOfWords, ReturnValue};
use optimize::{optimize, strictify_expr};
use parse::{parse_expr, parse_fn};
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::Error;
use types::Type;
use validate::ast::{BindingSymbol, StackVar, SymbolKind, ValidatedExpr};
use validate::{
    validate_expr, validate_expr_with_bindings, validate_function_body, ValidatedFunction,
    ValidationInputs,
};

use dynasmrt::x64::Rq;

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
fn consume_dynasm(
    emitter: &mut EaterOfWords,
    name: String,
    mut instrs: Vec<Instr>,
) -> Result<&mut EaterOfWords, std::io::Error> {
    instrs.push(Instr::Ret);
    match emitter.consume(name, instrs) {
        Some(()) => Ok(emitter),
        None => Err(Error::other("Dynasm failed.".to_string())),
    }
}

fn exert(emitter: &mut EaterOfWords, input: Option<i64>) -> Result<ReturnValue, RuntimeError> {
    emitter.digest();
    let res = match input {
        None => emitter.exert(None),
        Some(i) => {
            let mut x = i;
            let pi: *mut i64 = &mut x;
            emitter.exert(Some(pi))
        }
    };
    emitter.discard();
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

fn exert_repl(
    emitter: &mut EaterOfWords,
    vals: &mut Vec<i64>,
) -> Result<ReturnValue, RuntimeError> {
    emitter.digest();
    let pi: *mut i64 = vals.as_mut_ptr();
    let res = emitter.exert(Some(pi));
    emitter.discard();
    res
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

fn repl_new(mut emitter: EaterOfWords, context: CompilerContext, is_typed: bool) {
    let mut input = String::new();
    let mut define_vals: im::HashMap<String, Val> = im::HashMap::new();
    let mut define_symbols: im::HashMap<String, BindingSymbol> = im::HashMap::new();
    let mut symbol_slots: im::HashMap<StackVar, Val> = im::HashMap::new();
    let mut next_symbol_id: u32 = 0;

    // define'd variables stored in a vector
    let mut define_vec: Vec<i64> = Vec::new();
    let mut place: i32 = 0; // index of next available slot

    // map: function name -> (list of (arg, arg_type), function type)
    let mut functions: FnDefs = im::HashMap::new();

    // map: define'd variable name -> variable type
    let mut start_symbol_env = im::HashMap::new();

    loop {
        // 0. print little > character
        print!("> ");
        let _ = io::stdout().flush();

        // 1. read from output
        let read_result = io::stdin().read_line(&mut input);
        if let Err(e) = read_result {
            println!("Cannot read input: {:?}. Exiting...", e);
            // continue;
            return;
        }

        // 2. parse input into sexp, then delete old buffer
        let sexp_maybe = parse(input.trim());
        input.clear();

        // 3. create context
        let local_map = define_vals
            .iter()
            .fold(context.value_map.clone(), |acc, (key, val)| {
                acc.update(key.to_string(), *val)
            });

        let local_symbol_map = symbol_slots
            .iter()
            .fold(context.symbol_map.clone(), |acc, (stack, val)| {
                acc.update(*stack, *val)
            });

        let local_context = CompilerContext {
            value_map: local_map,
            symbol_map: local_symbol_map,
            ..context.clone()
        };

        // 4. parse sexp into expr
        let code_label = format!("L{}", context.shared.label_gen.get()); // this is a hack...
        match sexp_maybe {
            Ok(sexp) => match parse_repl(&sexp) {
                Ok(ExprKind::Define(id, e)) => {
                    let shared_context = SharedContext {
                        function_definitions: functions.clone(),
                        ..local_context.shared.clone()
                    };
                    let branch_context = CompilerContext {
                        shared: &shared_context,
                        ..local_context.clone()
                    };
                    let bindings_snapshot: Vec<(String, BindingSymbol)> = define_symbols
                        .iter()
                        .map(|(name, symbol)| (name.clone(), symbol.clone()))
                        .collect();
                    let validation_inputs = ValidationInputs {
                        fn_defs: &branch_context.shared.function_definitions,
                        allow_input: false,
                        in_function: None,
                    };
                    let mut validated = match validate_expr_with_bindings(
                        &e,
                        validation_inputs,
                        &bindings_snapshot,
                        next_symbol_id,
                    ) {
                        Ok(expr) => expr,
                        Err(err) => {
                            eprintln!("{}", std::io::Error::from(err));
                            continue;
                        }
                    };

                    if is_typed {
                        let typed = strictify_expr(
                            validated,
                            &start_symbol_env,
                            &local_context.shared.function_definitions,
                            None,
                        );
                        match typed {
                            Err(e) => {
                                eprintln!("{}", std::io::Error::from(e));
                                continue;
                            }
                            Ok(typed_expr) => {
                                validated = optimize(typed_expr, &local_context.symbol_map).into();
                            }
                        }
                    }

                    let res = compile_validated_expr(&validated, branch_context, Vec::new())
                        .map_err(std::io::Error::from)
                        .and_then(|vec| {
                            println!("let {} =", id);
                            for i in vec.iter() {
                                println!("{}", i.to_string());
                            }
                            consume_dynasm(&mut emitter, code_label, vec)
                        })
                        .bind(|consumer| exert_repl(consumer, &mut define_vec));
                    match res {
                        Err(e) => eprintln!("{}", e),
                        Ok(retval) => {
                            let (val, t): (i64, Type) = match retval {
                                ReturnValue::Num(i) => (i << 1, Type::Num),
                                ReturnValue::Bool(true) => (3, Type::Bool),
                                ReturnValue::Bool(false) => (1, Type::Bool),
                            };
                            define_vals = define_vals
                                .update(id.clone(), Val::Place(Loc::Offset(Rq::RDI, place)));
                            let symbol = BindingSymbol::new(
                                StackVar(next_symbol_id),
                                id.clone(),
                                SymbolKind::LetBinding,
                            );
                            next_symbol_id += 1;
                            define_symbols = define_symbols.update(id.clone(), symbol.clone());
                            symbol_slots = symbol_slots
                                .update(symbol.id, Val::Place(Loc::Offset(Rq::RDI, place)));
                            place += 8;
                            define_vec.push(val);
                            if is_typed {
                                start_symbol_env = start_symbol_env.update(symbol.id, t);
                            }
                        }
                    }
                }
                Ok(ExprKind::Function(f)) => {
                    if functions.contains_key(&*f.name) {
                        eprintln!("ERR: function {} already exists.", f.name);
                    } else {
                        // another disgusting clone
                        let fn_defs = &f.args;
                        let fn_type = f.fn_type;
                        let f_args = f.args.clone();
                        // let f_args: im::HashSet<String> = f.args.iter().fold(im::HashSet::new(),
                        //     |acc, a| acc.update(a.to_string()));
                        let tentative_functions =
                            functions.update(f.name.clone(), (f_args, f.fn_type));

                        let mut validated_fn = match validate_function_body(
                            &f.body,
                            &tentative_functions,
                            &f.args,
                            &f.name,
                        ) {
                            Ok(body) => body,
                            Err(e) => {
                                eprintln!("{}", std::io::Error::from(e));
                                continue;
                            }
                        };

                        if is_typed {
                            // typecheck environment using function args
                            let args_env = create_fn_tc_env(&validated_fn.params, fn_defs);

                            // union of args_env and global bindings
                            let mut fn_env = args_env;
                            for (symbol, t) in start_symbol_env.iter() {
                                fn_env = fn_env.update(*symbol, *t);
                            }
                            let typed = strictify_expr(
                                validated_fn.body,
                                &fn_env,
                                &tentative_functions,
                                None,
                            );
                            validated_fn.body = match typed {
                                Ok(typed_fn) => {
                                    if !typed_fn.type_().is_subtype_of(fn_type) {
                                        let err =
                                            TypeError::TypeMismatch(fn_type, typed_fn.type_());
                                        eprintln!("{}", std::io::Error::from(err));
                                        continue;
                                    }
                                    optimize(typed_fn, &local_context.symbol_map).into()
                                }
                                Err(e) => {
                                    eprintln!("{}", std::io::Error::from(e));
                                    continue;
                                }
                            }
                        }

                        // save tentative functions, now that it is good
                        functions = tentative_functions;

                        // freeze validated_fn so it can't be modified later
                        let validated_fn = validated_fn;

                        // create fn context
                        let shared_fn_context = SharedContext {
                            function_definitions: functions.clone(),
                            ..local_context.shared.clone()
                        };
                        let fn_context = CompilerContext {
                            si: -8,
                            shared: &shared_fn_context,
                            ..local_context.clone()
                        };

                        // compile function and send to emitter
                        let res = compile_validated_fn(f, &validated_fn, fn_context)
                            .map_err(std::io::Error::from) // enter the io ecosystem
                            .and_then(|(name, instrs)| {
                                println!("{}:", name);
                                for i in instrs.iter() {
                                    println!("\t{}", i.to_string())
                                }
                                consume_dynasm(&mut emitter, name, instrs)
                            });
                        if let Err(e) = res {
                            eprintln!("{}", e);
                        } else {
                            emitter.discard();
                        }
                    }
                }
                Ok(ExprKind::Normal(e)) => {
                    let shared_context = SharedContext {
                        function_definitions: functions.clone(),
                        ..local_context.shared.clone()
                    };
                    let local_context = CompilerContext {
                        shared: &shared_context,
                        ..local_context.clone()
                    };
                    let bindings_snapshot: Vec<(String, BindingSymbol)> = define_symbols
                        .iter()
                        .map(|(name, symbol)| (name.clone(), symbol.clone()))
                        .collect();
                    let validation_inputs = ValidationInputs {
                        fn_defs: &local_context.shared.function_definitions,
                        allow_input: false,
                        in_function: None,
                    };
                    let mut validated = match validate_expr_with_bindings(
                        &e,
                        validation_inputs,
                        &bindings_snapshot,
                        next_symbol_id,
                    ) {
                        Ok(expr) => expr,
                        Err(err) => {
                            eprintln!("{}", std::io::Error::from(err));
                            continue;
                        }
                    };
                    if is_typed {
                        let typed = strictify_expr(
                            validated,
                            &start_symbol_env,
                            &local_context.shared.function_definitions,
                            None,
                        );
                        match typed {
                            Err(e) => {
                                eprintln!("{}", std::io::Error::from(e));
                                continue;
                            }
                            Ok(typed_expr) => {
                                validated = optimize(typed_expr, &local_context.symbol_map).into();
                            }
                        }
                    }
                    let base = vec![Instr::TwoArg(
                        OpCode::Mov,
                        Loc::Reg(Rq::RBP),
                        Val::Place(Loc::Reg(Rq::RSP)),
                    )];
                    let res = compile_validated_expr(&validated, local_context, base)
                        .map_err(std::io::Error::from) // enter the io ecosystem
                        .bind(|vec| {
                            for i in vec.iter() {
                                println!("{}", i.to_string());
                            }
                            consume_dynasm(&mut emitter, code_label, vec)
                        })
                        .bind(|consumer| exert_repl(consumer, &mut define_vec))
                        .and_then(snek_format);
                    if let Err(e) = res {
                        eprintln!("{}", e);
                    }
                }
                Err(parse_error) => {
                    eprintln!("parse error: {:?}", parse_error);
                    continue;
                }
            },
            Err(e) if e.message.contains("unexpected eof") => {
                println!("Goodbye!");
                return;
            }
            Err(e) => {
                println!("Error parsing S-Expression: {:?}", e);
                continue;
            }
        };
    }
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
    mut emitter: EaterOfWords,
) -> Result<ReturnValue, RuntimeError> {
    // 0. consume errors
    emitter.consume_slice("type_error".to_string(), &TYPE_ERROR[..]);
    emitter.consume_slice("overflow_error".to_string(), &OVERFLOW_ERROR[..]);
    emitter.consume_slice("cast_error".to_string(), &CAST_ERROR[..]);

    // 1. consume functions
    for (name, body) in functions.iter() {
        emitter.consume_slice(name.clone(), body);
    }
    emitter.discard();

    // 2. consume our_code
    emitter.consume_slice("our_code_starts_here".to_string(), our_code);

    // 3. run
    exert(&mut emitter, input)
}

fn main_tc(
    e: &ValidatedExpr,
    env: &im::HashMap<StackVar, Type>,
    fn_env: &FnDefs,
    input_type: Option<Type>,
) -> Result<Type, TypeError> {
    let typed = strictify_expr(e.clone(), env, fn_env, input_type)?;
    Ok(typed.type_())
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.get(1).is_none() {
        usage_message(&args[0]);
        return Ok(());
    }

    // mode
    let (mode, is_typed): (Mode, bool) = match &*args[1] {
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

    let mut emitter: EaterOfWords = EaterOfWords::new();

    let label_gen: LabelNumGenerator = LabelNumGenerator::new();
    let has_input = input.is_some() || mode == Mode::Aot;

    // REPL dispatch
    if let Mode::Repl = mode {
        let shared_context = SharedContext::default(&label_gen, im::HashMap::new());
        let top_lvl_context = CompilerContext::new(&shared_context, has_input);
        emitter.consume_slice("type_error".to_string(), &TYPE_ERROR[..]);
        emitter.consume_slice("overflow_error".to_string(), &OVERFLOW_ERROR[..]);
        emitter.consume_slice("cast_error".to_string(), &CAST_ERROR[..]);
        emitter.discard(); // badly named -- this just advances the start pointer
        repl_new(emitter, top_lvl_context, is_typed);
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

                emitter.prep_fn(name.clone());

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
        // 0. create some dummy empty hashmaps to pass into optimize step
        let empty_define_env = im::HashMap::new();
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
            let optimized = optimize(typed_body, &empty_define_env);
            validated_fn.body = optimized.into();
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
        validated_top_expr = optimize(typed_top, &empty_define_env).into();

        // 3. print type if -t
        if mode == Mode::Typecheck {
            println!("Program will evaluate to: {}", top_tc);
        }
    }

    let shared_context = SharedContext::default(&label_gen, function_definitions);
    let top_lvl_context = CompilerContext::new(&shared_context, has_input);

    let fn_context = CompilerContext {
        si: -8, // at 0, we have the return pointer.
        ..top_lvl_context.clone()
    };

    // compile all functions into blocks that end with 'ret'
    let mut fns = Vec::with_capacity(validated_fns.len());
    for (snekfn, validated_body) in validated_fns.into_iter() {
        let new_shared_context = SharedContext {
            ..shared_context.clone()
        };
        let new_context = CompilerContext {
            shared: &new_shared_context,
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
        Mode::Jit => snek_format(run_jit(&fns[..], &our_code_instrs[..], input, emitter)?),
        Mode::Aot => {
            let dst_file = out_file.unwrap();
            write_aot(&fns[..], &our_code_instrs[..], dst_file)
        }
        Mode::Both => {
            // do aot
            write_aot(&fns[..], &our_code_instrs[..], out_file.unwrap())?;

            // do jit
            snek_format(run_jit(&fns[..], &our_code_instrs[..], input, emitter)?)
        }
    }
}
