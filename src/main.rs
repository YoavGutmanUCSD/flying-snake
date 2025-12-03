// You can add as many files as you want

mod ast;
mod compile;
mod context;
mod dynasm;
mod errors;
mod instr;
mod jit;
mod parse;
mod typed_ast;
mod types;

use ast::{Expr, SnekFn};
use compile::compile_to_instrs;
use context::FnDefs;
use context::{CompilerContext, LabelNumGenerator, SharedContext};
use errors::{CompileError, HomogenousBind, ParseError, RuntimeError, TypeError};
use instr::{Instr, Loc, OpCode, Val, CAST_ERROR, OVERFLOW_ERROR, TYPE_ERROR};
use jit::{EaterOfWords, ReturnValue};
use parse::{parse_expr, parse_fn};
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::{Error, ErrorKind};
use types::{leq, tc, Type};

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
                Ok(ExprKind::Define(id.to_string(), parse_expr(&e)?))
            }
            [Sexp::Atom(S(fun)), ..] if fun == "fun" => {
                let snekfn = parse_fn(sexp)?;
                Ok(ExprKind::Function(snekfn))
            }
            _ => Ok(ExprKind::Normal(parse_expr(&sexp)?)),
        },
        e => Ok(ExprKind::Normal(parse_expr(&e)?)),
    }
}

/* This function is for updating the repl state such that all define'd variables
 *  will be either substituted as immediates, OR bound somewhere in [RDI].
 * Not currently used or working, but will be exhumed eventually.
 */
// fn update_repl_state(e: &Expr, seen_bind: &im::hashset::HashSet<String>, mut state: im::HashMap<String, Val>, mut volatile_vars: Vec<i64> ) -> Result<(im::HashMap<String, Val>, Vec<i64>), CompileError> {
//     match e {
//         Expr::Set(name, expr) => {
//             let ind: i64 = volatile_vars.len().try_into().unwrap();
//             match state.get(name) {
//                 Some(Val::Imm(imm)) if !seen_bind.contains(name) => {
//                     volatile_vars.push(*imm as i64);
//                     state = state.update(name.to_string(), Val::Place(Loc::Offset(Rq::RDI, (ind * 8) as i32)));
//                     update_repl_state(expr, seen_bind, state, volatile_vars)
//                 }
//                 _ => {
//                     update_repl_state(expr, seen_bind, state, volatile_vars)
//                 }
//             }
//         },
//         Expr::Let(bindings, expr) => {
//             let mut new_seen_bind = seen_bind.clone();
//             for (name, e) in bindings.iter() {
//                 (state, volatile_vars) = update_repl_state(e, &new_seen_bind, state, volatile_vars)?;
//                 new_seen_bind.insert(name.to_string());
//             }
//             update_repl_state(expr, &new_seen_bind, state, volatile_vars)
//         },
//         Expr::UnOp(_, e) => update_repl_state(e, seen_bind, state, volatile_vars),
//         Expr::BinOp(_, e1, e2) => {
//             (state, volatile_vars) = update_repl_state(e1, seen_bind, state, volatile_vars)?;
//             update_repl_state(e2, seen_bind, state, volatile_vars)
//         },
//         Expr::If(econd, e1, e2) => {
//             (state, volatile_vars) = update_repl_state(econd, seen_bind, state, volatile_vars)?;
//             (state, volatile_vars) = update_repl_state(e1, seen_bind, state, volatile_vars)?;
//             update_repl_state(e2, seen_bind, state, volatile_vars)
//         }
//         Expr::Loop(e) => update_repl_state(e, seen_bind, state, volatile_vars),
//         Expr::Break(e) => update_repl_state(e, seen_bind, state, volatile_vars),
//         Expr::Block(es) => {
//             for e in es.iter() {
//                 (state, volatile_vars) = update_repl_state(e, seen_bind, state, volatile_vars)?;
//             }
//             Ok((state, volatile_vars))
//         }
//         Expr::Call(_, es) => {
//             for e in es.iter() {
//                 (state, volatile_vars) = update_repl_state(e, seen_bind, state, volatile_vars)?;
//             }
//             Ok((state, volatile_vars))
//         }
//         _ => Ok((state, volatile_vars))
//     }
// }

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
        Err(e) => Err(Error::new(ErrorKind::Other, format!("sexp error: {:?}", e))),
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
        None => Err(Error::new(ErrorKind::Other, format!("Dynasm failed."))),
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
    println!("{}", val.to_string());
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

fn compile_fn_maybe(
    fm: SnekFn,
    base_context: CompilerContext,
) -> Result<(String, Vec<Instr>), CompileError> {
    let SnekFn {
        name,
        args,
        body,
        fn_type: _,
    } = fm;

    let mut loc_fn_context = base_context;
    let mut loc_value_map = loc_fn_context.value_map;
    for (arg, _) in args.into_iter() {
        loc_value_map =
            loc_value_map.update(arg, Val::Place(Loc::Offset(Rq::RSP, loc_fn_context.si)));
        loc_fn_context.si -= 8;
    }

    loc_fn_context.value_map = loc_value_map;

    let mut instrs = compile_to_instrs(&body, loc_fn_context, Vec::new())?;
    instrs.push(Instr::Ret);
    Ok((name, instrs))
}

fn create_fn_tc_env(defs: &Vec<(String, Type)>) -> im::HashMap<String, Type> {
    defs.iter().fold(im::HashMap::new(), |acc, (name, t)| {
        acc.update(name.clone(), *t)
    })
}

fn repl_new(mut emitter: EaterOfWords, context: CompilerContext, is_typed: bool) {
    let mut input = String::new();
    let mut define_vals: im::HashMap<String, Val> = im::HashMap::new();

    // define'd variables stored in a vector
    let mut define_vec: Vec<i64> = Vec::new();
    let mut place: i32 = 0; // index of next available slot

    // map: function name -> (list of (arg, arg_type), function type)
    let mut functions: FnDefs = im::HashMap::new();

    // map: define'd variable name -> variable type
    let mut start_env = im::HashMap::new();

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
        let sexp_maybe = parse(&input.trim());
        input.clear();

        // 3. create context
        let local_map = define_vals
            .iter()
            .fold(context.value_map.clone(), |acc, (key, val)| {
                acc.update(key.to_string(), *val)
            });

        let local_context = CompilerContext {
            value_map: local_map,
            ..context.clone()
        };

        // 4. parse sexp into expr
        let code_label = format!("L{}", context.shared.label_gen.get()); // this is a hack...
        match sexp_maybe {
            Ok(sexp) => match parse_repl(&sexp) {
                Ok(ExprKind::Define(id, e)) => {
                    // horrendously inefficient... should be able to avoid 2 clones here.
                    // if let Ok((new_define_vals, new_define_vec)) = update_repl_state(&e, &HashSet::new(), define_vals.clone(), define_vec.clone()) {
                    //     define_vals = new_define_vals;
                    //     define_vec = new_define_vec;
                    // }

                    if is_typed {
                        if let Err(e) =
                            main_tc(&e, &start_env, &local_context.shared.function_definitions)
                        {
                            eprintln!("{}", std::io::Error::from(e).to_string());
                            continue;
                        }
                    }

                    let res = compile_to_instrs(&e, local_context, Vec::new())
                        .map_err(std::io::Error::from) // enter io ecosystem
                        .and_then(|vec| consume_dynasm(&mut emitter, code_label, vec))
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
                            place += 8;
                            define_vec.push(val);
                            if is_typed {
                                start_env = start_env.update(id, t);
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

                        if is_typed {
                            // typecheck environment using function args
                            let args_env = create_fn_tc_env(fn_defs);

                            // union of args_env and start_env
                            let mut fn_env = args_env;
                            for (id, t) in &start_env {
                                fn_env = fn_env.update(id.to_string(), *t);
                            }

                            match main_tc(&f.body, &fn_env, &tentative_functions) {
                                Ok(e_type) => {
                                    if !leq(e_type, fn_type) {
                                        let err = TypeError::TypeMismatch(fn_type, e_type);
                                        eprintln!("{}", std::io::Error::from(err).to_string());
                                        continue;
                                    } else {
                                        // println!("Function will evaluate to: {}", e_type.to_string());
                                    }
                                }
                                Err(e) => {
                                    eprintln!("{}", std::io::Error::from(e).to_string());
                                    continue;
                                }
                            }
                        }

                        // save tentative functions, now that it is good
                        functions = tentative_functions;

                        // create fn context
                        let shared_fn_context = SharedContext {
                            function_definitions: functions.clone(),
                            fn_name: Some(f.name.clone()),
                            ..local_context.shared.clone()
                        };
                        let fn_context = CompilerContext {
                            si: -8,
                            shared: &shared_fn_context,
                            ..local_context.clone()
                        };

                        // compile function and send to emitter
                        let res = compile_fn_maybe(f, fn_context)
                            .map_err(std::io::Error::from) // enter the io ecosystem
                            .and_then(|(name, instrs)| consume_dynasm(&mut emitter, name, instrs));
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
                    if is_typed {
                        if let Err(e) =
                            main_tc(&e, &start_env, &local_context.shared.function_definitions)
                        {
                            eprintln!("{}", std::io::Error::from(e).to_string());
                            continue;
                        }
                    }
                    let base = vec![Instr::TwoArg(
                        OpCode::IMov,
                        Loc::Reg(Rq::RBP),
                        Val::Place(Loc::Reg(Rq::RSP)),
                    )];
                    let res = compile_to_instrs(&e, local_context, base)
                        .map_err(std::io::Error::from) // enter the io ecosystem
                        .bind(|vec| consume_dynasm(&mut emitter, code_label, vec))
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
        .into_iter()
        .map(|(name, body)| create_block(&*name, &body[..]));
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
    for (name, body) in functions.into_iter() {
        emitter.consume_slice(name.clone(), body);
    }
    emitter.discard();

    // 2. consume our_code
    emitter.consume_slice("our_code_starts_here".to_string(), our_code);

    // 3. run
    exert(&mut emitter, input)
}

fn main_tc(e: &Expr, env: &im::HashMap<String, Type>, fn_env: &FnDefs) -> Result<Type, TypeError> {
    let (t, _) = tc(e, env, fn_env)?;
    Ok(t)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if let None = args.get(1) {
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
            return Err(Error::new(ErrorKind::Other, "Invalid mode."));
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
            return Err(Error::new(ErrorKind::Other, "Invalid arguments."));
        }
    };

    let mut emitter: EaterOfWords = EaterOfWords::new();

    let label_gen: LabelNumGenerator = LabelNumGenerator::new();
    let has_input = input != None || mode == Mode::Aot;

    // REPL dispatch
    if let Mode::Repl = mode {
        let shared_context = SharedContext::default(&label_gen, im::HashMap::new(), None);
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
        .and_then(|s| Ok("(".to_string() + &s + ")"))
        .and_then(parse_sexp)
        .and_then(|s| match s {
            Sexp::List(vec) => Ok(vec),
            _ => Err(Error::new(
                ErrorKind::Other,
                "Could not create a proper list of s-expressions.",
            )),
        })?;

    let top_lvl = program.pop().unwrap();

    let snek_fns = program
        .iter()
        .map(|s| parse_fn(s))
        .collect::<Result<Vec<SnekFn>, ParseError>>()?;

    let function_definitions: FnDefs =
        snek_fns
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

    // do typecheck here. short circuit on failure.
    if is_typed {
        // 1. typecheck all functions
        for snekfn in snek_fns.iter() {
            let (defs, _) = &function_definitions[&*snekfn.name];
            let start_env = create_fn_tc_env(defs);
            let fn_tc = main_tc(&snekfn.body, &start_env, &function_definitions)?;
            if !leq(fn_tc, snekfn.fn_type) {
                // using TypeMismatch for when the expression DOES typecheck, but
                //  not to the right type. Should use it for something else later.
                return Err(TypeError::TypeMismatch(snekfn.fn_type, fn_tc).into());
            }
        }
        // 2. typecheck top level (keep type)
        let input_type = match input {
            _ if mode == Mode::Aot || mode == Mode::Typecheck => Type::Any,
            None => Type::Nothing,
            Some(i) if i & 1 == 1 => Type::Bool,
            _ => Type::Num,
        };
        let empty_env = im::HashMap::new().update("input".to_string(), input_type);
        let top_tc = main_tc(&top_lvl_expr, &empty_env, &function_definitions)?;

        // 3. print type if -t
        if mode == Mode::Typecheck {
            println!("Program will evaluate to: {}", top_tc.to_string());
        }
    }

    let shared_context = SharedContext::default(&label_gen, function_definitions, None);
    let top_lvl_context = CompilerContext::new(&shared_context, has_input);

    let fn_context = CompilerContext {
        si: -8, // at 0, we have the return pointer.
        ..top_lvl_context.clone()
    };

    // compile all functions into blocks that end with 'ret'
    let mut fns = Vec::with_capacity(program.len());
    for snekfn in snek_fns.into_iter() {
        let fn_name = Some(snekfn.name.clone());
        let new_shared_context = SharedContext {
            fn_name,
            ..shared_context.clone()
        };
        let new_context = CompilerContext {
            shared: &new_shared_context,
            ..fn_context.clone()
        };
        let fn_res = compile_fn_maybe(snekfn, new_context)?;
        fns.push(fn_res);
    }

    // "freeze" fns here, so future code can't accidentally modify this.
    let fns = fns;

    // compile top level expression
    let first_instr = vec![Instr::TwoArg(
        OpCode::IMov,
        Loc::Reg(Rq::RBP),
        Val::Place(Loc::Reg(Rq::RSP)),
    )];
    let mut our_code_instrs: Vec<Instr> =
        compile_to_instrs(&top_lvl_expr, top_lvl_context, first_instr)?;
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
