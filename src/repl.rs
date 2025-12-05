use crate::ast::Expr;
use crate::compile::compile_validated_expr;
use crate::context::FnDefs;
use crate::context::{CompilerContext, SharedContext};
use crate::errors::HomogenousBind;
use crate::instr::{Instr, Loc, OpCode, Val};
use crate::jit::ReturnValue;
use crate::types::Type;
use crate::validate::ast::{BindingSymbol, StackVar, SymbolKind, ValidatedExpr};
use crate::validate::{validate_expr_with_bindings, ValidationInputs};
use crate::{compile_fun, consume_dynasm, exert_repl, main_tc, parse_repl, snek_format, ExprKind};

use dynasmrt::x64::Rq;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use sexp::*;
use std::ops::ControlFlow;
use std::process::exit;
use std::sync::Arc;

pub fn repl_new(context: CompilerContext, is_typed: bool) {
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

    // repl line
    let mut rl = Editor::<()>::new().unwrap();

    loop {
        // 1. print little > character and read from output
        let input = match rl.readline("> ") {
            Ok(d) => d,
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                exit(0)
            }
            Err(e) => {
                eprintln!("Cannot read input: {e}");
                exit(1)
            }
        };

        // 2. parse input into sexp, then delete old buffer
        let sexp_maybe = parse(input.trim());

        // 3. create context
        let local_context = build_local_context(&context, &define_vals, &symbol_slots);

        // 4. parse sexp into expr
        let code_label = format!("L{}", context.shared.label_gen.get()); // this is a hack...
        match sexp_maybe {
            Ok(sexp) => match parse_repl(&sexp) {
                Ok(ExprKind::Define(id, e)) => {
                    let branch_context = attach_functions(&local_context, &functions);
                    let Some(validated) =
                        validate_repl_expr(&e, &branch_context, &define_symbols, next_symbol_id)
                    else {
                        continue;
                    };

                    if !typecheck_repl_expr(
                        is_typed,
                        &validated,
                        &start_symbol_env,
                        &branch_context.shared.function_definitions,
                    ) {
                        continue;
                    }

                    let res = compile_validated_expr(
                        &validated,
                        branch_context,
                        vec![Instr::TwoArg(
                            OpCode::Mov,
                            Loc::Reg(Rq::RBP),
                            Val::Place(Loc::Reg(Rq::RSP)),
                        )],
                    )
                    .map_err(std::io::Error::from)
                    .and_then(|vec| consume_dynasm(code_label, vec))
                    .and_then(|()| exert_repl(&mut define_vec).map_err(std::io::Error::from));
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
                        match compile_fun(
                            f,
                            &mut functions,
                            &local_context,
                            &start_symbol_env,
                            is_typed,
                        ) {
                            ControlFlow::Break(_) => continue,
                            ControlFlow::Continue(_) => {}
                        }
                    }
                }
                Ok(ExprKind::Normal(e)) => {
                    let exec_context = attach_functions(&local_context, &functions);
                    let Some(validated) =
                        validate_repl_expr(&e, &exec_context, &define_symbols, next_symbol_id)
                    else {
                        continue;
                    };
                    if !typecheck_repl_expr(
                        is_typed,
                        &validated,
                        &start_symbol_env,
                        &exec_context.shared.function_definitions,
                    ) {
                        continue;
                    }
                    let base = vec![Instr::TwoArg(
                        OpCode::Mov,
                        Loc::Reg(Rq::RBP),
                        Val::Place(Loc::Reg(Rq::RSP)),
                    )];
                    let res = compile_validated_expr(&validated, exec_context, base)
                        .map_err(std::io::Error::from) // enter the io ecosystem
                        .bind(|vec| consume_dynasm(code_label, vec))
                        .bind(|()| exert_repl(&mut define_vec))
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
            Err(e) => {
                println!("Error parsing S-Expression: {:?}", e);
                continue;
            }
        };
    }
}

fn build_local_context(
    base: &CompilerContext,
    define_vals: &im::HashMap<String, Val>,
    symbol_slots: &im::HashMap<StackVar, Val>,
) -> CompilerContext {
    let value_map = define_vals
        .iter()
        .fold(base.value_map.clone(), |acc, (key, val)| {
            acc.update(key.to_string(), *val)
        });
    let symbol_map = symbol_slots
        .iter()
        .fold(base.symbol_map.clone(), |acc, (stack, val)| {
            acc.update(*stack, *val)
        });

    CompilerContext {
        value_map,
        symbol_map,
        ..base.clone()
    }
}

fn attach_functions(base: &CompilerContext, functions: &FnDefs) -> CompilerContext {
    let shared_context = SharedContext {
        function_definitions: functions.clone(),
        ..base.shared.as_ref().clone()
    };

    CompilerContext {
        shared: Arc::new(shared_context),
        ..base.clone()
    }
}

fn snapshot_bindings(
    define_symbols: &im::HashMap<String, BindingSymbol>,
) -> Vec<(String, BindingSymbol)> {
    define_symbols
        .iter()
        .map(|(name, symbol)| (name.clone(), symbol.clone()))
        .collect()
}

fn validate_repl_expr(
    expr: &Expr,
    context: &CompilerContext,
    define_symbols: &im::HashMap<String, BindingSymbol>,
    next_symbol_id: u32,
) -> Option<ValidatedExpr> {
    let bindings_snapshot = snapshot_bindings(define_symbols);
    let validation_inputs = ValidationInputs {
        fn_defs: &context.shared.function_definitions,
        allow_input: false,
        in_function: None,
    };

    match validate_expr_with_bindings(expr, validation_inputs, &bindings_snapshot, next_symbol_id) {
        Ok(expr) => Some(expr),
        Err(err) => {
            eprintln!("{}", std::io::Error::from(err));
            None
        }
    }
}

fn typecheck_repl_expr(
    is_typed: bool,
    expr: &ValidatedExpr,
    env: &im::HashMap<StackVar, Type>,
    fn_defs: &FnDefs,
) -> bool {
    if !is_typed {
        return true;
    }

    match main_tc(expr, env, fn_defs, None) {
        Ok(_) => true,
        Err(e) => {
            eprintln!("{}", std::io::Error::from(e));
            false
        }
    }
}
