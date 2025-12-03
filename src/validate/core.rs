use std::collections::HashSet;

use crate::ast::Expr;
use crate::checks::Check;
use crate::context::FnDefs;
use crate::errors::CompileError;
use crate::validate::ast::{
    BindingSymbol, SymbolKind, ValidatedBinding, ValidatedCall, ValidatedExpr,
};
use crate::validate::context::ValidateCtx;

#[derive(Clone, Debug)]
pub struct ValidatedFunction {
    pub body: ValidatedExpr,
    pub params: Vec<BindingSymbol>,
}

pub struct ValidationInputs<'a> {
    pub fn_defs: &'a FnDefs,
    pub allow_input: bool,
    pub in_function: Option<&'a str>,
}

pub fn validate_expr(
    expr: &Expr,
    inputs: ValidationInputs<'_>,
) -> Result<ValidatedExpr, CompileError> {
    let mut ctx = ValidateCtx::new(inputs.fn_defs, inputs.allow_input, inputs.in_function);
    validate(expr, &mut ctx)
}

pub fn validate_expr_with_bindings(
    expr: &Expr,
    inputs: ValidationInputs<'_>,
    bindings: &[(String, BindingSymbol)],
    next_symbol_id: u32,
) -> Result<ValidatedExpr, CompileError> {
    let mut ctx = ValidateCtx::with_bindings(
        inputs.fn_defs,
        inputs.allow_input,
        inputs.in_function,
        bindings,
        next_symbol_id,
    );
    validate(expr, &mut ctx)
}

pub fn validate_function_body(
    body: &Expr,
    fn_defs: &FnDefs,
    args: &[(String, crate::types::Type)],
    fn_name: &str,
) -> Result<ValidatedFunction, CompileError> {
    let mut ctx = ValidateCtx::new(fn_defs, false, Some(fn_name));
    let mut seen = HashSet::new();
    let mut param_symbols = Vec::with_capacity(args.len());
    for (arg, _) in args.iter() {
        if !seen.insert(arg.clone()) {
            return Err(CompileError::FnDupArg(fn_name.to_string(), arg.clone()));
        }
        let symbol = ctx.bind_symbol(arg, SymbolKind::Argument)?;
        param_symbols.push(symbol);
    }
    let body = validate(body, &mut ctx)?;
    Ok(ValidatedFunction {
        body,
        params: param_symbols,
    })
}

fn validate(expr: &Expr, ctx: &mut ValidateCtx<'_>) -> Result<ValidatedExpr, CompileError> {
    match expr {
        Expr::Number(value) => Ok(ValidatedExpr::Number(*value)),
        Expr::Boolean(value) => Ok(ValidatedExpr::Boolean(*value)),
        Expr::Id(name) => resolve_identifier(name, ctx),
        Expr::Let(bindings, body) => validate_let(bindings, body, ctx),
        Expr::UnOp(op, inner) => {
            let value = validate(inner, ctx)?;
            Ok(ValidatedExpr::UnOp(Check::all_true(), *op, Box::new(value)))
        }
        Expr::BinOp(op, lhs, rhs) => {
            let left = validate(lhs, ctx)?;
            let right = validate(rhs, ctx)?;
            Ok(ValidatedExpr::BinOp(
                Check::all_true(),
                *op,
                Box::new(left),
                Box::new(right),
            ))
        }
        Expr::If(cond, then_e, else_e) => {
            let cond_v = validate(cond, ctx)?;
            let then_v = validate(then_e, ctx)?;
            let else_v = validate(else_e, ctx)?;
            Ok(ValidatedExpr::If(
                Box::new(cond_v),
                Box::new(then_v),
                Box::new(else_v),
            ))
        }
        Expr::Loop(body) => {
            ctx.push_loop();
            let result = validate(body, ctx).map(|body_v| ValidatedExpr::Loop(Box::new(body_v)));
            ctx.pop_loop();
            result
        }
        Expr::Break(value) => {
            if !ctx.in_loop() {
                return Err(CompileError::NoBreakTarget);
            }
            let value_v = validate(value, ctx)?;
            Ok(ValidatedExpr::Break(Box::new(value_v)))
        }
        Expr::Set(name, expr) => {
            if ctx.is_keyword(name) {
                return Err(CompileError::SetKeyword);
            }
            let target = ctx
                .lookup_symbol(name)
                .ok_or(CompileError::SetUnboundVariable)?;
            let value = validate(expr, ctx)?;
            Ok(ValidatedExpr::Set(target, Box::new(value)))
        }
        Expr::Block(exprs) => {
            let mut validated = Vec::with_capacity(exprs.len());
            for expr in exprs.iter() {
                validated.push(validate(expr, ctx)?);
            }
            Ok(ValidatedExpr::Block(validated))
        }
        Expr::Call(name, args) => validate_call(name, args, ctx),
        Expr::Print(expr) => {
            let value = validate(expr, ctx)?;
            Ok(ValidatedExpr::Print(Box::new(value)))
        }
        Expr::Cast(expr, ty) => {
            let value = validate(expr, ctx)?;
            Ok(ValidatedExpr::Cast(Box::new(value), *ty))
        }
    }
}

fn validate_call(
    name: &str,
    args: &[Expr],
    ctx: &mut ValidateCtx<'_>,
) -> Result<ValidatedExpr, CompileError> {
    let (params, _) = ctx
        .functions()
        .get(name)
        .ok_or_else(|| CompileError::FnUnbound(name.to_string()))?;
    let expected = params.len();
    if expected != args.len() {
        return Err(CompileError::FnBadArgs(
            name.to_string(),
            expected as i32,
            args.len() as i32,
        ));
    }

    let mut validated_args = Vec::with_capacity(args.len());
    for arg in args.iter() {
        validated_args.push(validate(arg, ctx)?);
    }
    Ok(ValidatedExpr::Call(ValidatedCall {
        name: name.to_string(),
        args: validated_args,
    }))
}

fn validate_let(
    bindings: &[(String, Expr)],
    body: &Expr,
    ctx: &mut ValidateCtx<'_>,
) -> Result<ValidatedExpr, CompileError> {
    ctx.push_scope();
    let result = (|| {
        let mut seen = HashSet::new();
        let mut validated_bindings = Vec::with_capacity(bindings.len());
        for (name, binding_expr) in bindings.iter() {
            if !seen.insert(name.clone()) {
                return Err(CompileError::DuplicateBinding);
            }
            if ctx.is_keyword(name) {
                return Err(CompileError::SetKeyword);
            }
            let value = validate(binding_expr, ctx)?;
            let symbol = ctx.alloc_symbol(name, SymbolKind::LetBinding)?;
            ctx.insert_symbol(name, symbol.clone());
            validated_bindings.push(ValidatedBinding { symbol, value });
        }
        let body_value = validate(body, ctx)?;
        Ok(ValidatedExpr::Let(validated_bindings, Box::new(body_value)))
    })();
    ctx.pop_scope();
    result
}

fn resolve_identifier(
    name: &str,
    ctx: &mut ValidateCtx<'_>,
) -> Result<ValidatedExpr, CompileError> {
    if name == "input" {
        if let Some(fn_name) = ctx.in_function() {
            return Err(CompileError::IllegalInput(fn_name.to_string()));
        }
        if !ctx.input_available() {
            return Err(CompileError::UnboundIdentifier(name.to_string()));
        }
        return Ok(ValidatedExpr::Input);
    }
    match ctx.lookup_symbol(name) {
        Some(symbol) => Ok(ValidatedExpr::Symbol(symbol)),
        None => Err(CompileError::UnboundIdentifier(name.to_string())),
    }
}
