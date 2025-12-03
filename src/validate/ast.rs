use crate::ast::{Op1, Op2};
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StackVar(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LoopLabel(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Argument,
    LetBinding,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BindingSymbol {
    pub id: StackVar,
    pub name: Arc<str>,
    pub kind: SymbolKind,
}

impl BindingSymbol {
    pub fn new(id: StackVar, name: impl Into<Arc<str>>, kind: SymbolKind) -> Self {
        Self {
            id,
            name: name.into(),
            kind,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ValidatedBinding {
    pub symbol: BindingSymbol,
    pub value: ValidatedExpr,
}

#[derive(Clone, Debug)]
pub struct ValidatedCall {
    pub name: String,
    pub expected_arity: usize,
    pub args: Vec<ValidatedExpr>,
}

#[derive(Clone, Debug)]
pub enum ValidatedExpr {
    Number(i64),
    Boolean(bool),
    Symbol(BindingSymbol),
    Input,
    Let(Vec<ValidatedBinding>, Box<ValidatedExpr>),
    UnOp(Op1, Box<ValidatedExpr>),
    BinOp(Op2, Box<ValidatedExpr>, Box<ValidatedExpr>),
    If(Box<ValidatedExpr>, Box<ValidatedExpr>, Box<ValidatedExpr>),
    Loop(LoopLabel, Box<ValidatedExpr>),
    Break(LoopLabel, Box<ValidatedExpr>),
    Set(BindingSymbol, Box<ValidatedExpr>),
    Block(Vec<ValidatedExpr>),
    Call(ValidatedCall),
    Print(Box<ValidatedExpr>),
    Cast(Box<ValidatedExpr>, Type),
}
