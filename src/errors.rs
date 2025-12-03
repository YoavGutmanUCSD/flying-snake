use crate::types::Type;
use std::io;
use std::io::Error;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("bad list")]
    BadList,
    #[error("invalid program")]
    InvalidProgram, // BadEverything in boa
    #[error("unexpected define")]
    UnexpectedDefine,
    #[error("at least one bad let binding")]
    BadLetBinding,
    #[error("bad let bindings at large")]
    BadLetExpression,
    #[error("bad function args")]
    BadFnArgs,
    #[error("one bad function arg")]
    SingleBadFnArg,
    #[error("bad function expr")]
    BadFnExpr,
    #[error("bad type")]
    BadType,
    #[error("type wasn't written correctly for a function argument")]
    BadlyTypedFnArg,
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Unbound variable identifier {0}")]
    UnboundIdentifier(String),
    #[error("Duplicate binding.")]
    DuplicateBinding,
    #[error("No break target! Break outside of loop.")]
    NoBreakTarget,
    #[error("Tried to set! a variable that wasn't defined.")]
    SetUnboundVariable,
    #[error("Tried to redefine a keyword.")]
    SetKeyword,
    #[error("Duplicate parameter {1} in function {0}")]
    FnDupArg(String, String),
    #[error("Unbound function: {0}")]
    FnUnbound(String),
    #[error("Type error: arity mismatch in call to '{0}': expected {1} got {2}")]
    FnBadArgs(String, i32, i32),
    #[error("Duplicate function definition: {0}")]
    FnDup(String),
    #[error("Illegal usage of input in function '{0}'")]
    IllegalInput(String),
    #[error("{0}")]
    Other(String),
}

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Invalid arguments to one or more functions.")]
    TypeError,
    #[error("Integer overflow.")]
    OverflowError,
    #[error("Bad cast.")]
    CastError,
    #[error("Unknown error")]
    BadError,
}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Expected {0}, got {1}.")]
    TypeMismatch(Type, Type),
    #[error("No type for identifier {0}. Might be unbound.")]
    UntypedIdentifier(String),
    #[error("No type for function {0}. Might be unbound.")]
    UnboundFunctionNoType(String),
    #[error("Bad cast.")]
    BadCast,
    #[error("Program does not typecheck.")]
    DoesNotTC, // generic
}

// this does what i wanted out of and_then!
// thank you ChatGPT for explaining the mechanics of the rust type system to me...
pub trait HomogenousBind<T, E> {
    fn bind<F, U, E2>(self, f: F) -> Result<U, E>
    where
        F: FnOnce(T) -> Result<U, E2>,
        E: From<E2>;
}

impl<T, E> HomogenousBind<T, E> for Result<T, E> {
    fn bind<F, U, E2>(self, f: F) -> Result<U, E>
    where
        F: FnOnce(T) -> Result<U, E2>,
        E: From<E2>,
    {
        Ok(f(self?)?)
    }
}

impl From<ParseError> for io::Error {
    fn from(e: ParseError) -> io::Error {
        Error::other(format!("parse error: {}", e))
    }
}

impl From<CompileError> for io::Error {
    fn from(e: CompileError) -> io::Error {
        Error::other(format!("Compile error: {}", e))
    }
}

impl From<RuntimeError> for io::Error {
    fn from(e: RuntimeError) -> Self {
        Error::other(format!("Runtime error: {}", e))
    }
}

impl From<TypeError> for io::Error {
    fn from(e: TypeError) -> Self {
        Error::other(format!("Type error: {}", e))
    }
}
