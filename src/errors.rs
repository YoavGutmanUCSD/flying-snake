use crate::types::Type;
use std::fmt;
use std::io;
use std::io::Error;

pub enum ParseError {
    BadList,
    InvalidProgram, // BadEverything in boa
    UnexpectedDefine,
    BadLetBinding,
    BadLetExpression,
    BadFnArgs,
    SingleBadFnArg,
    BadFnExpr,
    BadType,
    BadlyTypedFnArg,
}

pub enum CompileError {
    UnboundIdentifier(String),
    DuplicateBinding,
    NoBreakTarget,
    SetUnboundVariable,
    SetKeyword,
    FnDupArg(String, String),
    FnUnbound(String),
    FnBadArgs(String, i32, i32),
    FnDup(String),
    IllegalInput(String),
    Other(String),
}

pub enum RuntimeError {
    TypeError,
    OverflowError,
    CastError,
    BadError,
}

pub enum TypeError {
    TypeMismatch(Type, Type),
    UntypedIdentifier(String),
    UnboundFunctionNoType(String),
    BadCast,
    DoesNotTC, // generic
}

impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            RuntimeError::TypeError => {
                write!(f, "Invalid arguments to one or more functions.")
            }
            RuntimeError::OverflowError => {
                write!(f, "Integer overflow.")
            }
            RuntimeError::CastError => {
                write!(f, "Bad cast.")
            }
            RuntimeError::BadError => {
                write!(f, "Unknown error")
            }
        }
    }
}

impl fmt::Debug for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            CompileError::UnboundIdentifier(s) => {
                write!(f, "Unbound variable identifier ")?;
                f.write_str(s)
            }
            CompileError::DuplicateBinding => write!(f, "Duplicate binding."),
            CompileError::NoBreakTarget => write!(f, "No break target! Break outside of loop."),
            CompileError::SetUnboundVariable => {
                write!(f, "Tried to set! a variable that wasn't defined.")
            }
            CompileError::SetKeyword => write!(f, "Tried to redefine a keyword."),
            CompileError::FnDupArg(fname, id) => {
                write!(f, "Duplicate parameter ")?;
                f.write_str(id)?;
                write!(f, " in function ")?;
                f.write_str(fname)
            }
            CompileError::FnUnbound(fname) => {
                write!(f, "Unbound function: ")?;
                f.write_str(fname)
            }
            CompileError::FnBadArgs(fname, expected, received) => {
                let msg = format!(
                    "Arity mismatch in call to '{}': expected {} got {}",
                    fname, expected, received
                );
                f.write_str(&msg)
            }
            CompileError::FnDup(fname) => {
                let msg = format!("Duplicate function definition: {}", fname);
                f.write_str(&msg)
            }
            CompileError::IllegalInput(fname) => {
                let msg = format!("Illegal usage of input in function '{}'", fname);
                f.write_str(&msg)
            }
            CompileError::Other(s) => f.write_str(s),
        }
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ParseError::BadList => write!(f, "bad list"),
            ParseError::InvalidProgram => write!(f, "invalid program"),
            ParseError::UnexpectedDefine => write!(f, "unexpected define"),
            ParseError::BadLetBinding => write!(f, "at least one bad let binding"),
            ParseError::BadLetExpression => write!(f, "bad let bindings at large"),
            ParseError::BadFnExpr => write!(f, "bad function expr"),
            ParseError::BadFnArgs => write!(f, "bad function args"),
            ParseError::SingleBadFnArg => write!(f, "one bad function arg"),
            ParseError::BadType => write!(f, "bad type"),
            ParseError::BadlyTypedFnArg => {
                write!(f, "type wasn't written correctly for a function argument")
            }
        }
    }
}

impl fmt::Debug for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            TypeError::TypeMismatch(expected, got) => {
                let msg = format!("Expected {}, got {}.", expected, got);
                f.write_str(&msg)
            }
            TypeError::UntypedIdentifier(id) => {
                let msg = format!("No type for identifier {}. Might be unbound.", id,);
                f.write_str(&msg)
            }
            TypeError::UnboundFunctionNoType(f_id) => {
                let msg = format!("No type for function {}. Might be unbound.", f_id,);
                f.write_str(&msg)
            }
            TypeError::BadCast => {
                write!(f, "Bad cast.")
            }
            TypeError::DoesNotTC => write!(f, "Program does not typecheck."),
        }
    }
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
        Error::other(format!("parse error: {:?}", e))
    }
}

impl From<CompileError> for io::Error {
    fn from(e: CompileError) -> io::Error {
        Error::other(format!("Compile error: {:?}", e))
    }
}

impl From<RuntimeError> for io::Error {
    fn from(e: RuntimeError) -> Self {
        Error::other(format!("Runtime error: {:?}", e))
    }
}

impl From<TypeError> for io::Error {
    fn from(e: TypeError) -> Self {
        Error::other(format!("Type error: {:?}", e))
    }
}
