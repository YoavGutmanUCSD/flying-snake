use std::{cmp::Ordering, fmt::Display};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Num,
    Bool,
    Any,
    Nothing,
}
use Type::*;

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (*self, *other) {
            (Nothing, Nothing) | (Num, Num) | (Bool, Bool) | (Any, Any) => Some(Ordering::Equal),
            (_, Any) => Some(Ordering::Less),
            (Any, _) => Some(Ordering::Greater),
            (_, Nothing) => Some(Ordering::Greater),
            (Nothing, _) => Some(Ordering::Less),
            (Num, Bool) | (Bool, Num) => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Num => "Num",
                Type::Bool => "Bool",
                Type::Any => "Any",
                Type::Nothing => "Nothing",
            }
        )
    }
}

impl Type {
    pub fn join(&self, other: Type) -> Type {
        match (*self, other) {
            (Nothing, a) => a,
            (a, Nothing) => a,
            (Num, Num) => Num,
            (Bool, Bool) => Bool,
            _ => Any,
        }
    }
}

// Historical `tc` checker has been superseded by `optimize::strictify_expr`.
