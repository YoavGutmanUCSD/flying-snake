use std::cmp::Ordering;

#[derive(Copy, Clone, PartialEq, Eq, Debug, derive_more::Display)]
pub enum Type {
    #[display("Num")]
    Num,
    #[display("Bool")]
    Bool,
    #[display("Any")]
    Any,
    #[display("Nothing")]
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

    pub fn is_subtype_of(self, other: Type) -> bool {
        matches!(
            self.partial_cmp(&other),
            Some(Ordering::Less | Ordering::Equal)
        )
    }
}
