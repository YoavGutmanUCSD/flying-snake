use crate::types::Type;
// Expressions
#[derive(Clone, Copy)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Clone, Copy)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Print(Box<Expr>),
    Cast(Box<Expr>, Type), // TailCall(String, Vec<Expr>) // eventually
}

// pub enum SnekFn {
//     VarArg(String, Vec<String>, Expr)
// }

pub struct SnekFn {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub body: Expr,
    pub fn_type: Type,
}

/* this is left as an example for future reader(s???)
 */

// impl Expr {
//     pub fn foreach(&self, f: fn (&Self)) {
//         let mut curr_expr = vec![self];
//         f(self);
//         while let Some(e) = curr_expr.pop() {
//             match e {
//                 Expr::If(econd, e1, e2) => {
//                     f(econd);
//                     f(e1);
//                     f(e2);
//                 },
//                 Expr::UnOp(_, e) => f(e),
//                 Expr::BinOp(_, e1, e2) => {
//                     f(e1);
//                     f(e2);
//                 }
//                 Expr::Loop(e) => f(e),
//                 Expr::Break(e) => f(e),
//                 Expr::Set(_, e) => f(e),
//                 a => f(a),
//             }
//         }
//     }
// }

// END Expressions
