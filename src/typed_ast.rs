use crate::types::union;
use crate::types::Type;

enum TypedExpr<T> {
    Number(T, i64),
    Boolean(T, bool),
    Id(T, String),
    Let(T, Vec<(String, T, TypedExpr<T>)>, Box<TypedExpr<T>>),
    UnOp(T, Op1, Box<TypedExpr<T>>),
    BinOp(T, Op2, Box<TypedExpr<T>>, Box<TypedExpr<T>>),
    If(T, Box<TypedExpr<T>>, Box<TypedExpr<T>>, Box<TypedExpr<T>>),
    Loop(T, Box<TypedExpr<T>>),
    Break(T, Box<TypedExpr<T>>),
    Set(T, String, Box<TypedExpr<T>>),
    Block(T, Vec<TypedExpr<T>>),
    Call(T, String, Vec<TypedExpr<T>>),
    Print(T, Box<TypedExpr<T>>),
}

fn get_type<T>(e: &TypedExpr<T>) -> &T {
    match e {
        TypedExpr::Number(t,..) => t,
        TypedExpr::Boolean(t,..) => t,
        TypedExpr::Id(t,..) => t,
        TypedExpr::Let(t,..) => t,
        TypedExpr::UnOp(t,..) => t,
        TypedExpr::BinOp(t,..) => t,
        TypedExpr::If(t,..) => t,
        TypedExpr::Loop(t,..) => t,
        TypedExpr::Break(t,..) => t,
        TypedExpr::Set(t,..) => t,
        TypedExpr::Block(t,..) => t,
        TypedExpr::Call(t,..) => t,
        TypedExpr::Print(t,..) => t,
    }
}

/* Not yet! This is for generating optimized code. Not necessary in Eastern Diamondback!
 */
fn strictify(e: &TypedExpr<Type>, env: &HashMap<String, Type>, fn_env: &HashMap<String, Vec<Type>>) -> Option<(TypedExpr<Type>, Type)> {
    match e {
        TypedExpr::Number(_, i) => 
            Some((TypedExpr::Number(Num, *i), Nothing)),
        TypedExpr::Boolean(_, b) => 
            Some((TypedExpr::Boolean(Bool, *b), Nothing)),
        TypedExpr::Id(_, id) => {
            let id_type = *env.get(id)?;
            Some((TypedExpr::Id(id_type, id.to_string()), Nothing))
        }
        TypedExpr::Let(_, bindings, e) => {
            let mut new_env = env.clone();
            let mut new_bindings = Vec::with_capacity(bindings.len());
            let mut binding_breaks = Nothing;
            for (id, id_type, e_id) in bindings.iter() {
                let (real_e_id, real_id_breaks) = strictify(e_id, &new_env, fn_env)?;
                binding_breaks = union(binding_breaks, real_id_breaks)?;
                let real_id_type = get_type(&real_e_id);
                new_bindings.push((id, real_id_type, real_e_id));
            }
            strictify(e, &new_env, fn_env)
        }
        TypedExpr::If(_, econd, e1, e2) => {
            let mut all_breaks = Nothing;
            let (real_econd, econd_breaks) = strictify(econd, env, fn_env)?;
            let (real_e1, e1_breaks) = strictify(e1, env, fn_env)?;
            let (real_e2, e2_breaks) = strictify(e2, env, fn_env)?;
            all_breaks = union(all_breaks, union(econd_breaks, union(e1_breaks, e2_breaks)?)?)?;
            let real_type = union(get_type(&real_e1), get_type(&real_e2))?;
            if get_type(&real_econd) != Bool {
                None
            } else {
                let new = TypedExpr::If(real_type, 
                    Box::new(real_econd),
                    Box::new(real_e1),
                    Box::new(real_e2));
                Some((new, all_breaks))
            }
        }
        // TypedExpr::Loop(_, )
    }
}

impl From<Expr> for TypedExpr<Type> {
    fn from(e: Expr) -> Self { 
        match e {
            Expr::Number(i) => TypedExpr::Number(Num, i),
            Expr::Boolean(b) => TypedExpr::Boolean(Bool, b),
            Expr::Id(i) => TypedExpr::Id(Any, i),
            Expr::Let(bindings, e) => TypedExpr::Let(
                Any
                , bindings.into_iter()
                    .map(|(id, e)| 
                        (id.to_string(), Any, TypedExpr::from(e)))
                    .collect()
                , Box::new(TypedExpr::from(*e))),
            Expr::UnOp(op, e) => TypedExpr::UnOp(Any, op, 
                Box::new(TypedExpr::from(*e))),
            Expr::BinOp(op, e1, e2) => TypedExpr::BinOp(Any, op, 
                Box::new(TypedExpr::from(*e1)), Box::new(TypedExpr::from(*e2))),
            Expr::If(econd, e1, e2) =>
                TypedExpr::If(Any, 
                    Box::new(TypedExpr::from(*econd)),
                    Box::new(TypedExpr::from(*e1)),
                    Box::new(TypedExpr::from(*e2))),
            Expr::Loop(e) => 
                TypedExpr::Loop(Any, Box::new(TypedExpr::from(*e))),
            Expr::Break(e) => 
                TypedExpr::Break(Nothing, Box::new(TypedExpr::from(*e))),
            Expr::Set(id, e) => 
                TypedExpr::Set(Any, id, Box::new(TypedExpr::from(*e))),
            Expr::Block(vec) => 
                TypedExpr::Block(Any
                    , vec.into_iter().map(TypedExpr::from).collect()),
            // TODO: implement function type checking
            Expr::Call(id, e) => todo!(),
            Expr::Print(e) => TypedExpr::Print(Any, Box::new(TypedExpr::from(*e))),
        }
    }
}
