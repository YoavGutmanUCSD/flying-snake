use std::collections::HashMap;

use crate::{
    context::FnDefs,
    errors::TypeError,
    types::Type,
    validate::ast::{BindingSymbol, StackVar},
};

pub struct StrictifyCtx {
    env: HashMap<StackVar, Type>,
    fn_env: FnDefs,
    loop_break_types: Vec<Vec<Type>>,
    input_type: Option<Type>,
}

impl StrictifyCtx {
    pub fn new(
        env: &im::HashMap<StackVar, Type>,
        fn_env: &FnDefs,
        input_type: Option<Type>,
    ) -> Self {
        Self {
            env: env.iter().map(|(k, v)| (*k, *v)).collect(),
            fn_env: fn_env.clone(),
            loop_break_types: vec![],
            input_type,
        }
    }

    pub(super) fn get_symbol_type(&self, symbol: &BindingSymbol) -> Option<Type> {
        self.env.get(&symbol.id).copied()
    }

    pub(super) fn bind_symbol(&mut self, symbol: &BindingSymbol, type_: Type) {
        self.env.insert(symbol.id, type_);
    }

    pub(super) fn unbind_symbol(&mut self, symbol: &BindingSymbol) {
        self.env.remove(&symbol.id);
    }

    pub(super) fn get_fn(&self, name: &str) -> Option<&(Vec<(String, Type)>, Type)> {
        self.fn_env.get(name)
    }

    pub(super) fn input_type(&self) -> Option<Type> {
        self.input_type
    }

    pub(super) fn with_loop<T, F>(&mut self, f: F) -> (T, Type)
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.loop_break_types.push(vec![]);

        let result = f(self);

        let break_types = self.loop_break_types.pop().unwrap();
        let break_type = break_types
            .into_iter()
            .reduce(|acc, next| acc.join(next))
            .unwrap_or(Type::Nothing);

        (result, break_type)
    }

    pub(super) fn add_break(&mut self, type_: Type) -> Result<(), TypeError> {
        match self.loop_break_types.last_mut() {
            Some(stack) => {
                stack.push(type_);
                Ok(())
            }
            None => Err(TypeError::DoesNotTC),
        }
    }
}
