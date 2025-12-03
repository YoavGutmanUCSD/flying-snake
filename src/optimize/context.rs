use std::collections::HashMap;

use crate::{context::FnDefs, errors::TypeError, types::Type};

pub struct StrictifyCtx {
    env: HashMap<String, Type>,
    fn_env: FnDefs,
    loop_break_types: Vec<Vec<Type>>,
}

impl StrictifyCtx {
    pub fn new(env: &im::HashMap<String, Type>, fn_env: &FnDefs) -> Self {
        Self {
            env: env.iter().map(|(k, v)| (k.clone(), *v)).collect(),
            fn_env: fn_env.clone(),
            loop_break_types: vec![],
        }
    }

    pub(super) fn get_id_type(&self, id: &str) -> Option<Type> {
        self.env.get(id).copied()
    }

    /// Returns the existing type before the update, if this `id` corresponds to a type.
    pub(super) fn update_env(&mut self, id: &str, type_: Type) -> Option<Type> {
        self.env.insert(id.to_string(), type_)
    }

    pub(super) fn remove_binding(&mut self, id: &str) {
        self.env.remove(id);
    }

    pub(super) fn get_fn(&self, name: &str) -> Option<&(Vec<(String, Type)>, Type)> {
        self.fn_env.get(name)
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
