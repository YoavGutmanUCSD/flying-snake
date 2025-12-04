use std::collections::HashMap;
use std::sync::Arc;

use crate::context::{FnDefs, KEYWORDS};
use crate::errors::CompileError;
use crate::validate::ast::{BindingSymbol, StackVar, SymbolKind};

#[derive(Default)]
pub struct SymbolFactory {
    next: u32,
}

impl SymbolFactory {
    pub fn with_start(next: u32) -> Self {
        Self { next }
    }

    pub fn alloc(&mut self, name: impl Into<Arc<str>>, kind: SymbolKind) -> BindingSymbol {
        let id = StackVar(self.next);
        self.next += 1;
        BindingSymbol::new(id, name, kind)
    }
}

pub struct ValidateCtx<'a> {
    pub(crate) scopes: Vec<HashMap<String, BindingSymbol>>,
    loop_depth: usize,
    symbol_factory: SymbolFactory,
    keywords: std::collections::HashSet<String>,
    fn_defs: &'a FnDefs,
    in_function: Option<&'a str>,
    input_available: bool,
}

impl<'a> ValidateCtx<'a> {
    pub fn new(fn_defs: &'a FnDefs, input_available: bool, in_function: Option<&'a str>) -> Self {
        Self::with_bindings(fn_defs, input_available, in_function, &[], 0)
    }

    pub fn with_bindings(
        fn_defs: &'a FnDefs,
        input_available: bool,
        in_function: Option<&'a str>,
        bindings: &[(String, BindingSymbol)],
        next_symbol_id: u32,
    ) -> Self {
        let scopes = vec![HashMap::new()];
        let mut ctx = Self {
            scopes,
            loop_depth: 0,
            symbol_factory: SymbolFactory::with_start(next_symbol_id),
            keywords: KEYWORDS.iter().map(|kw| kw.to_string()).collect(),
            fn_defs,
            in_function,
            input_available,
        };
        if input_available && in_function.is_none() {
            let input_symbol = ctx.symbol_factory.alloc("input", SymbolKind::Argument);
            ctx.insert_symbol("input", input_symbol);
        }
        for (name, symbol) in bindings.iter() {
            ctx.insert_symbol(name, symbol.clone());
        }
        ctx
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<BindingSymbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }

    pub fn alloc_symbol(
        &mut self,
        name: &str,
        kind: SymbolKind,
    ) -> Result<BindingSymbol, CompileError> {
        if self.keywords.contains(name) {
            return Err(CompileError::SetKeyword);
        }
        Ok(self.symbol_factory.alloc(name, kind))
    }

    pub fn bind_symbol(
        &mut self,
        name: &str,
        kind: SymbolKind,
    ) -> Result<BindingSymbol, CompileError> {
        let symbol = self.alloc_symbol(name, kind)?;
        self.insert_symbol(name, symbol.clone());
        Ok(symbol)
    }

    pub fn insert_symbol(&mut self, name: &str, symbol: BindingSymbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), symbol);
        }
    }

    pub fn push_loop(&mut self) {
        self.loop_depth += 1;
    }

    pub fn pop_loop(&mut self) {
        if self.loop_depth > 0 {
            self.loop_depth -= 1;
        }
    }

    pub fn in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    pub fn functions(&self) -> &'a FnDefs {
        self.fn_defs
    }

    pub fn in_function(&self) -> Option<&str> {
        self.in_function
    }

    pub fn input_available(&self) -> bool {
        self.input_available
    }

    pub fn is_keyword(&self, name: &str) -> bool {
        self.keywords.contains(name)
    }
}
