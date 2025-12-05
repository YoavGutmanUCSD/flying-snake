use crate::instr::{Loc, Val};
use crate::types::Type;
use crate::validate::ast::StackVar;
use dynasmrt::x64::Rq;
use im::hashset::HashSet;
use im::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

pub const KEYWORDS: &[&str] = &[
    "let", "if", "break", "loop", "+", "-", "*", "=", "<", ">", "<=", ">=", "set!", "block", "fun",
];

pub type FnDefs = im::HashMap<String, (Vec<(String, Type)>, Type)>;

pub struct LabelNumGenerator {
    count: AtomicUsize,
}

impl LabelNumGenerator {
    pub fn get(&self) -> usize {
        let old_count = self.count.fetch_add(1, Ordering::SeqCst);
        old_count + 1
    }

    pub fn new() -> LabelNumGenerator {
        LabelNumGenerator {
            count: AtomicUsize::new(0),
        }
    }
}

// this struct will replace most arguments to compile_to_instrs
#[derive(Clone)]
pub struct CompilerContext {
    pub value_map: HashMap<String, Val>,
    pub symbol_map: HashMap<StackVar, Val>,
    pub si: i32,
    pub enclosing_loop_label: Option<String>,
    pub shared: Arc<SharedContext>,
}

#[derive(Clone)]
pub struct SharedContext {
    pub label_gen: &'static LabelNumGenerator,
    pub keywords: HashSet<String>,
    pub type_err_label: String,
    pub overflow_err_label: String,
    pub cast_err_label: String,
    pub function_definitions: FnDefs,
}

impl SharedContext {
    pub fn keyword_set() -> HashSet<String> {
        KEYWORDS.iter().map(|kw| kw.to_string()).collect()
    }

    pub fn default(
        label_gen: &'static LabelNumGenerator,
        function_definitions: FnDefs,
    ) -> SharedContext {
        SharedContext {
            label_gen,
            keywords: SharedContext::keyword_set(),
            type_err_label: "type_error".to_string(),
            overflow_err_label: "overflow_error".to_string(),
            cast_err_label: "cast_error".to_string(),
            function_definitions,
        }
    }
}

impl CompilerContext {
    pub fn new(shared_context: Arc<SharedContext>, has_input: bool) -> CompilerContext {
        let value_map: im::HashMap<String, Val> = if has_input {
            im::HashMap::new().update("input".to_string(), Val::Place(Loc::Offset(Rq::RDI, 0)))
        } else {
            im::HashMap::new()
        };
        // let value_map: im::HashMap<String, Val> = if aot {
        //     im::HashMap::new().update("input".to_string(), Val::Place(Loc::Offset(Rq::RDI, 0)))
        // }
        // else if let Some(i) = input {
        //     im::HashMap::new().update("input".to_string(), Val::Imm(i))
        // }
        // else {
        //     im::HashMap::new()
        // };
        CompilerContext {
            si: -2 * 8,
            value_map,
            symbol_map: HashMap::new(),
            enclosing_loop_label: None,
            shared: shared_context,
        }
    }
}
