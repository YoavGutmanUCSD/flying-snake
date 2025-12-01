use crate::instr::{Loc, Val};
use crate::types::Type;
use dynasmrt::x64::Rq;
use im::hashset::HashSet;
use im::HashMap;
use std::cell::Cell;

pub type FnDefs = im::HashMap<String, (Vec<(String, Type)>, Type)>;

pub struct LabelNumGenerator {
    count: Cell<i32>,
}

impl LabelNumGenerator {
    pub fn get(&self) -> i32 {
        let temp: i32 = self.count.get();
        self.count.set(temp + 1);
        temp
    }
    pub fn new() -> LabelNumGenerator {
        LabelNumGenerator {
            count: Cell::new(0),
        }
    }
}

// this struct will replace most arguments to compile_to_instrs
#[derive(Clone)]
pub struct CompilerContext<'a> {
    pub value_map: HashMap<String, Val>,
    pub si: i32,
    pub enclosing_loop_label: Option<String>,
    pub shared: &'a SharedContext<'a>,
}

#[derive(Clone)]
pub struct SharedContext<'a> {
    pub label_gen: &'a LabelNumGenerator,
    pub keywords: HashSet<String>,
    pub type_err_label: String,
    pub overflow_err_label: String,
    pub cast_err_label: String,
    pub function_definitions: FnDefs,
    pub fn_name: Option<String>,
}

impl<'a> SharedContext<'a> {
    pub fn default(
        label_gen: &'a LabelNumGenerator,
        function_definitions: FnDefs,
        fn_name: Option<String>,
    ) -> SharedContext<'a> {
        let keywords: HashSet<String> = HashSet::from_iter([
            "let".to_string(),
            "if".to_string(),
            "break".to_string(),
            "loop".to_string(),
            "+".to_string(),
            "-".to_string(),
            "*".to_string(),
            "=".to_string(),
            "<".to_string(),
            ">".to_string(),
            "<=".to_string(),
            ">=".to_string(),
            "set!".to_string(),
            "block".to_string(),
            "fun".to_string(),
        ]);
        SharedContext {
            label_gen,
            keywords,
            type_err_label: "type_error".to_string(),
            overflow_err_label: "overflow_error".to_string(),
            cast_err_label: "cast_error".to_string(),
            function_definitions,
            fn_name,
        }
    }
}

impl<'a> CompilerContext<'a> {
    pub fn new(shared_context: &'a SharedContext, has_input: bool) -> CompilerContext<'a> {
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
            enclosing_loop_label: None,
            shared: shared_context,
        }
    }
}
