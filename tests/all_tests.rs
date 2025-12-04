mod infra;

// Your tests go here!
success_tests! {
    test_input: {file: "input", input: "2", expected: "2"},
    test_add: {file: "add", input: "", expected: "3"},
    test_fact_loop: {file: "fact_loop", input: "5", expected: "120"},
    test_mutrec: {file: "mutual_recursion", input: "20", expected: "true"},
    typed_mutrec: {file: "typed_mutual_recursion", input: "20", expected: "true", typecheck: true},
    fact_rec: {file: "fact_tail_rec", input: "5", expected: "120"},
    typed_fact_rec: {file: "typed_fact_tail_rec", input: "5", expected: "120", typecheck: true},
    test_untyped_fun_any: {file: "untyped_fun_any", expected: "1", typecheck: true},
    test_set_subtyping: {file: "set_subtyping", input: "4", expected: "0", typecheck: true},
    typed_set: {file: "typed_set", expected: "12", typecheck: true},
    test_ge_condition: {file: "ge_condition", expected: "0", typecheck: true},
    typed_if_bool_param: {file: "typed_if_bool_param", expected: "1", typecheck: true},
    test_cast_any_bool: {file: "cast_any", expected: "true", typecheck: true},
    test_untyped_fun_casts: {file: "untyped_fun_casts", expected: "25", typecheck: true},
    test_loop_break: {file: "loop_break", expected: "15", typecheck: true},
}

runtime_error_tests! {
    test_type_err: {file: "fact_loop", input: "true", expected: "Invalid arguments"},
    test_bad_cast: {file: "typed_fact_tail_rec", input: "true", expected: "Runtime error", typecheck: true},
    test_cast_nothing: {file: "cast_nothing", expected: "Bad cast", typecheck: true},
}

static_error_tests! {
    test_mutrec_with_typecheck: {file: "mutual_recursion", input: "20", expected: "Type error", typecheck: true},
    test_static_arity: {file: "arity_mismatch", expected: "Type error", typecheck: true},
    test_eq_type_error: {file: "eq_type_error", expected: "Type error", typecheck: true},
    test_break_outside_loop: {file: "break_outside_loop", expected: "No break target", typecheck: true},
    test_if_condition_type_error: {file: "if_condition_type_error", expected: "Type error", typecheck: true},
}

repl_tests! {
    test_add_two_defined: {commands: ["(define a 10)", "(define b 20)", "(+ a b)"], expected: ["30"], typecheck: true},
    test_fun_add_two_defined: {commands: ["(define a 10)", "(define b 20)", "(fun (add (a : Num) (b : Num)) -> Num (+ a b))", "(add a b)"], expected: ["30"], typecheck: true}
}
