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
}

runtime_error_tests! {
    test_type_err: {file: "fact_loop", input: "true", expected: "Invalid arguments"},
    test_bad_cast: {file: "typed_fact_tail_rec", input: "true", expected: "Type error", typecheck: true}
}

static_error_tests! {
    test_mutrec_with_typecheck: {file: "mutual_recursion", input: "20", expected: "Type error", typecheck: true},
}


repl_tests! {
    test_add_two_defined: {commands: ["(define a 10)", "(define b 20)", "(+ a b)"], expected: ["30"], typecheck: true},
    test_fun_add_two_defined: {commands: ["(define a 10)", "(define b 20)", "(fun (add (a : Num) (b : Num)) -> Num (+ a b))", "(add a b)"], expected: ["30"], typecheck: true}
}
