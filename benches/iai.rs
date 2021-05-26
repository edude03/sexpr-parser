use iai::black_box;
use sexpr_parser::*;


fn iai_bench() {
    let v = serde_json::from_str(r#"["and", ["!=", ["field", 3], null], ["or", [">", ["field", 4], 25], ["=", ["field", 2], "Jerry"]]]"#).unwrap();
    to_expr(black_box(&v));
}

iai::main!(iai_bench);