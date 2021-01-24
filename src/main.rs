use serde_json::Value;
use std::convert::TryFrom;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Null,
    Int(i32),
    String(String),
    Field(i32),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    EqualTo(Box<Expr>, Box<Expr>),
    VariadicEqual(Box<Expr>, Vec<Box<Expr>>),
    NotEqualTo(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    Macro(String),
}

fn main() {
    let sexpr: Value = serde_json::from_str(
        r#"["and", ["!=", ["field", 3], null], ["or", [">", ["field", 4], 25], ["=", ["field", 2], "Jerry"]]]"#
    ).unwrap();

    println!("{:?}", sexpr);

    let expr = to_expr(sexpr);
    println!("{:?}", expr);
}

fn to_expr(v: Value) -> Box<Expr> {
    use serde_json::Value::*;

    let res = match v {
        Number(n) => {
            let n = n.as_i64().unwrap() as i32;
            Expr::Int(n)
        }
        String(s) => Expr::String(s),
        Array(v) => match v.as_slice() {
            // TODO, there might be other variadic functions
            [String(s), x, rest @ ..] if rest.len() > 1 => {
                let exprs: Vec<Box<Expr>> = rest.iter().cloned().map(to_expr).collect();
                Expr::VariadicEqual(to_expr(x.to_owned()), exprs)
            }
            [String(s), x, y] => {
                let expr = match s.as_str() {
                    "and" => Expr::And,
                    "or" => Expr::Or,
                    "!=" => Expr::NotEqualTo,
                    ">" => Expr::GreaterThan,
                    "<" => Expr::LessThan,
                    "=" => Expr::EqualTo,
                    _ => panic!(),
                };
                expr(to_expr(x.to_owned()), to_expr(y.to_owned()))
            }
            // Bad way of checking if it's a Field type
            [String(s), Number(i)] => Expr::Field(i.as_i64().unwrap() as i32),
            // Bad way of checking if it's a macro
            [String(s), String(n)] => Expr::Macro(n.to_owned()),
            _ => Expr::Null,
        },
        _ => Expr::Null,
    };

    Box::new(res)
}

#[cfg(test)]
mod tests {
    use super::Expr::*;
    use super::*;

    #[test]
    fn test_case_1() {
        let v = serde_json::from_str(r#"[">", ["field", 4], 35]"#).unwrap();

        assert_eq!(
            to_expr(v),
            Box::new(GreaterThan(Box::new(Field(4)), Box::new(Int(35))))
        )
    }

    #[test]
    fn test_case_2() {
        let v =
            serde_json::from_str(r#"["and", ["<", ["field", 1], 5], ["=", ["field", 2], "joe"]]"#)
                .unwrap();

        assert_eq!(
            to_expr(v),
            Box::new(And(
                Box::new(LessThan(Box::new(Field(1)), Box::new(Int(5)))),
                Box::new(EqualTo(Box::new(Field(2)), Box::new(String("joe".into())))),
            ))
        )
    }

    #[test]
    fn test_case_4() {
        let v = serde_json::from_str(
            r#"["or", ["!=", ["field", 3], "2015-11-01"], ["=", ["field", 1], 456]]"#,
        )
        .unwrap();

        assert_eq!(
            to_expr(v),
            Box::new(Or(
                Box::new(NotEqualTo(
                    Box::new(Field(3)),
                    Box::new(String("2015-11-01".into())),
                )),
                Box::new(EqualTo(Box::new(Field(1)), Box::new(Int(456)))),
            ))
        )
    }

    #[test]
    fn test_case_6() {
        let v = serde_json::from_str(r#"["and", ["!=", ["field", 3], null], ["or", [">", ["field", 4], 25], ["=", ["field", 2], "Jerry"]]]"#).unwrap();

        assert_eq!(
            to_expr(v),
            Box::new(And(
                Box::new(NotEqualTo(Box::new(Field(3)), Box::new(Null))),
                Box::new(Or(
                    Box::new(GreaterThan(Box::new(Field(4)), Box::new(Int(25)))),
                    Box::new(EqualTo(
                        Box::new(Field(2)),
                        Box::new(String("Jerry".into())),
                    )),
                )),
            ))
        )
    }

    #[test]
    fn test_case_x() {
        let v: serde_json::Value =
            serde_json::from_str(r#"["=", ["field", 3], 25, 26, 27]"#).unwrap();

        assert_eq!(
            to_expr(v),
            Box::new(VariadicEqual(
                Box::new(Field(3)),
                vec![Box::new(Int(25)), Box::new(Int(26)), Box::new(Int(27))],
            ))
        )
    }

    #[test]
    fn test_case_7() {
        let v = serde_json::from_str(r#"["and", ["<", ["field", 1], 5], ["macro", "is_joe"]]"#)
            .unwrap();

        assert_eq!(
            to_expr(v),
            Box::new(And(
                Box::new(LessThan(Box::new(Field(1)), Box::new(Int(5)))),
                Box::new(Macro("is_joe".into())),
            ))
        )
    }
}
