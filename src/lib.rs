use serde_json::Value;
use std::convert::TryFrom;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Null,
    Int(i32),
    String(String),
    Field(i32),
    Bool(bool),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    EqualTo(Box<Expr>, Box<Expr>),
    VariadicEqual(Box<Expr>, Vec<Box<Expr>>),
    NotEqualTo(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    Macro(String),
}

// One potential optimization is to pass a &mut Value so that
// I could use .take() instead of clone. For a problem of this size
// It doesn't matter, but if the tree was large and more complex that would
// be a pretty easy optimization
pub fn to_expr(v: &Value) -> Result<Box<Expr>, &'static String> {
    // This makes one copy of the input since a pure function conversion method shouldn't change
    // the data that was passed in. The idea is one copy of the tree is better then recursively copying
    // the tree as the previous version
    priv_to_expr(v.clone())
}

fn priv_to_expr(v: Value) -> Result<Box<Expr>, &'static String> {
    use serde_json::Value::*;

    let res = match v {
        Null => Expr::Null,
        Number(n) => {
            let n = n.as_i64().unwrap() as i32;
            Expr::Int(n)
        }
        String(s) => Expr::String(s),
        Bool(b) => Expr::Bool(b),
        Object(_) => panic!(), // TODO Objects aren't supported
        Array(mut v) => match v.as_mut_slice() {
            // TODO, there might be other variadic functions,
            // But for now we always assume it's equal
            // but for example AND 1, 2, 3 would be invalid SQL probably
            // also if you know rust and are wondering why there is `.cloned() and to_owned` here and there
            // it's because I can't use the rest pattern if `v` is the object instead of pointers
            // because the size of the object isn't know at compile time
            [String(s), x, rest @ ..] if rest.len() > 1 => {
                // Look what rustfmt did to my boy :'(
                let exprs: Vec<Box<Expr>> =
                    rest.iter_mut()
                        .map(|x| x.take())
                        .map(priv_to_expr)
                        .collect::<Result<Vec<Box<Expr>>, &'static std::string::String>>()?;

                Expr::VariadicEqual(priv_to_expr(x.take())?, exprs)
            }
            [String(s), x, y] => {
                let expr = match s.as_str() {
                    "and" => Expr::And,
                    "or" => Expr::Or,
                    "!=" => Expr::NotEqualTo,
                    ">" => Expr::GreaterThan,
                    "<" => Expr::LessThan,
                    "=" => Expr::EqualTo,
                    _ => panic!(), // Unimplemented operand
                };
                expr(priv_to_expr(x.take())?, priv_to_expr(y.take())?)
            }
            // Bad way of checking if it's a Field type
            // TODO, handle the error
            [String(s), Number(i)] => Expr::Field(i.as_i64().unwrap() as i32),
            // Bad way of checking if it's a macro
            [String(s), String(n)] => Expr::Macro(n.to_string()),
            // There are cases that aren't covered,
            // If you remove this _ the compiler will complain
            _ => Expr::Null,
        },
    };

    Ok(Box::new(res))
}

#[cfg(test)]
mod tests {
    use super::Expr::*;
    use super::*;

    #[test]
    fn test_case_1() {
        let v = serde_json::from_str(r#"[">", ["field", 4], 35]"#).unwrap();

        assert_eq!(
            to_expr(&v).unwrap(),
            Box::new(GreaterThan(Box::new(Field(4)), Box::new(Int(35))))
        )
    }

    #[test]
    fn test_case_2() {
        let v =
            serde_json::from_str(r#"["and", ["<", ["field", 1], 5], ["=", ["field", 2], "joe"]]"#)
                .unwrap();

        assert_eq!(
            to_expr(&v).unwrap(),
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
            to_expr(&v).unwrap(),
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
            to_expr(&v).unwrap(),
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
            to_expr(&v).unwrap(),
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
            to_expr(&v).unwrap(),
            Box::new(And(
                Box::new(LessThan(Box::new(Field(1)), Box::new(Int(5)))),
                Box::new(Macro("is_joe".into())),
            ))
        )
    }
}
