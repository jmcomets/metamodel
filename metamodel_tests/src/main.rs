#[macro_use] extern crate metamodel_derive;

use std::collections::HashMap;

use metamodel::{Type, FieldType, Model};

fn main() {
    #[derive(Model)]
    struct Test {
        foo: i32,
        bar: (bool, f32),
        baz: Vec<i64>,
        qux: HashMap<String, Vec<f64>>,
        tix: Box<str>,
    }

    assert_eq!(Test::to_type(), {
        Type::new(vec![
            FieldType::Int,
            FieldType::Tuple(vec![FieldType::Bool, FieldType::Float]),
            FieldType::List(Box::new(FieldType::Long)),
            FieldType::Dict(Box::new(FieldType::Str), Box::new(FieldType::List(Box::new(FieldType::Double)))),
            FieldType::Str,
        ])
    });
}
