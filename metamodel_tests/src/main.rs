#[macro_use] extern crate metamodel_derive;

use std::collections::HashMap;

use metamodel::{Type, Model};

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
        Type::Tuple(Box::new([
            Type::Int,
            Type::Tuple(Box::new([Type::Bool, Type::Float])),
            Type::List(Box::new(Type::Long)),
            Type::Dict(Box::new(Type::Str), Box::new(Type::List(Box::new(Type::Double)))),
            Type::Str,
        ]))
    });
}
