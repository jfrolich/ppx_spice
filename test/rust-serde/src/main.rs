//! Mirrors the types in `test/src/SerdeRust.res`. Each case is written to
//! `fixtures/serde-rust.json` under the name the ReScript test looks it up by.

use std::collections::BTreeMap;

use serde::Serialize;
use serde_json::{json, Value};

#[derive(Serialize)]
enum Size {
    S,
    M,
    #[serde(rename = "large")]
    L,
}

#[derive(Serialize)]
enum Poly {
    Left,
    Right,
    Payload(i32),
    Pair(String, bool),
}

#[derive(Serialize)]
enum Ext {
    Unit,
    #[serde(rename = "renamed")]
    Renamed,
    #[serde(rename = "둘")]
    Two,
    Single(i32),
    Float(f64),
    Pair(String, bool),
    Rec {
        a: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        b: Option<i32>,
    },
    Opt(Option<i32>),
    List(Vec<i32>),
    Map(BTreeMap<String, i32>),
    Nested(Box<Ext>),
    Point(Point),
}

#[derive(Serialize)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Serialize)]
#[serde(tag = "type")]
enum Int {
    Regular,
    Overlap {
        #[serde(rename = "cameraSize")]
        camera_size: Size,
    },
    #[serde(rename = "full")]
    Full {
        #[serde(rename = "screenFit")]
        screen_fit: String,
        punch_in: bool,
    },
    Point(Point),
    Map(BTreeMap<String, i32>),
}

#[derive(Serialize)]
#[serde(tag = "kind")]
enum Nested {
    SideBySide { position: Poly, style: Int },
}

#[derive(Serialize)]
#[serde(tag = "type")]
enum Gen<T> {
    G { value: T },
    E,
}

#[derive(Serialize)]
enum Tree {
    Leaf,
    Node {
        left: Box<Tree>,
        right: Box<Tree>,
        value: i32,
    },
}

/// Written with `null` for `None`, serde's default, to check spice reads it.
#[derive(Serialize)]
#[serde(tag = "type")]
enum WithNull {
    N { b: Option<i32> },
}

fn main() {
    let mut cases: BTreeMap<&str, Value> = BTreeMap::new();
    let mut add = |name, value: Value| {
        cases.insert(name, value);
    };
    let to = |value: &dyn erased::Ser| value.to_value();

    add("size_s", to(&Size::S));
    add("size_l", to(&Size::L));
    add("poly_left", to(&Poly::Left));
    add("poly_right", to(&Poly::Right));
    add("poly_payload", to(&Poly::Payload(-3)));
    add("poly_pair", to(&Poly::Pair("a\"b".into(), true)));
    add("ext_unit", to(&Ext::Unit));
    add("ext_renamed", to(&Ext::Renamed));
    add("ext_two", to(&Ext::Two));
    add("ext_single", to(&Ext::Single(7)));
    add("ext_float", to(&Ext::Float(1.5)));
    add("ext_pair", to(&Ext::Pair("x".into(), false)));
    add("ext_rec", to(&Ext::Rec { a: "a".into(), b: Some(2) }));
    add("ext_rec_none", to(&Ext::Rec { a: "a".into(), b: None }));
    add("ext_opt_some", to(&Ext::Opt(Some(1))));
    add("ext_opt_none", to(&Ext::Opt(None)));
    add("ext_list", to(&Ext::List(vec![1, 2, 3])));
    add("ext_list_empty", to(&Ext::List(vec![])));
    add(
        "ext_map",
        to(&Ext::Map(BTreeMap::from([("a".into(), 1), ("b".into(), 2)]))),
    );
    add(
        "ext_nested",
        to(&Ext::Nested(Box::new(Ext::Nested(Box::new(Ext::Unit))))),
    );
    add("ext_point", to(&Ext::Point(Point { x: 1, y: -2 })));
    add("int_regular", to(&Int::Regular));
    add("int_overlap", to(&Int::Overlap { camera_size: Size::L }));
    add(
        "int_full",
        to(&Int::Full { screen_fit: "cover".into(), punch_in: true }),
    );
    add("int_point", to(&Int::Point(Point { x: 3, y: 4 })));
    add("int_map", to(&Int::Map(BTreeMap::from([("k".into(), 5)]))));
    add("int_map_empty", to(&Int::Map(BTreeMap::new())));
    add(
        "nested",
        to(&Nested::SideBySide {
            position: Poly::Pair("p".into(), false),
            style: Int::Overlap { camera_size: Size::M },
        }),
    );
    add(
        "gen_g",
        to(&Gen::G { value: vec!["a".to_string(), "b".to_string()] }),
    );
    add("gen_e", to(&Gen::<Vec<String>>::E));
    add(
        "tree",
        to(&Tree::Node {
            left: Box::new(Tree::Leaf),
            right: Box::new(Tree::Node {
                left: Box::new(Tree::Leaf),
                right: Box::new(Tree::Leaf),
                value: 2,
            }),
            value: 1,
        }),
    );
    add("with_null", to(&WithNull::N { b: None }));

    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../fixtures/serde-rust.json");
    let text = serde_json::to_string_pretty(&json!(cases)).unwrap();
    std::fs::write(path, text + "\n").unwrap();
}

mod erased {
    pub trait Ser {
        fn to_value(&self) -> serde_json::Value;
    }

    impl<T: serde::Serialize> Ser for T {
        fn to_value(&self) -> serde_json::Value {
            serde_json::to_value(self).unwrap()
        }
    }
}
