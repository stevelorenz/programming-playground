pub use std::boxed::Box;
pub use std::collections::HashMap;
pub use std::string::ToString;

#[macro_export]
macro_rules! json {
    (null) => {
        $crate::Json::Null
    };
    // tt: token tree -> anything
    ([ $( $element:tt),* ]) => {
        $crate::Json::Array(vec![ $( json!($element) ),* ])
    };
    ({ $( $key:tt : $value:tt ),* }) => {
        {
            let mut fields = $crate::macros::Box::new(
                $crate::macros::HashMap::new());
            $(
                fields.insert($crate::macros::ToString::to_string($key),
                              json!($value));
            )*
            $crate::Json::Object(fields)
        }
    };
    ($other:tt) => {
        $crate::Json::from($other)
    };
}

#[test]
fn json_null() {
    use crate::Json;
    assert_eq!(json!(null), Json::Null);
}

#[test]
fn json_with_rust_expressions() {
    use crate::Json;
    const HELLO: &'static str = "hello";
    let macro_generated_value = json!({
        "math_works": (4-2 == 2),
        "en": HELLO,
        HELLO: "bonjour!"
    });

    let hand_coded_value = Json::Object(Box::new(
        vec![
            ("math_works".to_string(), Json::Boolean(true)),
            ("en".to_string(), Json::String("hello".to_string())),
            ("hello".to_string(), Json::String("bonjour!".to_string())),
        ]
        .into_iter()
        .collect(),
    ));
    assert_eq!(macro_generated_value, hand_coded_value);
}

// I still feel the syntax of declarative macro and procedure macro is
// tooooo difficult for me. LOL :)

#[macro_export]
macro_rules! my_vec {
    ( $( $x:expr ),* ) => {
        {
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push($x);
        )*
        temp_vec // temp_vec is dropped here
        }
    };
}

#[test]
fn test_my_vec() {
    let _ = my_vec![1, 2, 3];
}
