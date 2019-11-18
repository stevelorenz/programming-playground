//
// test_collections.rs
//

use std::collections::HashMap;

fn main() {
    // *** Vector
    // Vec is loaded by default.
    let mut v: Vec<i32> = Vec::new();
    v.push(1);
    v.push(2);
    v.push(3);
    v.push(4);
    let second: &i32 = &v[1];
    println!("The vector: {:#?}", v);
    println!("The second element is {}", second);

    // This is like python dict.get(value, None). then check if the return value is None.
    match v.get(1) {
        Some(second) => println!("The second element is {}", second),
        None => println!("There is no second element!"),
    };

    println!("Iterate over elements in a vector:");
    for i in &v {
        println!("{}", i);
    }

    // *** String
    // to_string or String::from are equal, just a matter of choice
    let s: String = "initial contents".to_string();
    let suffix = String::from("aloha!");
    let mut out = s + &suffix;
    println!("The final output: {}", out);
    // The s is already removed by + operator for a string.
    // println!("The final output: {}", s);

    // Use format! macro to cat strings.
    let s1 = String::from("tic");
    let s2 = String::from("tac");
    let s3 = String::from("toe");
    out = format!("{}-{}-{}", s1, s2, s3);
    println!("The final output: {}", out);

    // chars to iterate individual unicode scalar values.
    for c in out.chars() {
        print!("{}-", c);
    }
    println!();

    // *** Hash Map
    let mut scores = HashMap::new();
    scores.insert("Blue".to_string(), 10);
    scores.insert("Yellow".to_string(), 50);
    println!("The hashmap: {:#?}", scores);
}
