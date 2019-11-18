//
// About: Simple snippets to try ownership in Rust
//

fn main() {
    let s1 = String::from("Blabla");
    let s2 = s1.clone();
    let n1: i32 = 17;

    takes_ownership(s1);
    make_copy(n1);

    // println!("The string s1: {}, s2: {}", s1, s2);
    println!("The string s2: {}", s2);
    println!("The number of n1: {}", n1);

    let s3 = String::from("Jabujabu");
    let mut s4 = take_and_giveback(s3);

    let len = calculate_len(&s4);
    println!("The length of {} is {}.", s4, len);

    append_str(&mut s4);
    println!("The content of s4 after append: {}", s4);

    let word_index = first_word_index(&s4);
    println!("The index of the first space is {}", word_index);
    // Let substring borrow from s4 as a slice reference
    let substr = &s4[..word_index];
    println!("The first word is {}", substr);

    println!("The first word with slice function: {}", get_first_word(&s4));
}

/// Take the ownership of a string
fn takes_ownership(some_str: String) {
    println!("Got a string:{}", some_str);
} // some_str is out of scope, memory is freed.

/// i32 has Copy trait, so the parameter is copied into function
/// the ownership is not taken.
fn make_copy(some_num: i32) {
    println!("Got a number: {}", some_num);
}

/// Get the ownership of a string and return back to another variable
fn take_and_giveback(a_string: String) -> String {
    a_string
}

// fn calculate_len(a_string: String) -> (String, usize) {
//     // String.len(): returns length of this String
//     let length = a_string.len();
//     (a_string, length)
// }

/// Use reference as a parameter
fn calculate_len(a_string: &String) -> usize {
    // a_string is here only reference
    a_string.len()
}

/// An example of using mutable reference
fn append_str(s: &mut String) {
    s.push_str(", aloha!");
}

/// Convert string into bytes and first the index of the first space
fn first_word_index(s: &String) -> usize {
    let bytes = s.as_bytes();
    // The enumerate returns the reference of the value
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return i;
        }
    }
    // Case: there is no space in the string
    s.len()
}

// Get the first word of a string as a string slice.
fn get_first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }
    &s[..]
}
