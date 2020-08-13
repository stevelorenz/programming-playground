fn main() {
    let s1 = String::from("test");
    let len = calculate_length(&s1);
    println!("The length of {} is {}.", s1, len);
}

fn calculate_length(some_string: &String) -> usize {
    return some_string.len();
} // THe some_string goes out of scope. But because it does not have ownership of what it refers to, nothing happens.
