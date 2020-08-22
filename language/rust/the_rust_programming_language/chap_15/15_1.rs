pub fn main() {
    // Rust's style of malloc
    let b = Box::new(5);
    println!("b = {}", b);
}
