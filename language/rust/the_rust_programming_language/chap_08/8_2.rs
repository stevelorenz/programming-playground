pub fn main() {
    let s1 = String::from("Hello, ");
    let s2 = String::from("World!");
    let s3 = s1 + &s2; // s1 has been moved here and can no longer be used.
    println!("{}", s3);

    let s1 = String::from("Hello, ");
    println!("{}", format!("{}{}", s1, s2));
    for c in "नमस्ते".chars() {
        println!("{}", c);
    }
}
