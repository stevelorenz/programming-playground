pub fn main() {
    let s = String::from("hello");
    take_ownership(s); // s's value moves into the function

    let x = 17;
    makes_copy(x);
    println!("{}", x);
}

fn take_ownership(some_string: String) {
    println!("{}", some_string);
} // Here some_string goes out of the scope and the memory is freed.

fn makes_copy(some_integer: i32) {
    println!("{}", some_integer);
} // Here some integer goes out of scope. Nothing special happens.
