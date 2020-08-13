pub fn main() {
    let s1 = get_ownership();
    println!("{}", s1);

    let s2 = String::from("hello");
    let s3 = takes_and_give_back(s2);
    println!("{}", s3);
} // s2 goes out of the scope but was removed, so nothing happens.

fn get_ownership() -> String {
    let some_string = String::from("hello");
    some_string // some_string is returned and moves out to the calling function.
}

fn takes_and_give_back(some_string: String) -> String {
    some_string // Returns the ownership back to the calling function.
}
