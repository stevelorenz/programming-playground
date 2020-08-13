pub fn main() {
    let mut s = String::from("hello");
    change_string(&mut s);
    println!("{}", s);

    let r1 = &mut s;
    let r2 = &mut s; // ERROR: Can not have more than one mutable reference.
    println!("{},{}", r1, r2);
}

fn change_string(some_string: &mut String) {
    some_string.push_str(", world");
}
