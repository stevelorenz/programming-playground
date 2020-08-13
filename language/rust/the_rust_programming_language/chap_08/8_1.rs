pub fn main() {
    let v: Vec<i32> = Vec::new();
    // The type is infered.
    let mut v2 = vec![1, 2, 3];
    v2.push(4);

    let third: &i32 = &v2[2];
    println!("The third element is {}", third);

    println!("{:#?}", v2);
    match v2.get(2) {
        Some(third) => println!("The third element is {}", third),
        None => println!("There is no third element"),
    }
    for i in &mut v2 {
        *i += 50;
    }
} // vectors go out of scope and are freed.
