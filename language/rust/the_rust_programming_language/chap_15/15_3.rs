use std::ops::Deref;

// Define a customized box type with generics.
struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

fn hello(name: &str) {
    println!("Hello, {}!", name);
}

pub fn main() {
    let x = 5;
    let y = Box::new(x);

    assert_eq!(5, x);
    assert_eq!(5, *y);

    let x1 = 5;
    let y1 = MyBox::new(x1);
    assert_eq!(5, *y1);

    let m = MyBox::new(String::from("MyBox"));
    hello(&m);
}
