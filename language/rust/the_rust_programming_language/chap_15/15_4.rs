struct CustomSmartPointer {
    data: String,
}

impl Drop for CustomSmartPointer {
    fn drop(&mut self) {
        println!("Dropping CustomSmartPointer with data `{}`!", self.data);
    }
}

pub fn main() {
    let c = CustomSmartPointer {
        data: String::from("my stuff"),
    };
    drop(c);
    println!("CustomSmartPointer dropped before the end of the main.");
}
