#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };
    println!("rect1 is {:#?}", rect1);
    println!("The area of the rectangle is {}.", area(&rect1));
    println!("The area of the rectangle is {}.", rect1.area());
}

fn area(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}
