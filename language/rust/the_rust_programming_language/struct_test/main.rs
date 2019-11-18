//
// About: Test Rust structs
//
//

struct User {
    username: String,
    email: String,
    active: bool,
    sign_in_count: u64,
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }

    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

fn area(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}

struct ColorRGB(u32, u32, u32);

fn build_user(email: String, username: String) -> User {
    User {
        // The field init shorthand syntax use the parameter automatically
        // If they have the same name
        email,
        username,
        active: true,
        sign_in_count: 1,
    }
}

fn main() {
    let user1 = build_user(String::from("Blabla"), String::from("Blabla@bla.bla"));
    // Use the struct update syntax to build a new struct based on a old one.
    let _user2 = User {
        email: String::from("hhh@hhh"),
        username: String::from("hhhh"),
        ..user1
    };

    let _black = ColorRGB(255, 255, 255);
    println!("The RGB of black: {}, {}, {}", _black.0, _black.1, _black.2);

    let rec1 = Rectangle {
        width: 30,
        height: 50,
    };
    println!("The area of the rectangle is {}.", area(&rec1));
    println!("rec1 is {:#?}", rec1);

    println!("The area of the rec1: {}", rec1.area());

    let rec2 = Rectangle {
        width: 10,
        height: 10,
    };
    println!("Can rec1 hold rec2? {}", rec1.can_hold(&rec2));

    let sq1 = Rectangle::square(10);
    println!("The area of square sq1 is {}.", sq1.area());
}
