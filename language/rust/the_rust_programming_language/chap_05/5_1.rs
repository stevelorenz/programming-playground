struct User {
    username: String,
    email: String,
    sign_in_account: u64,
    active: bool,
}

fn build_user(email: String, username: String) -> User {
    User {
        email: email,
        username: username,
        active: true,
        sign_in_account: 1,
    }
}

fn main() {
    let mut user1 = User {
        email: String::from("someone@somedomain.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_account: 1,
    };
    user1.email = String::from("anotheremail@somedomain.com");

    // The struct update syntax
    let user2 = User {
        email: String::from("anotheremail@anotherdomain.com"),
        username: String::from("anotherusername123"),
        ..user1
    };
}
