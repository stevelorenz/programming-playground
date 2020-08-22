enum List {
    Cons(i32, Box<List>),
    Nil,
}

use crate::List::{Cons, Nil};
pub fn main() {
    let _list = Cons(1, Box::new(Cons(2, Box::new(Cons(3, Box::new(Nil))))));
}
