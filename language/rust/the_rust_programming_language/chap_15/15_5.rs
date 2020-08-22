use crate::List::{Cons, Nil};
use std::rc::Rc;

enum List {
    Cons(i32, Rc<List>),
    Nil,
}

pub fn main() {
    let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
    // Everytime Rc::clone is called, the reference counting is increased.
    let _b = Cons(3, Rc::clone(&a));
    let _c = Cons(4, Rc::clone(&a));
}
