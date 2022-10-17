#![allow(elided_lifetimes_in_paths, dead_code)]

pub struct Queue {
    older: Vec<char>,   // older elements, the eldest last.
    younger: Vec<char>, // young elements, the youngst last.
}

impl Queue {
    
    const DUMMY:i32 = 0;

    pub fn new() -> Queue {
        return Queue {
            older: Vec::new(),
            younger: Vec::new(),
        };
    }

    pub fn push<'a>(&'a mut self, c: char) {
        self.younger.push(c);
    }

    /// Pop a character off the front of a queue.
    /// Return Some(c) if there was a character to pop, or None if the queue was empty.
    pub fn pop(&mut self) -> Option<char> {
        if self.older.is_empty() {
            if self.younger.is_empty() {
                return None;
            }

            // Bring the elements in younger to older
            use std::mem::swap;
            swap(&mut self.older, &mut self.younger);
            self.older.reverse();
        }
        return self.older.pop();
    }
}

#[test]
fn test_push_pop() {
    let mut q = Queue {
        older: vec![],
        younger: vec![],
    };
    assert_eq!(Queue::DUMMY, 0);

    q.push('0');
    q.push('1');
    assert_eq!(q.pop(), Some('0'));

    assert_eq!(q.pop(), Some('1'));
    assert_eq!(q.pop(), None);
}

impl Queue {
    pub fn is_empty(&self) -> bool {
        return self.older.is_empty() && self.younger.is_empty();
    }
}

#[test]
fn test_is_empty() {
    let mut q = Queue::new();
    assert!(q.is_empty());
    q.push('0');
    assert!(!q.is_empty());
}

impl Queue {
    pub fn split(self) -> (Vec<char>, Vec<char>) {
        return (self.older, self.younger);
    }
}

#[test]
fn test_split() {
    let mut q = Queue::new();

    q.push('P');
    q.push('D');
    assert_eq!(q.pop(), Some('P'));
    q.push('X');

    let (older, younger) = q.split();
    assert_eq!(older, vec!['D']);
    assert_eq!(younger, vec!['X']);
}

trait Vehicle {
    fn drive(&self);
}

struct Truck;

impl Vehicle for Truck {
    fn drive(&self) {
        println!("The truck is driving!");
    }
}

#[test]
fn test_box() {
    // This is a trait type, the size can't be computed at the compile time, put it on the heap
    let t: Box<dyn Vehicle>;
    t = Box::new(Truck);
    t.drive();
}
