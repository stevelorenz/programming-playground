#![allow(dead_code, unused_imports, unused_variables)]

#[derive(Clone, Debug)]
pub struct Queue<T> {
    stack_a: Vec<T>,
    stack_b: Vec<T>,
}

pub struct Dummy<'a, T> {
    q: &'a Queue<T>,
    len: usize,
}

impl<T> Queue<T> {
    pub fn new() -> Self {
        return Queue {
            stack_a: Vec::new(),
            stack_b: Vec::new(),
        };
    }

    pub fn push(&mut self, t: T) {
        self.stack_b.push(t);
    }

    pub fn is_empty(&self) -> bool {
        return self.stack_a.is_empty() && self.stack_b.is_empty();
    }

    pub fn len(&self) -> usize {
        return self.stack_a.len() + self.stack_b.len();
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.stack_a.is_empty() {
            use std::mem::swap;

            if self.stack_b.is_empty() {
                return None;
            }

            while !self.stack_b.is_empty() {
                self.stack_a
                    .push(self.stack_b.pop().expect("Failed to pop from the stack_b"));
            }
        }
        return self.stack_a.pop();
    }

    pub fn split(self) -> (Vec<T>, Vec<T>) {
        return (self.stack_a, self.stack_b);
    }
}

#[test]
fn test_queue() {
    // Explicitly give the type of the Queue
    let mut q = Queue::<char>::new();
    q.push('0');
    q.push('1');
    assert_eq!(q.len(), 2);
    assert_eq!(q.pop(), Some('0'));
    assert_eq!(q.pop(), Some('1'));
    assert_eq!(q.pop(), None);
    drop(q);

    // Let Rust compiler inference the type
    let mut q = Queue::new();
    let mut r = Queue::new();
    q.push("ABC".to_string());
    r.push(3.14);

    let dummy_ref = Dummy {
        q: &q,
        len: q.len(),
    };
    assert_eq!(dummy_ref.q.len(), q.len());
}
