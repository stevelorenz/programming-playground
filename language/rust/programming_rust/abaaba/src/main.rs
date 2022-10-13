#![allow(dead_code, unused_variables, unused_assignments)]

use colored::*;
use std::collections::HashMap;
use std::rc::Rc;

type Table = HashMap<String, Vec<String>>;

fn print_slice(n: &[u64]) {
    for e in n {
        println!("{}", e);
    }
}

fn test_ownership() -> () {
    println!("{}", "test_ownership --------");
    let a: u32 = 1000;
    // let a: u32 = 100000000;
    let b: u32 = 2000;

    let c: u32;
    // Rust will check integer overflow at runtime in debug mode.
    c = a * b;

    println!("c={}", c);

    assert_eq!(false as i32, 0);
    assert_eq!(true as i32, 1);

    println!("{}_{}", "\u{CA0}", "\u{CA0}");

    assert_eq!('\u{CA0}' as u16, 0xca0);
    assert_eq!('8'.to_digit(10), Some(8));

    // Pattern matching feels pretty good!
    let _d = match '8'.to_digit(10) {
        Some(v) => v,
        None => {
            std::process::exit(1);
        }
    };

    let _e = match 't'.to_digit(10) {
        Some(v) => v,
        None => {
            eprintln!("{}", "Can not convert 't' to digit!".red());
            0
        }
    };

    let text: String = "I see the eigenvalue in thine eye".to_string();
    let (head, tail) = text.split_at(21);
    assert_eq!(head, "I see the eigenvalue ");
    assert_eq!(tail, "in thine eye");

    let t = (12, "eggs");
    // Just like the unique pointer, all memory is freed immediately when b goes out of the scope.
    // unless b is moved! Comparing to C++, Rust uses move by default and explicitly use Copy.
    let _b = Box::new(t);

    let lazy_caterer: [u32; 6] = [1, 2, 4, 7, 11, 16];
    let taxonomy = ["Animalia", "Arthropoda", "Insecta"];
    println!("{:?}", taxonomy);
    assert_eq!(lazy_caterer[3], 7);
    assert_eq!(taxonomy.len(), 3);

    let mut sieve = [true; 10000];
    for i in 2..100 {
        if sieve[i] {
            let mut j = i * i;
            while j < 10000 {
                sieve[j] = false;
                j += i;
            }
        }
    }
    assert!(sieve[211]);
    assert!(!sieve[9876]);

    let mut chaos = [3, 5, 4, 1, 2];
    chaos.sort();
    assert_eq!(chaos, [1, 2, 3, 4, 5]);

    let mut primes: Vec<u64> = vec![2, 3, 5, 7];
    // I'm not familier with generic function yet.
    assert_eq!(primes.iter().product::<u64>(), 210);

    primes.push(11);
    primes.push(13);

    assert_eq!(primes.iter().product::<u64>(), 30030);

    let mut pal = Vec::new();
    pal.push("step");
    pal.push("on");
    pal.push("no");
    pal.push("pets");
    assert_eq!(pal, vec!["step", "on", "no", "pets"]);

    let v: Vec<u64> = (0..5).collect();
    assert_eq!(v, [0, 1, 2, 3, 4]);

    let mut palindrome = vec!["a man", "a plan", "a canal", "panama"];
    palindrome.reverse();

    let mut v = Vec::with_capacity(2);
    assert_eq!(v.len(), 0);
    assert_eq!(v.capacity(), 2);
    v.push(1);
    v.push(2);
    v.push(3);
    assert_eq!(v.len(), 3);
    println!("The capacity is now {}", v.capacity());

    let mut v = vec!["Snow Puff", "Glass Gem"];
    assert_eq!(v.pop(), Some("Glass Gem"));
    assert_eq!(v.pop(), Some("Snow Puff"));
    assert_eq!(v.pop(), None);

    // Only print the first two elements
    print_slice(&primes[0..2]);

    assert!("ONE".to_lowercase() == "one");
    assert!("peanut".contains("nut"));

    for word in "veni, vidi, vici".split(", ") {
        assert!(word.starts_with("v"));
    }

    let s1 = vec!["udon".to_string(), "ramen".to_string(), "soba".to_string()];
    // s is already moved to t, t is now the owner of the vector
    let t1 = s1;
    // The next line panic because the s is already moved to t, so it can not
    // be further moved to the u value.
    // let u1 = s1;

    let s2 = vec!["udon".to_string(), "ramen".to_string(), "soba".to_string()];
    let t2 = s2.clone(); // Use deep copy explicitly!
    let u2 = s2.clone();

    {
        let mut s = "Govinda".to_string();
        s = "Siddhartha".to_string(); // The value "Govinda" is already dropped here.
    }

    {
        let mut s = "Govinda".to_string();
        let t = s;
        s = "Siddhartha".to_string(); // Nothing is dropped here because the value of s is moved to
                                      // t!
    }

    struct Person {
        name: String,
        birth: i32,
    }

    // Move the owernership of the vector to the composer
    let mut composers: Vec<Person> = Vec::new();
    composers.push(Person {
        name: "Musterman".to_string(), // the owernership of String is moved to the struct
        birth: 1525,
    });
    // After this push function, the owner of the struct is moved to the vector
    // so the vector now also becomes the indirect owner of the String.
    assert_eq!(composers.len(), 1);

    let mut v = Vec::new();
    for i in 101..106 {
        v.push(i.to_string());
    }
    // This line is error because the Vec can not track uninitialized value
    // let third = v[2];
    let third = v[2].clone();
    println!("The third element of the vector v is: {}", third);

    // expect function will panic
    let fifth = v.pop().expect("vector empty!");
    let second = v.swap_remove(1);
    assert_eq!(second, "102");

    #[derive(Copy, Clone)]
    struct Label {
        number: i32,
    }

    fn print_label(l: Label) {
        println!("STAMP: {}", l.number);
    }

    let l = Label { number: 3 };
    print_label(l);
    println!("My label number is {}", l.number);

    let s3: Rc<String> = Rc::new("shirataki".to_string());
    let t3 = s3.clone(); // Clone a Rc value does not copy the T
    let u3 = s3.clone(); // Create an another pointer to it and increments the reference count.

    assert_eq!(t3.find("taki"), Some(5));

    return ();
}

fn sort_works(table: &mut Table) {
    for (_, works) in &mut (*table) {
        // equals to works.sort(). The dot operator borrows and dereferences implicitly!
        (*works).sort();
    }
}

fn show_table(table: &Table) {
    for (artist, works) in &(*table) {
        println!("works by {}: ", *(artist));
        for work in works {
            println!("   {}", *(work));
        }
    }
}

static mut STASH: &i32 = &128;

// The lifetime annotation must be static here, because the p is assigned to
// a variable STASH with the static lifetime!!!
fn f<'a>(p: &'static i32) {
    unsafe {
        // The life time of p MUST outlive the static variable STASH.
        STASH = p;
    }
}

fn smallest<'a>(v: &'a [i32]) -> &'a i32 {
    let mut s = &v[0];
    for r in &v[1..] {
        if *r < *s {
            s = r;
        }
    }
    return s;
}

struct S3<'a, 'b> {
    x: &'a i32,
    y: &'b i32,
}

fn sum_r_xy<'a, 'b, 'c>(r: &'a i32, s: S3<'b, 'c>) -> i32 {
    return r + s.x + s.y;
}

struct StringTable {
    elements: Vec<String>,
}

impl StringTable {
    fn find_by_prefix<'a, 'b>(&'a self, prefix: &'b str) -> Option<&'a String> {
        for i in 0..self.elements.len() {
            if self.elements[i].starts_with(prefix) {
                return Some(&self.elements[i]);
            }
        }
        return None;
    }
}

// The first mutable reference vec is a mutable access -> It is exclusive access
// The second reference is a sharing/unmutable reference!
fn extend(vec: &mut Vec<f64>, slice: &[f64]) -> () {
    for element in slice {
        vec.push(*element);
    }
}

fn test_reference() -> () {
    println!("{}", "test_reference --------");
    let mut table = Table::new();
    table.insert(
        "Gesualdo".to_string(),
        vec![
            "many madrigals".to_string(),
            "Tenebrae Responsoria".to_string(),
        ],
    );
    table.insert(
        "Cellini".to_string(),
        vec![
            "Perseus with the head of Medusa".to_string(),
            "a salt cellar".to_string(),
        ],
    );
    // Use the read-only shared reference
    println!("Before sort ------");
    show_table(&table);

    sort_works(&mut table);
    println!("After sort ------");
    show_table(&table);

    let r;
    {
        let x = 1;
        r = &x;
    }
    // ERROR:
    // println!("{}", *r);

    let parabola = [9, 4, 1, 0, 1, 4, 9];
    let s = smallest(&parabola);
    println!("The smallest value of the array: {:?} is {}", parabola, s);

    struct S1<'a> {
        r: &'a i32,
    }

    let s;
    {
        let x = 10;
        s = S1 { r: &x };
        assert_eq!(*s.r, 10);
    }
    // Bad! the x is already dropped, the s.r refer to a already dropped value.
    // assert_eq!(*s.r, 10);

    struct S2<'a, 'b> {
        x: &'a i32,
        y: &'b i32,
    }

    let x = 10;
    let r;
    {
        let y = 20;
        {
            let s = S2 { x: &x, y: &y };
            r = s.x;
        }
    }
    println!("r: {}", r);

    let mut wave: Vec<f64> = Vec::new();
    let head = vec![0.0, 1.0];
    let tail = [0.0, -1.0];

    extend(&mut wave, &head);
    extend(&mut wave, &tail);
    assert_eq!(wave, vec![0.0, 1.0, 0.0, -1.0]);

    // Error! This may trigger the iterator invalidation!!! Rust denies this at compile time.
    // extend(&mut wave, &wave);

    let x = 42;
    let p = &x;
    assert_eq!(*p, 42);
    // This is impossible! Because there's a read-only reference to the x.
    // x += 1;
    assert_eq!(*p, 42);

    return ();
}

fn test_expression() -> () {
    println!("{}", "test_expression --------");
    for i in 0..3 {
        print!("{}, ", i);
    }
    println!("");

    for i in (std::ops::Range { start: 0, end: 3 }) {
        print!("{}, ", i);
    }
    println!("");

    let is_even = |x: u64| -> bool {
        return x % 2 == 0;
    };
    assert_eq!(is_even(14), true);
}

use std::error::Error;
use std::io::{stderr, Write};

fn print_error(mut err: &dyn Error) {
    let _ = writeln!(stderr(), "error: {}", err);

    loop {
        match err.source() {
            Some(source) => {
                let _ = writeln!(stderr(), "error: {}", err);
                err = source;
            }
            _ => {
                break;
            }
        }
    }

    // Use while let instead of loop
    // while let Some(source) = err.source() {
    //     let _ = writeln!(stderr(), "error: {}", err);
    //     err = source;
    // }
}

use std::io::BufRead;

type GenericError = Box<dyn std::error::Error + Send + Sync + 'static>;
type GenericResult<T> = Result<T, GenericError>;

fn read_numbers(file: &mut dyn BufRead) -> GenericResult<Vec<i64>> {
    let mut numbers = vec![];
    for line_result in file.lines() {
        let line = line_result?; // Read files can fail and lead to io::Error...
        numbers.push(line.parse()?); // Parsing integers can also fail
    }

    Ok(numbers)
}

use std::fmt;

#[derive(Debug, Clone)]
struct JsonError {
    message: String,
    line: usize,
    column: usize,
}

impl fmt::Display for JsonError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} ({}:{})", self.message, self.line, self.column)
    }
}

impl std::error::Error for JsonError {}

fn test_error_handling() -> () {}

fn main() {
    test_ownership();
    test_reference();
    test_expression();
    test_error_handling();
}
