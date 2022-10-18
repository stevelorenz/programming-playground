#![allow(dead_code, unused_variables, unused_assignments, unused_imports)]

use std::collections::HashMap;
use std::rc::Rc;

type Table = HashMap<String, Vec<String>>;

fn print_slice(n: &[u64]) {
    for e in n {
        println!("{}", e);
    }
}

#[test]
fn test_ownership() -> () {
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
            eprintln!("{}", "Can not convert 't' to digit!");
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

#[test]
fn test_reference() -> () {
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

#[test]
fn test_expression() -> () {
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

#[test]
fn test_error_handling() -> () {}

use std::cmp::Ordering;
fn compare(n: i32, m: i32) -> Ordering {
    if n < m {
        Ordering::Less
    } else if n > m {
        Ordering::Greater
    } else {
        Ordering::Equal
    }
}

#[derive(Copy, Clone, Debug)]
enum HttpStatus {
    Ok = 200,
    NotModified = 304,
    NotFound = 404,
}

#[test]
fn test_enum() -> () {
    use std::mem::size_of;
    assert_eq!(size_of::<Ordering>(), 1);
    assert_eq!(size_of::<HttpStatus>(), 2);

    assert_eq!(HttpStatus::Ok as i32, 200);
}

fn http_status_from_u32(n: u32) -> Option<HttpStatus> {
    match n {
        200 => Some(HttpStatus::Ok),
        304 => Some(HttpStatus::NotModified),
        404 => Some(HttpStatus::NotFound),
        _ => None,
    }
}

enum Json {
    Null,
    Boolen(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(Box<HashMap<String, Json>>),
}

fn describe_point(x: i32, y: i32) -> &'static str {
    match (x.cmp(&0), y.cmp(&0)) {
        (Ordering::Equal, Ordering::Equal) => "at the original",
        (_, Ordering::Equal) => "on the x axis",
        (Ordering::Equal, _) => "on the y axis",
        (Ordering::Greater, Ordering::Greater) => "in the first quadrant",
        _ => "somewhere else",
    }
}

// Use the trait as a function argument.
// This is actually dynamic dispatch/polymorphism, so dyn keyword is used here.
fn say_hello_write(out: &mut dyn Write) -> std::io::Result<()> {
    out.write_all(b"hello world\n")?;
    return out.flush();
}

// Trait can be used as bound/limitation of a generic type T.
fn min<T: Ord>(value1: T, value2: T) -> T {
    if value1 < value2 {
        return value1;
    }
    return value2;
}

#[test]
fn test_traits() {
    use std::fs::File;
    let mut local_file = File::create("/tmp/hello.txt").expect("Failed to open file");
    say_hello_write(&mut local_file).expect("Failed to write to file!");

    let mut bytes = vec![];
    say_hello_write(&mut bytes).unwrap();
    assert_eq!(bytes, b"hello world\n");

    assert_eq!(min(1, 2), 1);

    let a: u64 = 1;
    let b: u64 = 2;
    assert_eq!(min(a, b), 1);

    struct Dummy {
        a: u64,
        b: u64,
    }

    let c: Dummy = Dummy { a: 1, b: 4 };
    let d: Dummy = Dummy { a: 2, b: 3 };
    // Error: The Dummy struct doesn't implement the Ord Trait required by the Generic
    // let e = min(c, d);
}

#[test]
fn test_drop() {
    struct Appellation {
        name: String,
        nicknames: Vec<String>,
    }

    use std::ops::Drop;

    impl Drop for Appellation {
        fn drop(&mut self) {
            println!("Drop Appellation with name: {}", self.name);
        }
    }

    let a = Appellation {
        name: "Musterman".to_string(),
        nicknames: vec!["Musterman".to_string()],
    };
}

fn triangle(n: i32) -> i32 {
    return (1..=n).fold(0, |sum, item| sum + item);
}

#[test]
fn test_iterators() {
    let v = vec!["a", "b", "c"];
    for element in &v {}

    // The vector implements the IntoIterator trait
    let mut iterator = (&v).into_iter();
    while let Some(element) = iterator.next() {
        println!("{}", element);
    }

    // while true loop with break can be written as "while let" syntax in Rust
    loop {
        match iterator.next() {
            Some(element) => println!("{}", element),
            None => break,
        }
    }

    use std::ffi::OsStr;
    use std::path::Path;
    // The iter method of a path return an iterator for paths.
    let path = Path::new("./");
    let mut iterator = path.iter();
    _ = iterator.next();

    use std::collections::BTreeSet;
    let mut favorites = BTreeSet::new();
    favorites.insert("A".to_string());
    favorites.insert("B".to_string());

    let mut it = favorites.into_iter();
    assert_eq!(it.next(), Some("A".to_string()));
    assert_eq!(it.next(), Some("B".to_string()));
    assert_eq!(it.next(), None);

    use rand::random;
    use std::iter::from_fn;

    const LEN_SIZE: usize = 1000;

    let lengths: Vec<f64> = from_fn(|| Some((random::<f64>() - random::<f64>()).abs()))
        .take(LEN_SIZE) // yield the first n elements
        .collect();
    assert_eq!(lengths.len(), LEN_SIZE);

    use std::iter::FromIterator;
    let mut outer = "Earth".to_string();
    let inner = String::from_iter(outer.drain(1..4));
    assert_eq!(outer, "Eh");
    assert_eq!(inner, "art");

    let text = " a \n b \n c \n d \n".to_string();
    let v: Vec<&str> = text.lines().map(str::trim).collect(); // Convert an iterator to a vector
    assert_eq!(v, ["a", "b", "c", "d"]);

    let text = " a \n b \n c \n d \n".to_string();
    // A chain of iterator adaptors work like a pipeline in Unix shell
    let v: Vec<&str> = text.lines().map(str::trim).filter(|s| *s != "c").collect();
    assert_eq!(v, ["a", "b", "d"]);

    use std::str::FromStr;
    let text = "1\nfrond .25 289\n3.1415 estuary\n";
    for number in text
        .split_whitespace()
        .filter_map(|w| f64::from_str(w).ok())
    {
        println!("{:4.2}", number.sqrt());
    }

    use std::collections::BTreeMap;

    let mut parks = BTreeMap::new();
    parks.insert("Portland", vec!["Mt. Tabor Park", "Forest Park"]);
    parks.insert("Kyoto", vec!["Tadasu-no-Mori Forest", "Maruyama Koen"]);

    let all_parks: Vec<_> = parks.values().cloned().collect();
    assert_eq!(
        all_parks,
        vec![
            vec!["Tadasu-no-Mori Forest", "Maruyama Koen"],
            vec!["Mt. Tabor Park", "Forest Park"],
        ]
    );

    let all_parks: Vec<_> = parks.values().flatten().cloned().collect();
    assert_eq!(
        all_parks,
        vec![
            "Tadasu-no-Mori Forest",
            "Maruyama Koen",
            "Mt. Tabor Park",
            "Forest Park",
        ]
    );

    let message = "To: jimb\r\n\
                   From: superego <editor@orilly.com>\r\n\
                   \r\n\
                   Blablablabla";
    let mut headers: Vec<String> = Vec::new();
    for header in message.lines().take_while(|l| !l.is_empty()) {
        headers.push(header.to_string());
    }

    assert_eq!(
        headers,
        vec![
            "To: jimb".to_string(),
            "From: superego <editor@orilly.com>".to_string()
        ]
    );

    let v: Vec<i32> = (1..4).chain(vec![20, 30, 40]).collect();
    assert_eq!(v, [1, 2, 3, 20, 30, 40]);

    let v: Vec<_> = (0..).zip("ABCD".chars()).collect();
    assert_eq!(v, vec![(0, 'A'), (1, 'B'), (2, 'C'), (3, 'D')]);

    struct I32Range {
        start: i32,
        end: i32,
    }

    impl Iterator for I32Range {
        type Item = i32;

        fn next(&mut self) -> Option<i32> {
            if self.start >= self.end {
                return None;
            }
            let result = Some(self.start);
            self.start += 1;
            return result;
        }
    }

    let mut pi = 0.0;
    let mut numerator = 1.0;

    for k in (I32Range { start: 0, end: 14 }) {
        pi += numerator / (2 * k + 1) as f64;
        numerator /= -3.0;
    }
    pi *= f64::sqrt(12.0);
    assert_eq!(pi as f32, std::f32::consts::PI);
}

#[test]
fn test_collections() {
    assert_eq!(
        [[1, 2], [3, 4], [5, 6]].join(&0),
        vec![1, 2, 0, 3, 4, 0, 5, 6]
    )
}

fn option_to_raw<T>(opt: Option<&T>) -> *const T {
    match opt {
        None => std::ptr::null(),
        Some(r) => r as *const T,
    }
}

#[test]
fn test_unsafe() {
    let mut x = 10;
    let ptr_x = &mut x as *mut i32; // Convert the reference to raw pointer

    let y = Box::new(20);
    let ptr_y = &(*y) as *const i32;

    unsafe {
        *ptr_x = *ptr_x + *ptr_y;
        assert_eq!(*ptr_x, 30);
    }
    assert_eq!(x, 30);

    assert_eq!(option_to_raw::<i32>(None), std::ptr::null());

    let trucks = vec!["garbage truck", "dump truck", "moonstruck"];
    let first: *const &str = &trucks[0];
    let last: *const &str = &trucks[2];

    assert_eq!(unsafe { last.offset_from(first) }, 2);
    assert_eq!(unsafe { first.offset_from(last) }, -2);
    assert_eq!(unsafe { *(first.add(2)) }, unsafe { *last });
}
