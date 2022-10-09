#![warn(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]

use num::Complex;
use std::str::FromStr;

fn escape_time(c: Complex<f64>, limit: usize) -> Option<usize> {
    let mut z = Complex { re: 0.0, im: 0.0 };
    for i in 0..limit {
        if z.norm_sqr() > 4.0 {
            return Some(i);
        }
        z = z * z + c;
    }
    return None;
}

// <T: FromStr> is like the function template in C++, means this function works on
// ANY type that implements the FromStr trait!
fn parse_pair<T: FromStr>(s: &str, separator: char) -> Option<(T, T)> {
    match s.find(separator) {
        None => None,
        Some(index) => match (T::from_str(&s[..index]), T::from_str(&s[index + 1..])) {
            (Ok(l), Ok(r)) => Some((l, r)),
            _ => None,
        },
    }
}

#[test]
fn test_parse_pair() {
    assert_eq!(parse_pair::<i32>("", ','), None);
    assert_eq!(parse_pair::<i32>("10,20", ','), Some((10, 20)));
    // Rust could infer the type of the given parameter and perform the function.
    assert_eq!(parse_pair("20,30", ','), Some((20, 30)));
    assert_eq!(parse_pair::<f64>("0.5x", 'x'), None);
    assert_eq!(parse_pair::<f64>("0.5x1.5", 'x'), Some((0.5, 1.5)));
}

fn main() {
    println!("Hello, world!");
}
