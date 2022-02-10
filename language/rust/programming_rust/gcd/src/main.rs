#![warn(rust_2018_idioms)]

fn gcd(mut n: u64, mut m: u64) -> u64 {
    assert!(n != 0 && m != 0);

    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t
        }
        m = m % n;
    }
    return n;
}

// It's very convinient to add test functions together with the code
#[test]
fn test_gcd() {
    assert_eq!(gcd(14, 15), 1);

    assert_eq!(gcd(2 * 3 * 5 * 11 * 17, 3 * 7 * 11 * 13 * 19), 3 * 11);
}

fn main() {
    let ret = gcd(2, 4);
    println!("The GCD of 2 and 4: {ret}");
}
