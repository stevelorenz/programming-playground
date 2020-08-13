//
// tuple.rs
//

fn reverse(pair: (i32, bool)) -> (bool, i32) {
    let (integer, boolean) = pair;
    return (boolean, integer);
}

fn main() {
    let long_tuple = (
        1u8, 2u16, 3u32, 4u64, -1i8, -2i16, -3i32, -4i64, 0.1f32, 0.2f64, 'a', true,
    );
    println!("Long tuple first value {}", long_tuple.0);
    println!("Long tuple second value {}", long_tuple.1);
    println!("Long tuple is {:?}", long_tuple);

    let pair = (1, true);
    println!("The reverse pair is {:?}", reverse(pair));
}
