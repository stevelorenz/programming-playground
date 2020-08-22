fn main() {
    let v1 = vec![1, 2, 3];
    let v1_iter = v1.iter();
    let total: i32 = v1_iter.sum();
    println!("{}", total);

    let total: i32 = v1.iter().map(|x| x + 1).sum();
    println!("{}", total);

    let v2: Vec<_> = v1.iter().map(|x| x + 1).collect();
    println!("{:#?}", v2);
}
