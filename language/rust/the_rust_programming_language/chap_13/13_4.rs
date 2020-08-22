#[derive(Debug)]
struct Shoe {
    size: u32,
    style: String,
}

fn shoe_in_my_size(shoes: Vec<Shoe>, shoe_size: u32) -> Vec<Shoe> {
    shoes.into_iter().filter(|s| s.size == shoe_size).collect()
}

pub fn main() {
    let shoes = vec![
        Shoe {
            size: 10,
            style: String::from("sneaker"),
        },
        Shoe {
            size: 13,
            style: String::from("sandal"),
        },
        Shoe {
            size: 10,
            style: String::from("boot"),
        },
    ];
    println!("{:#?}", shoes);

    let in_my_size = shoe_in_my_size(shoes, 10);
    println!("{:#?}", in_my_size);
}
