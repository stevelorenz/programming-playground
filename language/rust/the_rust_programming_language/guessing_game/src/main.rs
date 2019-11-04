use rand::Rng;
use std::cmp::Ordering;
use std::io;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1, 101);
    println!("The secret integer is {}", secret_number);

    println!("Guessing a integer!");
    loop {
        println!("Please input a integer");
        let mut guess = String::new();

        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to get input");

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Ignore invalid input. Please input a integer");
                continue;
            }
        };

        println!("You guessed {}", guess);

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small."),
            Ordering::Greater => println!("Too big."),
            Ordering::Equal => {
                println!("Bingo !!!");
                break;
            }
        }
    }
}
