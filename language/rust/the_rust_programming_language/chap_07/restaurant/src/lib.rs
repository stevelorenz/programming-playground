mod front_of_house;

mod back_of_house;

use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    // crate::front_of_house::hosting::add_to_waitlist();
    // front_of_house::hosting::add_to_waitlist();
    hosting::add_to_waitlist();

    let mut meal = crate::back_of_house::Breakfast::summer("Rye");
    meal.toast = String::from("Wheat");
    println!("I'd like {} toast please", meal.toast);

    let order1 = crate::back_of_house::Appetizer::Soup;
}
