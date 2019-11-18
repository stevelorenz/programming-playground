use std::collections::HashMap;

mod front_of_house;

pub use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    // Absolute path
    // crate::front_of_house::hosting::add_to_wait_list();
    hosting::add_to_wait_list();
    // Relative path, the eat_at_restaurant and front_of_house are defined in the same level.
    // front_of_house::hosting::add_to_wait_list()
    let mut map = HashMap::new();
    map.insert(1, 2);
}
