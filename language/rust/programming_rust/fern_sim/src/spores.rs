#![allow(dead_code, unused_variables, unused_imports)]

use cells::{Cell, Gene};

pub struct Spore {
    size: f64,
}

/// Simulate the production of a spore by meiosis
pub fn produce_spore(factory: &Sporangium) -> Spore {
    return Spore { size: 1.0 };
}

fn genes(sport: &Spore) -> Vec<Gene> {
    todo!()
}

fn recombine(parent: &mut Cell) {
    todo!()
}

pub struct Sporangium;

mod cells {
    pub struct Cell {
        x: f64,
        y: f64,
    }

    impl Cell {
        pub fn distance_from_origin(&self) -> f64 {
            return f64::hypot(self.x, self.y);
        }
    }

    pub struct Gene;
}
