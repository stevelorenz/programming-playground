#![allow(unused_variables, unused_imports, dead_code)]

use crate::plant_structure::{Fern, FernType};
use std::fs::File;
use std::time::Duration;

pub struct Terrarium {
    ferns: Vec<Fern>,
}

impl Terrarium {
    pub fn new() -> Terrarium {
        return Terrarium { ferns: vec![] };
    }

    pub fn load(filename: &str) -> Terrarium {
        File::open(filename).unwrap();
        return Terrarium {
            ferns: vec![Fern::new(FernType::Fiddlehead)],
        };
    }

    pub fn fern(&self, index: usize) -> &Fern {
        return &self.ferns[index];
    }

    pub fn apply_sunlight(&mut self, time: Duration) {
        for f in &mut self.ferns {
            for s in &mut f.stems {
                s.furled = false;
            }
        }
    }
}
