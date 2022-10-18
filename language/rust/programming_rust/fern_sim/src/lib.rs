#![warn(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths, dead_code)]

pub const ROOM_TEMPERATURE: f64 = 20.0;

pub mod plant_structure;
pub mod simulation;
pub mod spores;

pub use plant_structure::Fern;
pub use simulation::Terrarium;

pub mod net;

use std::ops::Range;

/// Return true if two ranges overlap
/// 
///    assert_eq!(overlap(0..7, 3..10), true);
fn overlap(r1: Range<usize>, r2: Range<usize>) -> bool {
    return r1.start < r1.end && r2.start < r2.end && r1.start < r2.end && r2.start < r1.end;
}
