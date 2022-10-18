#![allow(unused_imports, dead_code)]

use fern_sim::Terrarium;
use std::time::Duration;

#[test]
// Let the compiler allow us to do things that can statically prove will be panic!
// This feature is used to test code that should be panic!
#[allow(unconditional_panic)]
#[should_panic(expected = "divide by zero")]
fn dummy() {
    assert_eq!(1, 1);
    let _ = 1 / 0;
    return ();
}

#[test]
fn test_parse_int() -> Result<(), std::num::ParseIntError> {
    i32::from_str_radix("1024", 10)?;
    return Ok(());
}

#[test]
fn test_fiddlehead_unfurling() {
    let mut world = Terrarium::load("tests/unfurl_files/fiddlehead.tm");
    assert!(world.fern(0).is_furled());

    let one_hour = Duration::from_secs(60 * 60);
    world.apply_sunlight(one_hour);
    assert!(world.fern(0).is_fully_unfurled());
}
