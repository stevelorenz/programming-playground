#![allow(dead_code, unused_imports)]

use reqwest;
use std::error::Error;
use std::io;

// This limit that the return error type MUST implement the Error trait!
// This error trait has the basic methods of description and cause.
// Box<dyn Error> is a runtime generic Error type.
fn http_get_main(url: &str) -> Result<(), Box<dyn Error>> {
    let mut response = reqwest::blocking::get(url)?;
    if !response.status().is_success() {
        return Err(format!("{}", response.status()))?;
    }

    // Read the response body and write it to stdout
    let stdout = io::stdout();
    // I'm not fully sure why response should also be a mutable reference
    io::copy(&mut response, &mut stdout.lock())?;

    return Ok(());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: http-get URL");
        std::process::exit(1);
    }

    match http_get_main(&args[1]) {
        Ok(()) => (),
        Err(err) => eprintln!("{}", err),
    }
}
