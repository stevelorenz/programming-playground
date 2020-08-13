use std::env;
use std::process;

use minigrep;

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = minigrep::Config::new(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });
    // config return () in the success case, so only care about the error, so we don't need
    // unwarp_or_else to return the unwrapped value because it would be ()
    if let Err(e) = minigrep::run(config) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    }
}
