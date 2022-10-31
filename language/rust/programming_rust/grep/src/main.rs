// use package_name::module_name
use colored::*;

use grep::grep;

fn main() {
    match grep::grep_main() {
        Ok(_) => (),
        Err(err) => {
            eprintln!("{}", err.to_string().red());
            std::process::exit(1);
        }
    }
}
