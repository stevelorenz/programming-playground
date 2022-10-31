use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use std::path::PathBuf;

use colored::*;

fn grep<R>(target: &str, reader: R) -> io::Result<()>
where
    R: BufRead,
{
    for line_result in reader.lines() {
        let line = line_result?;
        if line.contains(target) {
            println!("{}", line.green());
        }
    }
    return Ok(());
}

pub fn grep_main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let pattern = match args.next() {
        Some(s) => s,
        None => Err("Error! Missing pattern! Usage: grep PATTERN FILE ...")?,
    };
    let files: Vec<PathBuf> = args.map(PathBuf::from).collect();

    if files.is_empty() {
        let stdin = io::stdin();
        grep(&pattern, stdin.lock())?;
    } else {
        for file in files {
            let f = File::open(file)?;
            grep(&pattern, BufReader::new(f))?;
        }
    }

    return Ok(());
}
