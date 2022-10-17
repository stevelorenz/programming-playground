/// `fingertips` creates an inverted index for a set of text files.

#[allow(dead_code, unused_imports, unused_variables)]
mod index;

use argparse::{ArgumentParser, Collect, StoreTrue};
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver};
use std::thread::{spawn, JoinHandle};

fn expand_filename_arguments(args: Vec<String>) -> io::Result<Vec<PathBuf>> {
    let mut filenames = vec![];
    for arg in args {
        let path = PathBuf::from(arg);
        if path.metadata()?.is_dir() {
            for entry in path.read_dir()? {
                let entry = entry?;
                if entry.file_type()?.is_file() {
                    filenames.push(entry.path());
                }
            }
        } else {
            filenames.push(path);
        }
    }
    Ok(filenames)
}

fn run_single_threaded(documents: Vec<PathBuf>, output_dir: PathBuf) -> io::Result<()> {
    todo!()
}

fn run_pipeline(documents: Vec<PathBuf>, output_dir: PathBuf) -> io::Result<()> {
    todo!()
}

fn run(filenames: Vec<String>, single_threaded: bool) -> io::Result<()> {
    let output_dir = PathBuf::from(".");
    let documents = expand_filename_arguments(filenames)?;

    if single_threaded {
        println!("Run with a single thread!");
        return run_single_threaded(documents, output_dir);
    } else {
        println!("Run with multiple threads in pipeline!");
        return run_pipeline(documents, output_dir);
    }
}

fn main() {
    let mut single_threaded = false;
    let mut filenames = vec![];

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Make an inverted index for searching documents");
        ap.refer(&mut single_threaded).add_option(
            &["-1", "--single-threaded"],
            StoreTrue,
            "Do all the work in a single thread.",
        );
        ap.refer(&mut filenames)
            .add_argument("filenames", Collect, "Names of files to index.");
        ap.parse_args_or_exit();
    }

    match run(filenames, single_threaded) {
        Ok(()) => (),
        Err(err) => eprintln!("error:{}", err),
    }
}
