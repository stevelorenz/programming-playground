#![allow(dead_code, unused_imports, unused_variables)]

mod git;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let path: String;
    if args.len() == 1 {
        path = args[0].clone();
    } else {
        eprintln!("Usage: libgit2-rs PATH");
        std::process::exit(1);
    }

    let repo = git::Repository::open(&path).expect("open the repo with the given path");
    let commit_id = repo.reference_name_to_id("HEAD").expect("map name to OID");

    let commit = repo
        .find_commit(&commit_id)
        .expect("find the commit with the given ID");

    println!("****** Print the HEAD commit of the given git repo path ******");
    println!(
        "{} <{}>\n",
        commit.author().name().unwrap_or("None"),
        commit.author().email().unwrap_or("None")
    );
    println!("{}", commit.message().unwrap_or("None"))
}
