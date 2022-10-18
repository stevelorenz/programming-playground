#![allow(dead_code, unused_imports, unused_variables)]

mod raw;

use std::ffi::CStr;
use std::os::raw::c_int;

/// Check the return/status code of a C function.
/// Print error and terminate if the status code < 0
/// Otherwise, just return the given status code.
fn check(activity: &str, status: c_int) -> c_int {
    // Error happened!
    if status < 0 {
        unsafe {
            let error = &*raw::giterr_last();
            eprintln!(
                "error while {}: {} ({})",
                activity,
                CStr::from_ptr(error.message).to_string_lossy(),
                error.kclass
            );
            std::process::exit(1);
        }
    }

    return status;
}

use std::ffi::CString;
use std::mem;
use std::os::raw::c_char;
use std::ptr;

unsafe fn show_commit(commit: *const raw::git_commit) {
    let author = raw::git_commit_author(commit);

    let name = CStr::from_ptr((*author).name).to_string_lossy();
    let email = CStr::from_ptr((*author).email).to_string_lossy();
    println!("{} <{}>", name, email);

    let message = raw::git_commit_message(commit);
    println!("{}", CStr::from_ptr(message).to_string_lossy());
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let path: CString;

    if args.len() == 1 {
        path = CString::new(args[0].clone()).expect("path contains invalid characters");
    } else {
        eprintln!("Usage: libgit2-rs PATH");
        std::process::exit(1);
    }

    println!("Start calling unsafe libgit2 functions");
    unsafe {
        check("init libgit2 library", raw::git_libgit2_init());
        let mut repo = ptr::null_mut();
        check(
            "opening repository",
            raw::git_repository_open(&mut repo, path.as_ptr()),
        );

        let c_name = b"HEAD\0".as_ptr() as *const c_char;
        let oid = {
            let mut oid = mem::MaybeUninit::uninit();
            check(
                "looking up HEAD",
                raw::git_reference_name_to_id(oid.as_mut_ptr(), repo, c_name),
            );
            oid.assume_init()
        };

        let mut commit = ptr::null_mut();
        check(
            "looking up commit",
            raw::git_commit_lookup(&mut commit, repo, &oid),
        );

        println!("****** Print the message of the HEAD commit ******");
        show_commit(commit);

        raw::git_repository_free(repo);

        check("shutdown libgit2 library", raw::git_libgit2_shutdown());
    }
}
