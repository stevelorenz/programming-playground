/// Build script to tell

fn main() {
    // Tell the compiler where to find the shared library for linking.
    println!(r"cargo:rustc-link-search=native=/Users/zuoxiang/dev_projects/libgit2/build");
}
