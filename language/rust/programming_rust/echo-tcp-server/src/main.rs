#![allow(elided_lifetimes_in_paths, dead_code)]

use std::io;
use std::net::TcpListener;
use std::thread::spawn;

fn echo_main(addr: &str) -> io::Result<()> {
    let listener = TcpListener::bind(addr)?;
    println!("Listening on {}", addr);

    loop {
        let (mut stream, addr) = listener.accept()?;
        println!("Connection recevied from {}", addr);

        // Spawn a new thread to handle this client.
        let mut write_stream = stream.try_clone()?;
        spawn(move || {
            io::copy(&mut stream, &mut write_stream).expect("Error happens in the client thread!");
            println!("connection closed!");
        });
    }
}

fn main() {
    let addr = "127.0.0.1:6666".to_string();
    let err_msg = format!("Error! Can not listen on: {}", addr);
    echo_main(&addr).expect(&err_msg);
}
