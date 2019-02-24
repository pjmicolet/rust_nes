use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;

fn read_header( romfile: &mut [u8])
{
    if romfile[0] == 0
    {
        println!("OKAY");
    }
    else
    {
        println!("It was {}", romfile[0]);
    }
}

pub fn setup_fn()
{
    let path = Path::new("C:/Users/Paul/Coding/Rust/rust_nes/src/smb.nes");
    let display = path.display();

    // Open the path in read-only mode, returns `io::Result<File>`
    let file = match File::open(&path) {
        // The `description` method of `io::Error` returns a string that
        // describes the error
        Err(why) => panic!("couldn't open {}: {}", display,
                                                   why.description()),
        Ok(file) => file,
    };

    // Read the file contents into a string, returns `io::Result<usize>`
    let mut reader = BufReader::new(file);
    let mut buf = vec![];
    let size = reader.read_until(b'=',&mut buf).expect("Rom file could not be fully read");
    read_header( &mut buf );
    println!("I read a byte {}", size);
    println!("WOAH {}",buf[1]);
}
