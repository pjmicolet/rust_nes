use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;

/*
 * Right now I don't need this code
 */
#[allow(dead_code)]
fn read_header( romfile: &mut [u8]) -> [u32;2]
{
	let size_data : u32 = romfile[4] as u32;
	let second : u32 = romfile[5] as u32;
	let data : [u32; 2] = [ size_data * 16384, second * 8192 ];	
	data
}

pub fn setup_fn() -> Vec<u8>
{
    let path = Path::new("/mnt/c/Users/PJ/Coding/rust_nes/src/nestest.nes");
    let display = path.display();

    // Open the path in read-only mode, returns `io::Result<File>`
    let mut file = match File::open(&path) {
        // The `description` method of `io::Error` returns a string that
        // describes the error
        Err(why) => panic!("couldn't open {}: {}", display,
                                                   why.description()),
        Ok(file) => file,
    };

    // Read the file contents into a string, returns `io::Result<usize>`
    //let mut reader = BufReader::new(file);
    //let mut buf = vec![];
    //let data_size = reader.read_until(b'=',&mut buf).expect("Rom file could not be fully read");
	let mut buf = vec![];
	let data_size = file.read_to_end(&mut buf);
    let _info : [u32;2] = read_header( &mut buf );
	buf[16..buf.len()].to_vec()
}
