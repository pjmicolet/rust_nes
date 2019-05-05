use std::env;
mod romfile;
mod cpu;

fn main() {
    let args: Vec<String>  = env::args().collect();
	let buff : Vec<u8> = romfile::setup_fn( &args[1] );
	let mut theCPU = cpu::CPU::new();
    let mut debug_path = "";
    let mut debug = false;
    if args.len() > 2
    {
        debug = true;
        debug_path = &args[2];
    }
	theCPU.load_rom( buff, debug, debug_path );
	theCPU.execute();
}
