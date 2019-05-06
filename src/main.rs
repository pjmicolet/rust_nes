use std::env;
mod romfile;
mod cpu;
mod memory;

fn main() {
    let args: Vec<String>  = env::args().collect();
	let buff : Vec<u8> = romfile::setup_fn( &args[1] );
    let mut mem : [ u8; 0x10000 ];
    let mut memory = memory::MemoryController::new();
    mem = [ 0; 0x10000 ];
	let mut the_cpu = cpu::CPU::new( &mut memory );
    let mut debug_path = "";
    let mut debug = false;
    if args.len() > 2
    {
        debug = true;
        debug_path = &args[2];
    }
	if the_cpu.load_rom( buff, debug, debug_path ).is_ok()
    {
        the_cpu.execute();
    }
}
