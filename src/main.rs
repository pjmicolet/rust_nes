mod romfile;
mod cpu;

fn main() {
	let buff : Vec<u8> = romfile::setup_fn();
	let mut theCPU = cpu::CPU::new();
	theCPU.loadRom( buff );
	theCPU.execute();
}
