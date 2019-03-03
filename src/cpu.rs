use std::fmt;

static address_bytes : [u8; 256] =
[ 0,1,0,1,1,1,1,1,0,1,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,
  2,1,0,1,1,1,1,1,0,1,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,
  0,1,0,1,1,1,1,1,0,1,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,
  0,1,0,1,1,1,1,1,0,1,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,
  1,1,0,1,1,1,1,1,0,0,0,0,2,2,2,2,
  1,1,0,0,1,1,1,1,0,2,0,0,0,2,0,0,
  1,1,1,1,1,1,1,1,0,1,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,1,0,0,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,
  1,1,0,1,1,1,1,1,0,1,0,1,2,2,2,2,
  1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2 ];

static addressing_mode : [u8; 256] =
[ 0, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  7, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  0, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 13, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  0, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 10, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  1, 18, 1, 18, 12, 12, 12, 12, 0, 1, 0, 1, 13, 13, 13, 13,
  11, 19, 0, 6, 16, 16, 17, 17, 0, 15, 0, 9, 8, 14, 8, 8,
  1, 5, 1, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 4, 4, 0, 9, 0, 9, 8, 8, 9, 9,
  1, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  1, 5, 1, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7,
  11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8 ];

static names : [&str; 256] =
[ "brk", "ora", "kil", "slo", "nop", "ora", "asl", "slo", "php", "ora", "asl", "anc", "nop", "ora", "asl", "slo",
  "bpl", "ora", "kil", "slo", "nop", "ora", "asl", "slo", "clc", "ora", "nop", "slo", "nop", "ora", "asl", "slo",
  "jsr", "and", "kil", "rla", "bit", "and", "rol", "rla", "plp", "and", "rol", "anc", "bit", "and", "rol", "rla",
  "bmi", "and", "kil", "rla", "nop", "and", "rol", "rla", "sec", "and", "nop", "rla", "nop", "and", "rol", "rla",
  "rti", "eor", "kil", "sre", "nop", "eor", "lsr", "sre", "pha", "eor", "lsr", "alr", "jmp", "eor", "lsr", "sre",
  "bvc", "eor", "kil", "sre", "nop", "eor", "lsr", "sre", "cli", "eor", "nop", "sre", "nop", "eor", "lsr", "sre",
  "rts", "adc", "kil", "rra", "nop", "adc", "ror", "rra", "pla", "adc", "ror", "arr", "jmp", "adc", "ror", "rra",
  "bvs", "adc", "kil", "rra", "nop", "adc", "ror", "rra", "sei", "adc", "nop", "rra", "nop", "adc", "ror", "rra",
  "nop", "sta", "nop", "sax", "sty", "sta", "stx", "sax", "dey", "nop", "txa", "xaa", "sty", "sta", "stx", "sax",
  "bcc", "sta", "kil", "ahx", "sty", "sta", "stx", "sax", "tya", "sta", "txs", "tas", "shy", "sta", "shx", "ahx",
  "ldy", "lda", "ldx", "lax", "ldy", "lda", "ldx", "lax", "tay", "lda", "tax", "lax", "ldy", "lda", "ldx", "lax",
  "bcs", "lda", "kil", "lax", "ldy", "lda", "ldx", "lax", "clv", "lda", "tsx", "las", "ldy", "lda", "ldx", "lax",
  "cpy", "cmp", "nop", "dcp", "cpy", "cmp", "dec", "dcp", "iny", "cmp", "dex", "axs", "cpy", "cmp", "dec", "dcp",
  "bne", "cmp", "kil", "dcp", "nop", "cmp", "dec", "dcp", "cld", "cmp", "nop", "dcp", "nop", "cmp", "dec", "dcp",
  "cpx", "sbc", "nop", "isc", "cpx", "sbc", "inc", "isc", "inx", "sbc", "nop", "sbc", "cpx", "sbc", "inc", "isc",
  "beq", "sbc", "kil", "isc", "nop", "sbc", "inc", "isc", "sed", "sbc", "nop", "isc", "nop", "sbc", "inc", "isc" ];

struct Regs {
	a : u8,
	x : u8,
	y : u8,
	s : u8,
	p : [u8; 8],
	sl : u16,
}

impl fmt::Display for Regs {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let p_flag = self.p[0] | ( self.p[1]  << 1 ) | ( self.p[2] << 2 ) | ( self.p[3] << 3 ) | (self.p[4] << 4 ) | (self.p[5] << 5) | (self.p[6] << 6) | (self.p[7] << 7 ); 
		write!(f, "[ A: {:x}, X: {:x}, Y: {:x}, S: {:x}, P: {:x}, SL: {:x} ]", self.a, self.x, self.y, self.s, p_flag, self.sl)
	}
}

impl Regs {
	fn new() -> Regs {
		Regs {
			sl: 0x0,
			s:	0xFD,
			a:	0x0,
			x: 	0x0,
			y: 	0x0,
			p: [ 0x0, 0x0, 0x1, 0x0, 0x0, 0x1, 0x0, 0x0 ]
		}
	}
}

pub struct CPU {
	pc : u16,
	regs: Regs,
	memory : [u8; 0x10000],
	dma_wait : u8,
	dma_wait_cycles : u8,
	vram_buff : u8,
	cycles : u64,
}

impl CPU {
	pub fn new( ) -> CPU {
		CPU {
			regs: Regs::new(),
			pc: 0x0000,
			cycles: 0,
			dma_wait: 0,
			dma_wait_cycles: 0,
			vram_buff: 0,
			memory: [0; 0x10000],
		}
	}

	pub fn loadRom( &mut self, data : Vec<u8> ) {
		if data.len() == 16384  {
			let base1 = 0x8000;
			let base2 = 0xC000;
			let mut index  = 0;
			for byte in data.iter() {
				self.memory[ base1 + index ] = byte.clone();
				self.memory[ base2 + index ] = *byte;
				println!("{}",byte);
				index = index + 1;
			}
		}
		else {
			println!("{} SIZE ?",data.len());
			let base1 = 0x8000;
			let mut index = 0;
			for byte in data.iter() {
				self.memory[ base1 + index ] = byte.clone();
				index = index + 1;
			}
		}
		let pcPart1 : u16 = self.memory[0xFFFC] as u16;
		let pcPart2 : u16 = self.memory[0xFFFD] as u16;
		self.pc = ( pcPart1 | pcPart2 << 8 );
		self.pc = 0x8000;
	}
	
	pub fn execute(&mut self) {
		while( self.pc < 0xFFFF ) {
			let memIndex : usize = self.pc as usize;
			let index : usize = self.memory[memIndex] as usize;
			self.debugDecode();
			self.nextPc();
		}
	} 

	fn nextPc(&mut self) {
		let memIndex : usize = self.pc as usize;
		let index : usize = self.memory[memIndex] as usize;
		self.pc = self.pc + address_bytes[index] as u16 + 1;	
	}

	fn debugDecode(&mut self) {
		println!("{}", self);
	}
	
	fn dataFetch(&mut self) {
		let inst_size = address_bytes[ self.pc as usize ] + 1;
	}
}	

impl fmt::Display for CPU {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let addressingMode = addressing_mode[self.memory[ self.pc as usize ] as usize ];
		match addressingMode {
			0 => write!(f, "[{:x}]{} {} \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize]),
			1 => write!(f, "[{:x}]{} {} #{:x}\t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			2 | 12 => write!(f, "[{:x}]{} {} ${:x}\t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			3 | 16 => write!(f, "[{:x}]{} {} ${:x},X\t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			4 | 17 => write!(f, "[{:x}]{} {} ${:x},Y \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			5 | 18 => write!(f, "[{:x}]{} {} (${:x}),X \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			6 | 19 => write!(f, "[{:x}]{} {} (${:x}),Y \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			7 | 13=> write!(f, "[{:x}]{} {} ${:x}{:x}\t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
			8 | 14 => write!(f, "[{:x}]{} {} {:x}{:x}, X\t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
			9 | 15 => write!(f, "[{:x}]{} {} {:x}{:x}, Y \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
			10 => write!(f, "[{:x}]{} {} $({:x}{:x}) \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
			11 => write!(f, "[{:x}]{} {} {:x} \t", self.pc, self.regs, names[self.memory[ self.pc as usize] as usize], self.memory[self.pc as usize + 1]),
			_ => write!(f, "Specified wrong format"),
		}
	}
}
