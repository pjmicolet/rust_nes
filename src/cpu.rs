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

struct StatusReg {
    carry : u8,
    zero : u8,
    interrupt : u8,
    decimal : u8,
    s1 : u8,
    s2 : u8,
    overflow : u8,
    negative : u8,
}

struct Regs {
	a : u8,
	x : u8,
	y : u8,
	s : u8,
	p : StatusReg,
	sl : u16,
}

impl fmt::Display for Regs {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let p_flag = self.p.carry | ( self.p.zero  << 1 ) | ( self.p.interrupt << 2 ) | ( self.p.decimal << 3 ) | (self.p.s1 << 4 ) | (self.p.s2 << 5) | (self.p.overflow << 6) | (self.p.negative << 7 ); 
		write!(f, "[ A: {:x}, X: {:x}, Y: {:x}, S: {:x}, P: {:x}, SL: {:x} ]", self.a, self.x, self.y, self.s, p_flag, self.sl)
	}
}

impl StatusReg {
    fn new() -> StatusReg {
        StatusReg {
            carry : 0x0,
            zero : 0x0,
            interrupt : 0x1,
            decimal : 0x0,
            s1 : 0x0,
            s2 : 0x1,
            overflow : 0x0,
            negative : 0x0,
        }
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
			p:  StatusReg::new(),
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

macro_rules! memAt {
    ( $x:ident, $( $y:expr ),+ ) => {
        {
            let mut data = 0;
            $(
                data = $x.memory[ ( ( data + $y as u16 ) as usize ) ] as u16;
             )+
            data as u16
        }
    };
}

macro_rules! composeAddress {
    ( $part_one:expr, $part_two:expr ) =>
        {
            ( ( $part_one as u16) << 8 ) | $part_two as u16
        };
}

macro_rules! isNeg {
    ( $byte:expr ) => {
        ( ( $byte & 0x80 ) == 0x80 ) as u8
    };
}

macro_rules! isZer {
    ( $bytes:expr ) => {
        ( $bytes == 0 ) as u8
    };
}

macro_rules! composeData {
    ( $self:expr, $addr1:expr, $addr2:expr ) => {
        ( $self.memory[ ( $addr1 ) as usize ] as u16 ) << 8 | $self.memory[ ( $addr2 ) as usize ] as u16
    };
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
            println!("{} index", index );
		}
		let pcPart1 : u16 = self.memory[0xFFFC] as u16;
		let pcPart2 : u16 = self.memory[0xFFFD] as u16;
		self.pc = ( pcPart1 | pcPart2 << 8 );
		self.pc = 0xC000;
        println!("What {:x} ", 0x8000 + data.len() );
	}
	
	pub fn execute(&mut self) {
		while( self.pc < 0xFFFF ) {
			let memIndex : usize = self.pc as usize;
			let index : usize = self.memory[memIndex] as usize;
			self.debugDecode();
            self.stepOnce();
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

    fn stepOnce(&mut self) {
        let instruction_opcode = memAt!( self, self.pc );
        match instruction_opcode {
            0x4C | 0x6 => { self.jmp(); return },
            0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => self.lda(),
            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(),
            0x86 | 0x96 | 0x8E => self.stx(),
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => self.nop(),
            0x20 => { self.jsr(); return },
            _ => println!("NO"),
        }
        self.nextPc();
    }
	
	fn dataFetch(&mut self) -> u16 {
		let addressingMode = addressing_mode[ self.memory[ self.pc as usize ] as usize ];
		match addressingMode {
            0 => return 0,
            1 => return memAt!( self, self.pc+1 ),
            2 => return memAt!( self, self.pc+1, 0 ), 
            3 => return memAt!( self, self.pc+1, self.regs.x ), 
            4 => return memAt!( self, self.pc+1, self.regs.y ),
            5 => { 
                    let address = composeAddress!( memAt!( self, self.pc+1, self.regs.x + 1 ), memAt!( self, self.pc+1, self.regs.x ) );
                    return memAt!( self, address ) 
                 },
            6 => {
                    let address = composeAddress!( memAt!( self, self.pc+1, 1 ), memAt!( self, self.pc+1 ) );
                    return memAt!( self, address )
                 },
            7 => {
                    let address = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1 ) );
                    return memAt!( self, address )
                 },
            8 => {
                    let address = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1, self.regs.x ) );
                    return memAt!( self, address )
                 },
            9 => {
                    let address = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1, self.regs.y ) );
                    return memAt!( self, address )
                 },
            10 => {
                    let address1 = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1 ) );
                    let address2 = composeAddress!( address1 & 0xFF00, address1 & 0x00FF );
                    return composeAddress!( memAt!( self, address2 ), memAt!( self, address1 ) )
                  },
            11 => return memAt!( self, self.pc + 1 ),
            12 => return memAt!( self, self.pc + 1 ),
            13 => return composeAddress!( memAt!( self, self.pc + 2 ), memAt!( self, self.pc+ 1 ) ),
            14 => return composeAddress!( memAt!( self, self.pc + 2 ), memAt!( self, self.pc+ 1, self.regs.x ) ),
            15 => return composeAddress!( memAt!( self, self.pc + 2 ), memAt!( self, self.pc+ 1, self.regs.y ) ),
            16 => return memAt!( self, self.pc + 1, self.regs.x ),
            17 => return memAt!( self, self.pc + 1, self.regs.y ),
            18 => return composeAddress!( memAt!( self, self.pc +1, self.regs.x+1), memAt!( self, self.pc+1,self.regs.x ) ),
            19 => return composeAddress!( memAt!( self, self.pc +1 ), memAt!( self, self.pc+1 ) ),
            _ => return 0,
		}
	}

    fn jmp( &mut self ) {
        let pc = self.pc;
        let address = self.dataFetch();
        self.pc = address;
        self.cycles += 1;
    }

    fn lda( &mut self ) {
        let data = self.dataFetch();
        self.regs.a = data as u8;
        self.cycles += 2;
    }

    fn ldx( &mut self ) {
        let data = self.dataFetch();
        self.regs.x = data as u8;
        self.regs.p.zero = isZer!( self.regs.x );
        self.regs.p.negative = isNeg!( self.regs.x );
        self.cycles += 2;
    }

    fn stx( &mut self ) {
        let memory_address = self.dataFetch() as usize;
        self.memory[ memory_address ] = self.regs.x;
        self.cycles += 2;
    }

    fn jsr( &mut self ) {
        let next_pc = self.pc + 2;
        let current_pc = self.pc;
        let address1 = ( 0x100 | self.regs.s as u16 ) as usize;
        let address2 = ( 0x100 | ( self.regs.s - 1 ) as u16 ) as usize;
        self.memory[ address1 ] = ( next_pc >> 8 ) as u8;
        self.memory[ address2 ] = ( next_pc & 0x00FF ) as u8;
        self.regs.s = self.regs.s - 2;
        self.pc = composeData!( self, current_pc + 2, current_pc + 1 );
        self.cycles += 2;
    }

    fn nop( &mut self ) {
        self.cycles += 2;
    }

    fn sec( &mut self ) {
        self.regs.p.carry = 1;
        self.cycles += 2;
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
