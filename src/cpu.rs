extern crate hex;
use std::num::Wrapping;
use std::fmt;
use std::path::Path;
use std::fs::File;
use std::io::{BufRead, BufReader, Result};

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

static ADDRESSING_MODE : [u8; 256] =
[ 0, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7, 11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  7, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7, 11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  0, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 13, 7, 7, 7, 11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  0, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 10, 7, 7, 7, 11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  1, 18, 1, 18, 12, 12, 12, 12, 0, 1, 0, 1, 13, 13, 13, 13, 11, 19, 0, 6, 16, 16, 17, 17, 0, 15, 0, 9, 8, 14, 8, 8,
  1, 5, 1, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7, 11, 6, 0, 6, 3, 3, 4, 4, 0, 9, 0, 9, 8, 8, 9, 9,
  1, 5, 0, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7, 11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8,
  1, 5, 1, 5, 2, 2, 2, 2, 0, 1, 0, 1, 7, 7, 7, 7, 11, 6, 0, 6, 3, 3, 3, 3, 0, 9, 0, 9, 8, 8, 8, 8 ];

static NAMES : [&str; 256] =
[ "BRK", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "PHP", "ORA", "ASL", "ANC", "NOP", "ORA", "ASL", "SLO", "BPL", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "CLC", "ORA", "NOP", "SLO", "NOP", "ORA", "ASL", "SLO",
"JSR", "AND", "KIL", "RLA", "BIT", "AND", "ROL", "RLA", "PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "RLA", "BMI", "AND", "KIL", "RLA", "NOP", "AND", "ROL", "RLA", "SEC", "AND", "NOP", "RLA", "NOP", "AND", "ROL", "RLA",
"RTI", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "SRE", "BVC", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "CLI", "EOR", "NOP", "SRE", "NOP", "EOR", "LSR", "SRE",
"RTS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA", "BVS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "SEI", "ADC", "NOP", "RRA", "NOP", "ADC", "ROR", "RRA",
"NOP", "STA", "NOP", "SAX", "STY", "STA", "STX", "SAX", "DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "SAX", "BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "SAX", "TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX",
"LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX", "TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "LAX", "BCS", "LDA", "KIL", "LAX", "LDY", "LDA", "LDX", "LAX", "CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "LAX",
"CPY", "CMP", "NOP", "DCP", "CPY", "CMP", "DEC", "DCP", "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP", "BNE", "CMP", "KIL", "DCP", "NOP", "CMP", "DEC", "DCP", "CLD", "CMP", "NOP", "DCP", "NOP", "CMP", "DEC", "DCP",
"CPX", "SBC", "NOP", "ISB", "CPX", "SBC", "INC", "ISB", "INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISB", "BEQ", "SBC", "KIL", "ISB", "NOP", "SBC", "INC", "ISB", "SED", "SBC", "NOP", "ISB", "NOP", "SBC", "INC", "ISB" ];

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
}

struct DebugInfo {
    instr : String,
    cycles : u64,
    regs : Regs,
    pc : u16,
}

impl DebugInfo {
    fn new() -> DebugInfo {
        DebugInfo
        {
            instr : String::from( "" ),
            cycles : 0,
            regs : Regs::new(),
            pc : 0,
        }
    }

    fn new_args( new_pc : u16, new_instr : String, new_regs : Vec<u16>, new_cycles : u64 ) -> DebugInfo {
        let mut db = DebugInfo
        {
            pc : new_pc,
            instr : new_instr,
            regs : Regs::new(),
            cycles : new_cycles,
        };

        let mut p_reg = vec![0; 8];
        db.regs.a = new_regs[0] as u8;
        db.regs.x = new_regs[1] as u8;
        db.regs.y = new_regs[2] as u8;

        for i in 0..8 {
            p_reg[i] = ( ( new_regs[3] >> i ) as u8 ) & 0x1;
        }
        db.regs.p.carry = p_reg[0];
        db.regs.p.zero = p_reg[1];
        db.regs.p.interrupt = p_reg[2];
        db.regs.p.decimal = p_reg[3];
        db.regs.p.s1 = p_reg[4];
        db.regs.p.s2 = p_reg[5];
        db.regs.p.overflow = p_reg[6];
        db.regs.p.negative = p_reg[7];

        db.regs.s = new_regs[4] as u8;
        db
    }
}

impl fmt::Display for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let p_flag = self.p.to_int(); 
        write!(f, "[ A: {:x}, X: {:x}, Y: {:x}, S: {:x}, P: {:x} ]", self.a, self.x, self.y, self.s, p_flag)
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

    pub fn to_int( & self ) -> u8 {
      self.negative << 7 | self.overflow << 6 | self.s2 << 5 | self.s1 << 4 | 
          self.decimal << 3 | self.interrupt << 2 | self.zero << 1 | self.carry 
    }

    pub fn from_int( & self, data : u8 ) -> StatusReg {
        StatusReg {
            carry : data & 0x1,
            zero : ( data & 0x2 ) >> 1,
            interrupt : ( data & 0x4 ) >> 2,
            decimal : ( data & 0x8 ) >> 3,
            s1 : ( data & 0x10 ) >> 4,
            s2 : ( data & 0x20 ) >> 5,
            overflow : ( data & 0x40 ) >> 6,
            negative : ( data & 0x80 ) >> 7,
        }
    }
}

impl PartialEq for StatusReg {
    fn eq(&self, other: &StatusReg) -> bool {
        self.carry == other.carry &&
            self.zero == other.zero &&
            self.interrupt == other.interrupt &&
            self.decimal == other.decimal &&
            self.s1 == other.s1 &&
            self.s2 == other.s2 &&
            self.overflow == other.overflow &&
            self.negative == other.negative
    }
}

impl Eq for StatusReg {}

impl Regs {
    fn new() -> Regs {
        Regs {
            s:    0xFD,
            a:    0x0,
            x:     0x0,
            y:     0x0,
            p:  StatusReg::new(),
        }
    }
}

impl PartialEq for Regs {
    fn eq(&self, other: &Regs) -> bool {
        self.s == other.s &&
            self.a == other.a &&
            self.x == other.x &&
            self.y == other.y &&
            self.p == other.p
    }
}

impl Eq for Regs {}

pub struct CPU {
    pc : u16,
    regs: Regs,
    memory : [u8; 0x10000],
    dma_wait : u8,
    dma_wait_cycles : u8,
    vram_buff : u8,
    cycles : u64,
    sl : u16,
    debug_data : Vec< DebugInfo >,
    debug_iter : usize
}

macro_rules! memAt {
    ( $x:ident, $( $y:expr ),+ ) => {
        {
            let mut data = 0;
            $(
                let address = ovop!(+=, u16, data as u16, $y as u16 );
                data = $x.memory[ ( address as usize ) ] as u16;
             )+
            data as u16
        }
    };
}

macro_rules! memAtZp {
    ( $x:ident, $pc:expr, $( $y:expr ),+ ) => {
        {
            let mut data = $x.memory[ $pc as usize ] as u16;
            $(
                let address = ovop!(+=, u16, data as u16, $y as u16 );
                data = $x.memory[ ( address & 0x00FF ) as usize ] as u16;
             )+
            data as u16
        }
    };
}

// This conversion is terrible
macro_rules! branchOn {
    ( $self:ident, $reg:ident, $val:expr ) => {
        let displacement = memAt!( $self, $self.pc + 1 ) as u8;
        if $self.regs.p.$reg == $val 
        { 
            $self.cycles += 3;
            if ( ( displacement as i8 as i16 + 0xFF & ( $self.pc as i16 + 2 ) ) & 0x100 ) != 0 { // we cross page boundary 
                $self.cycles += 2;
            }

            $self.pc = ( $self.pc as i16 + displacement as i8 as i16 + 2 ) as u16;
        }
        else
        {
            $self.cycles += 2;
            $self.next_pc();
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

macro_rules! set_zn {
    ( $self:expr, $data:expr ) => {
        $self.regs.p.zero = isZer!( $data );
        $self.regs.p.negative = isNeg!( $data );
    };
}

macro_rules! composeData {
    ( $self:expr, $addr1:expr, $addr2:expr ) => {
        ( $self.memory[ ( $addr1 ) as usize ] as u16 ) << 8 | $self.memory[ ( $addr2 ) as usize ] as u16
    };
}

macro_rules! getDebugReg {
    ( $line:expr, $number:expr, $reg:expr ) => {
         match hex::decode( $line[$number].split(':').collect::<Vec<_>>()[1] ) {
                    Ok( v ) => v,
                    Err( e ) => panic!("Messed up getting {}", $reg),
                }[0];
    };
}

macro_rules! name {
    ( $self:expr ) => {
        NAMES[$self.memory[ $self.pc as usize ] as usize ]
    };
}

macro_rules! debugName {
    ( $self:expr ) => {
        $self.debug_data[ $self.debug_iter ].instr
    };
}

macro_rules! overflow {
    ( $data1:expr, $data2:expr, $data3:expr ) => {
        ( !( ( $data1 ^ $data2 ) & 0x80 ) & ( ( $data1 ^ $data3 ) & 0x80 ) ) >> 7
    }
}

macro_rules! overflowsbc {
    ( $data1:expr, $data2:expr, $data3:expr ) => {
        ( ( ( $data1 ^ $data2 ) & 0x80 ) & ( ( $data1 ^ $data3 ) & 0x80 ) ) >> 7
    }
}

macro_rules! ovop {
    ( $op:tt, $type:tt, $firstitem:expr, $( $item:expr ),+ ) =>  {
        {
            let mut sum = Wrapping( $firstitem );
            $(
                sum  $op Wrapping( $item );
             )+
            sum.0 as $type
        }
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
            sl : 0,
            debug_data : Vec::new(),
            debug_iter : 0
        }
    }

    pub fn load_rom( &mut self, data : Vec<u8>, debug : bool, debug_path : &str ) -> Result<()> {
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
        self.pc = pcPart1 | pcPart2 << 8;
        self.pc = 0xC000;
        println!("What {:x} ", 0x8000 + data.len() );

        if debug {
            let path = Path::new( debug_path );
            // Open the path in read-only mode, returns `io::Result<File>`
            let file = File::open(path)?;
            for line in BufReader::new( file ).lines() {
                let newline = line?;
                let split_line = newline.split(' ').collect::<Vec<_>>();
                let pc_parts = match hex::decode( split_line[0] ) {
                    Ok( v ) => v,
                    Err( e ) => panic!("That don't work"),
                };
                let pc = ( pc_parts[0] as u16 ) << 8 | pc_parts[1] as u16;

                let instr = String::from( split_line[1] );
                let mut regs = vec![0 ; 5 ];

                regs[0] = getDebugReg!( split_line, 2, "A" ) as u16; 
                regs[1] = getDebugReg!( split_line, 3, "X" ) as u16;
                regs[2] = getDebugReg!( split_line, 4, "Y" ) as u16;
                regs[3] = getDebugReg!( split_line, 5, "P" ) as u16;
                regs[4] = getDebugReg!( split_line, 6, "SP" ) as u16;
                
                let cycle = split_line[8].split(":").collect::<Vec<_>>()[1].parse::<u64>().unwrap();
                self.debug_data.push( DebugInfo::new_args( pc, instr, regs, cycle ) );

            }
        }
        Ok(())
    }
    
    pub fn debugValidate( & mut self ){
        if self.pc != self.debug_data[ self.debug_iter ].pc {
            panic!( "PC is different Got: {:x} Expected: {:x}", self.pc, self.debug_data[ self.debug_iter].pc );
        }
        if name!( self ) != debugName!( self ) {
            panic!( "Hey this isn't right Got: {} Expected: {}", name!( self ) , debugName!( self ) );
        }
        if self.regs != self.debug_data[ self.debug_iter ].regs {
            panic!( "regs\nGot:{:x}{}\nExpected:{:x}{}", self.pc, self.regs,self.debug_data[self.debug_iter].pc, self.debug_data[ self.debug_iter].regs );
        }
    }

    pub fn execute(&mut self) {
        while( self.pc < 0xFFFF ) {
            if self.debug_data.len() > 1 {
                self.debugValidate();
                self.debug_iter = self.debug_iter + 1;
            }
            self.debug_decode();
            self.step_once();
        }
    } 

    fn next_pc(&mut self) {
        let mem_index : usize = self.pc as usize;
        let index : usize = self.memory[mem_index] as usize;
        self.pc = self.pc + address_bytes[index] as u16 + 1;    
    }

    fn debug_decode(&mut self) {
        println!("{}", self);
    }

    fn step_once(&mut self) {
        let instruction_opcode = memAt!( self, self.pc );
        match instruction_opcode {
            0x08 => self.php(),
            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(),
            0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.cmp(),
            0x4C | 0x6C => { self.jmp(); return },
            0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => self.lda(),
            0xA3 | 0xA7 | 0xAF | 0xB3 | 0xB7 | 0xBF => self.lax(),
            0x83 | 0x87 | 0x8F | 0x97 => self.sax(),
            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(),
            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(),
            0x84 | 0x94 | 0x8C => self.sty(),
            0x86 | 0x96 | 0x8E => self.stx(),
            0x04 | 0x0C | 0x14 | 0x1C | 0x34 | 0x3C | 0x44 | 0x80 | 0x54 | 0x5C | 0x7C | 0xDC | 0xFC | 0x64 | 0x74 | 0xD4 | 0xF4 | 0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => self.nop(),
            0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => self.sta(),
            0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(),
            0xC0 | 0xC4 | 0xCC => self.cpy(),
            0xE0 | 0xE4 | 0xEC => self.cpx(),
            0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(),
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => self.adc(),
            0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 | 0xEB => self.sbc(),
            0x2A | 0x26 | 0x36 | 0x2E | 0x3E => self.rol(),
            0x6A | 0x66 | 0x76 | 0x6E | 0x7E => self.ror(),
            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => self.asl(),
            0x4A | 0x46 | 0x56 | 0x4E | 0x5E => self.lsr(),
            0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(),
            0xC3 | 0xC7 | 0xCF | 0xD3 | 0xD7 | 0xDB | 0xDF => self.dcp(),
            0xE3 | 0xE7 | 0xEF | 0xF3 | 0xF7 | 0xFB | 0xFF => self.isc(),
            0x03 | 0x07 | 0x0F | 0x13 | 0x17 | 0x1B | 0x1F => self.slo(),
            0x23 | 0x27 | 0x2F | 0x33 | 0x37 | 0x3B | 0x3F => self.rla(),
            0x43 | 0x47 | 0x4F | 0x53 | 0x57 | 0x5B | 0x5F => self.sre(),
            0x63 | 0x67 | 0x6F | 0x73 | 0x77 | 0x7B | 0x7F => self.rra(),
            0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(),
            0x24 | 0x2C => self.bit(),
            0x68 => self.pla(),
            0x28 => self.plp(),
            0x38 => self.sec(),
            0x48 => self.pha(),
            0x78 => self.sei(),
            0x88 => self.dey(),
            0x8A => self.txa(),
            0x98 => self.tya(),
            0x9A => self.txs(),
            0xAA => self.tax(),
            0xA8 => self.tay(),
            0xBA => self.tsx(),
            0xCA => self.dex(),
            0xC8 => self.iny(),
            0xD8 => self.cld(),
            0xE8 => self.inx(),
            0xB8 => self.clv(),
            0xF8 => self.sed(),
            0x20 => { self.jsr(); return },
            0x10 => { self.bpl(); return },
            0x30 => { self.bmi(); return },
            0x50 => { self.bvc(); return },
            0x70 => { self.bvs(); return },
            0xB0 => { self.bcs(); return },
            0x90 => { self.bcc(); return },
            0xF0 => { self.beq(); return },
            0xD0 => { self.bne(); return },
            0x40 => { self.rti(); return },
            0x60 => { self.rts(); return },
            0x18 => self.clc(),
            _ => panic!( "Hey you haven't implemented {}", name!(self)),
        }
        self.next_pc();
    }
    
    fn dataFetch(&mut self, get_data : bool ) -> u16 {
        let addressing_mode = ADDRESSING_MODE[ self.memory[ self.pc as usize ] as usize ];
        match addressing_mode {
            0 => return 0,
            1 => return memAt!( self, self.pc+1 ),
            2 => { if get_data { return memAt!( self, self.pc+1, 0 ) } else { return memAt!( self, self.pc+1 ) } }, 
            3 => { if get_data { return memAtZp!( self, self.pc+1, self.regs.x ) } else { return ( memAt!(self, self.pc+1 ) + self.regs.x as u16 ) & 0x00FF } }, 
            4 => { if get_data { return memAtZp!( self, self.pc+1, self.regs.y ) } else { return ( memAt!(self, self.pc+1 ) + self.regs.y as u16 ) & 0x00FF } },
            5 => { 
                    let address = composeAddress!( memAtZp!( self, self.pc + 1, self.regs.x + 1 ), memAtZp!( self, self.pc+1, self.regs.x ) );
                    if get_data { return memAt!( self, address ) } else { return address } 
                 },
            6 => {
                    let address = composeAddress!( memAtZp!( self, self.pc+1, 1 ), memAt!( self, self.pc+1, 0) );
                    let address2 = ovop!( +=, u16, address, self.regs.y as u16 );
                    if get_data { return memAt!( self, address2 ) } else { return address2 }
                 },
            7 => {
                    let address = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1 ) );
                    if get_data { return memAt!( self, address ) } else { return address }
                 },
            8 => {
                    let address = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1 ) );
                    let address2 = ovop!( +=, u16, address, self.regs.x as u16 );
                    if get_data { return memAt!( self, address2 ) } else { return address2 }
                 },
            9 => {
                    let address = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1 ) );
                    let address2 = ovop!( +=, u16, address, self.regs.y as u16 );
                    if get_data { return memAt!( self, address2 ) } else { return address2 } 
                 },
            10 => {
                    let address1 = composeAddress!( memAt!( self, self.pc+2 ), memAt!( self, self.pc+1 ) );
                    let address2 = address1 & 0xFF00 | (address1+1) & 0x00FF;
                    return composeAddress!( memAt!( self, address2 ), memAt!( self, address1 ) )
                  },
            11 => return memAt!( self, self.pc + 1 ),
            12 => return memAt!( self, self.pc + 1 ),
            13 => return composeAddress!( memAt!( self, self.pc + 2 ), memAt!( self, self.pc+ 1 ) ),
            14 => return composeAddress!( memAt!( self, self.pc + 2 ), memAt!( self, self.pc+ 1 ) )  + self.regs.x as u16,
            15 => return composeAddress!( memAt!( self, self.pc + 2 ), memAt!( self, self.pc+ 1 ) ) + self.regs.y as u16,
            16 => return ( memAt!( self, self.pc + 1 ) + self.regs.x as u16 ) & 0x00FF,
            17 => return ( memAt!( self, self.pc + 1 ) + self.regs.y as u16 ) & 0x00FF,
            18 => return composeAddress!( memAtZp!( self, self.pc +1, self.regs.x+1), memAtZp!( self, self.pc+1,self.regs.x ) ),
            19 => return composeAddress!( memAtZp!( self, self.pc +1, 1), memAt!( self, self.pc+1, 0 ) ) + self.regs.y as u16,
            _ => return 0,
        }
    }

    fn jmp( &mut self ) {
        let pc = self.pc;
        let address = self.dataFetch(true);
        self.pc = address;
        self.cycles += 1;
    }

    fn lda( &mut self ) {
        let data = self.dataFetch(true);
        self.regs.a = data as u8;
        set_zn!( self, self.regs.a );
        self.cycles += 2;
    }

    fn ldx( &mut self ) {
        let data = self.dataFetch(true);
        self.regs.x = data as u8;
        set_zn!( self, self.regs.x );
        self.cycles += 2;
    }

    fn ldy( &mut self ) {
        let data = self.dataFetch(true);
        self.regs.y = data as u8;
        self.regs.p.zero = isZer!( self.regs.y );
        self.regs.p.negative = isNeg!( self.regs.y );
        self.cycles += 2;
    }

    fn stx( &mut self ) {
        let memory_address = self.dataFetch(true) as usize;
        self.memory[ memory_address ] = self.regs.x;
        self.cycles += 2;
    }

    fn sty( &mut self ) {
        let memory_address = self.dataFetch(true) as usize;
        self.memory[ memory_address ] = self.regs.y;
        self.cycles += 2;
    }

    fn sta( &mut self ) {
        let memory_address = self.dataFetch(true) as usize;
        self.memory[ memory_address ] = self.regs.a;
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

    fn clc( &mut self )
    {
        self.regs.p.carry = 0;
        self.cycles += 2;
    }

    fn clv( &mut self )
    {
        self.regs.p.overflow = 0;
        self.cycles += 2;
    }

    fn cld( &mut self ) {
        self.regs.p.decimal = 0;
        self.cycles += 2;
    }

    fn bcs( &mut self ) {
        branchOn!( self, carry, 1 );
    }
    fn bcc( &mut self ) {
        branchOn!( self, carry, 0 );
    }

    fn beq( &mut self ) {
        branchOn!( self, zero, 1 );
    }

    fn bne( &mut self ) {
        branchOn!( self, zero, 0 );
    }

    fn bvs( &mut self ) {
        branchOn!( self, overflow, 1 );
    }

    fn bvc( &mut self ) {
        branchOn!( self, overflow, 0 );
    }

    fn bpl( &mut self ) {
        branchOn!( self, negative, 0 );
    }

    fn bmi( &mut self ) {
        branchOn!( self, negative, 1 );
    }

    fn rts( &mut self ) {
        let pc = composeData!( self, 0x100 | ( self.regs.s + 2 ) as u16, 0x100 | ( self.regs.s + 1 ) as u16 );
        self.pc = pc + 1;
        self.regs.s += 2;
        self.cycles += 6;
    }

    fn bit( &mut self ) {
        let bit_data = self.dataFetch(true) as u8;
        let a_anded = bit_data & self.regs.a;
        self.regs.p.zero = isZer!( a_anded );
        self.regs.p.overflow = ( ( bit_data & 0x40 ) == 0x40 ) as u8;
        self.regs.p.negative = isNeg!( bit_data );
        self.cycles += 2;
    }

    fn sei( &mut self ) {
        self.regs.p.interrupt = 1;
        self.cycles += 2;
    }

    fn php( &mut self ) {
        let p_int = self.regs.p.to_int();
        self.memory[ ( 0x100 | self.regs.s as u16 ) as usize ] = ( p_int | 0x1 << 4 ) as u8;
        self.regs.s -= 1;
        self.cycles += 3;
    }

    fn pha( &mut self ) {
        self.memory[ 0x100 | (self.regs.s as u16) as usize ] = self.regs.a;
        self.regs.s -= 1;
        self.cycles += 3;
    }

    fn plp( &mut self ) {
        let data = memAt!( self, ( 0x100 | self.regs.s as u16 ) + 1 ) as u8;
        self.regs.p = self.regs.p.from_int( data ); 
        self.regs.p.s1 = 0;
        self.regs.p.s2 = 1;
        self.regs.s += 1;
        self.cycles += 4;
    }

    fn adc( &mut self ) {
        let data = self.dataFetch(true) as u8;
       // let temp = data as u16 + self.regs.a as u16 + self.regs.p.carry as u16;
        let temp = ovop!( +=, u16, data as u16 , self.regs.a as u16 , self.regs.p.carry as u16 );

        self.regs.p.carry = ( 0x100 & temp == 0x100 ) as u8;
        self.regs.p.zero = isZer!( temp & 0xFF );
        self.regs.p.negative = isNeg!( temp );
        self.regs.p.overflow = overflow!( self.regs.a, data, temp as u8 );
        self.regs.a = ( temp & 0xFF ) as u8;
        self.cycles += 2;
    }

    fn sbc( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let temp = ovop!( -=, u16, self.regs.a as i16, data as i16, ( 1 - self.regs.p.carry as i16 ) );

        self.regs.p.carry = ( 0x100 & temp != 0x100 ) as u8;
        self.regs.p.zero = isZer!( temp & 0xFF );
        self.regs.p.negative = isNeg!( temp );
        self.regs.p.overflow = overflowsbc!( self.regs.a, data, temp as u8 );
        self.regs.a = ( temp & 0xFF ) as u8;
        self.cycles += 2;
    }

    fn ora( &mut self ) {
        self.regs.a |= self.dataFetch(true) as u8;
        self.regs.p.zero = isZer!( self.regs.a );
        self.regs.p.negative = isNeg!( self.regs.a );
        self.cycles += 2;
    }

    fn iny( &mut self ) {
        self.regs.y = ovop!( +=, u8, self.regs.y, 1 );
        self.regs.p.zero = isZer!( self.regs.y );
        self.regs.p.negative = isNeg!( self.regs.y );
        self.cycles += 2;
    }

    fn dey( &mut self ) {
        self.regs.y = ovop!( -=, u8, self.regs.y, 1 );
        self.regs.p.zero = isZer!( self.regs.y );
        self.regs.p.negative = isNeg!( self.regs.y );
        self.cycles += 2;
    }

    fn dex( &mut self ) {
        self.regs.x = ovop!( -=, u8, self.regs.x, 1 );
        self.regs.p.zero = isZer!( self.regs.x );
        self.regs.p.negative = isNeg!( self.regs.x );
        self.cycles += 2;
    }

    fn inx( &mut self ) {
        self.regs.x = ovop!( +=, u8, self.regs.x, 1 );
        self.regs.p.zero = isZer!( self.regs.x );
        self.regs.p.negative = isNeg!( self.regs.x );
        self.cycles += 2;
    }

    fn eor( &mut self ) {
        self.regs.a ^= self.dataFetch(true) as u8;
        self.regs.p.zero = isZer!( self.regs.a );
        self.regs.p.negative = isNeg!( self.regs.a );
        self.cycles += 2;
    }

    fn cpy( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let compare = ( self.regs.y as i16 - data as i16 ) as u8; // this is to aovid any kind of issue with u8 overflow / undeflow

        self.regs.p.carry = if self.regs.y >= data { 1 } else { 0 };
        self.regs.p.zero = if self.regs.y == data { 1 } else { 0 };
        self.regs.p.negative = isNeg!( compare );
        self.cycles += 2;
    }

    fn cpx( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let compare = ( self.regs.x as i16 - data as i16 ) as u8; // this is to aovid any kind of issue with u8 overflow / undeflow

        self.regs.p.carry = if self.regs.x >= data { 1 } else { 0 };
        self.regs.p.zero = if self.regs.x == data { 1 } else { 0 };
        self.regs.p.negative = isNeg!( compare );
        self.cycles += 2;
    }

    fn tay( &mut self ) {
        self.regs.y = self.regs.a;

        self.regs.p.zero = isZer!( self.regs.y );
        self.regs.p.negative = isNeg!( self.regs.y );
        self.cycles += 2;
    }

    fn tya( &mut self ) {
        self.regs.a = self.regs.y;

        self.regs.p.zero = isZer!( self.regs.a );
        self.regs.p.negative = isNeg!( self.regs.a );
        self.cycles += 2;
    }

    fn tax( &mut self ) {
        self.regs.x = self.regs.a;

        self.regs.p.zero = isZer!( self.regs.x );
        self.regs.p.negative = isNeg!( self.regs.x );
        self.cycles += 2;
    }

    fn txa( &mut self ) {
        self.regs.a = self.regs.x;

        self.regs.p.zero = isZer!( self.regs.a );
        self.regs.p.negative = isNeg!( self.regs.a );
        self.cycles += 2;
    }

    fn tsx( &mut self ) {
        self.regs.x = self.regs.s;

        self.regs.p.zero = isZer!( self.regs.x );
        self.regs.p.negative = isNeg!( self.regs.x );
        self.cycles += 2;
    }

    fn txs( &mut self ) {
        self.regs.s = self.regs.x;

        self.cycles += 2;
    }

    fn rti( &mut self ) {
        let pc = composeData!( self, 0x100 | ( self.regs.s + 3 ) as u16, 0x100 | ( self.regs.s + 2 ) as u16 );
        self.regs.p = self.regs.p.from_int( memAt!( self, ( 0x100 | ( self.regs.s + 1 ) as u16 ) ) as u8 );
        self.pc = pc;
        self.regs.s += 3;
        self.cycles += 6;
        self.regs.p.s2 = 1;
    }

    fn lsr( &mut self ) {
        if memAt!( self, self.pc ) == 0x4a {
            self.regs.p.carry = self.regs.a & 0x1; // check this first
            self.regs.a = self.regs.a >> 1;
            self.cycles += 2;
            self.regs.p.zero = isZer!( self.regs.a );
            self.regs.p.negative = isNeg!( self.regs.a );
            self.cycles += 2;
        }
        else {
            let data = self.dataFetch(true);
            let address = self.dataFetch(false);
            self.regs.p.carry = ( data as u8 ) & 0x1;

            let shifted_data = ( data as u8 ) >> 1;
            set_zn!(self, shifted_data);
            self.memory[ address as usize ] = shifted_data;
            self.cycles += 4;
        }
    }

    fn asl( &mut self ) {
        if memAt!( self, self.pc ) == 0x0a {
            self.regs.p.carry = if ( self.regs.a & 0x80 ) == 0x80 { 1 } else { 0 }; // check this first
            self.regs.a = self.regs.a << 1;
            self.cycles += 2;
            self.regs.p.zero = isZer!( self.regs.a );
            self.regs.p.negative = isNeg!( self.regs.a );
            self.cycles += 2;
        }
        else {
            let data = self.dataFetch(true);
            let address = self.dataFetch(false);
            self.regs.p.carry = if ( ( data as u8 ) & 0x80 ) == 0x80 { 1 } else { 0 };

            let shifted_data = ( data as u8 ) << 1;
            set_zn!(self, shifted_data );
            self.memory[ address as usize ] = shifted_data;
            self.cycles += 4;
        }
    }
    
    fn sed( &mut self ) {
        self.regs.p.decimal = 1;
        self.cycles += 2;
    }

    fn pla( &mut self ) {
        self.regs.a = memAt!( self, 0x100 | ( self.regs.s as u16 ) + 1 ) as u8;
        set_zn!( self, self.regs.a );
        self.regs.s += 1;
        self.cycles += 4;
    }

    fn and( &mut self ) {
        self.regs.a &= self.dataFetch(true) as u8;
        set_zn!( self, self.regs.a );
        self.cycles += 2;
    }

    fn cmp( &mut self ) {
        let value = self.dataFetch(true) as u8;
        let diff = ovop!( -=, u8, self.regs.a, value );
        self.regs.p.carry = if self.regs.a >= value { 1 } else { 0 };
        self.regs.p.zero = if self.regs.a == value { 1 } else { 0 };
        self.regs.p.negative = if diff & 0x80 == 0x80 { 1 } else { 0 };
        self.cycles += 2;
    }

    fn ror( &mut self ) {
        if memAt!( self, self.pc ) == 0x6A {
            let a = self.regs.a;
            let old_p = self.regs.p.carry;
            self.regs.p.carry = self.regs.a & 0x1;
            self.regs.a = self.regs.a >> 1 | old_p << 7;
            set_zn!( self, self.regs.a );
            self.cycles += 2;
        }
        else {
            let value = self.dataFetch(true) as u8;
            let address = self.dataFetch(false);
            let old_p = self.regs.p.carry;
            self.regs.p.carry = value & 0x1;
            let shifted_data = value >> 1 | old_p << 7;
            set_zn!( self, shifted_data );
            self.memory[ address as usize ] = shifted_data;
            self.cycles += 4;
        }
    }

    fn rol( &mut self ) {
        if memAt!( self, self.pc ) == 0x2A {
            let a = self.regs.a;
            let old_p = self.regs.p.carry;
            self.regs.p.carry = if self.regs.a & 0x80 == 0x80 { 1 } else { 0 };
            self.regs.a = self.regs.a << 1 | old_p;
            set_zn!( self, self.regs.a );
            self.cycles += 2;
        }
        else {
            let value = self.dataFetch(true) as u8;
            let address = self.dataFetch(false) ;
            let old_p = self.regs.p.carry;
            self.regs.p.carry = if value & 0x80 == 0x80 { 1 } else { 0 };
            let shifted_data = value << 1 | old_p;
            set_zn!( self, shifted_data );
            self.memory[ address as usize ] = shifted_data;
            self.cycles += 4;
        }
    }

    fn inc( &mut self ) {
        let mut value = self.dataFetch(true) as u8;
        let address = self.dataFetch(false);
        value = ovop!( +=, u8, value, 1 );
        set_zn!( self, value );
        self.memory[ address as usize ] = value;
        self.cycles += 4;
    }

    fn dec( &mut self ) {
        let mut value = self.dataFetch(true) as u8;
        let address = self.dataFetch(false);
        value = ovop!( -=, u8, value, 1 );
        set_zn!( self, value );
        self.memory[ address as usize ] = value;
        self.cycles += 4;
    }

    fn lax( &mut self ) {
        let data = self.dataFetch(true);
        set_zn!( self, data );
        self.regs.x = ( data & 0xFF ) as u8;
        self.regs.a = ( data & 0xFF ) as u8;
        self.cycles += 2;
    }

    fn sax( &mut self ) {
        let address = self.dataFetch(true);
        self.memory[ address as usize ] = self.regs.a & self.regs.x;
        self.cycles += 2;
    }

    fn dcp( &mut self ) {
        let shift_val = self.dataFetch(true) as u8;
        let address = self.dataFetch(false);
        let new_val = ovop!(-=, u8, shift_val, 1 );
        self.regs.p.carry = if self.regs.a >= new_val { 1 } else { 0 };
        self.regs.p.zero = if self.regs.a == new_val { 1 } else { 0 };
        self.regs.p.negative = if self.regs.a & ( ovop!( -=, u8, self.regs.a, new_val ) ) & 0x80 == 0x80 { 1 } else { 0 };
        self.memory[ address as usize ] = new_val;
        self.cycles += 4;
        if memAt!(self, self.pc) == 0xDF
        {
            self.cycles -= 1;
        }
    }

    fn isc( &mut self ) {
        let shift_val = self.dataFetch(true) as u8;
        let address = self.dataFetch(false);
        let new_val = ovop!(+=, u8, shift_val, 1 );
        let semi_carr = 1 - self.regs.p.carry;
        let temp = ovop!( -=, u16, self.regs.a as i16, new_val as i16, semi_carr as i16 );
        self.regs.p.zero = if temp & 0xFF == 0 { 1 } else { 0 };
        self.regs.p.negative = if temp & 0x80 == 0x80 { 1 } else { 0 };
        self.regs.p.overflow = overflowsbc!( self.regs.a, new_val, temp as u8 );
        self.regs.p.carry = if temp & 0x100 == 0x100 { 0 } else { 1 };
        self.regs.a = temp as u8;
        self.memory[ address as usize ] = new_val;
        self.cycles += 4;
        if memAt!(self, self.pc) == 0xFF 
        {
            self.cycles -= 1;
        }
    }

    fn slo( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let address = self.dataFetch(false) as usize;
        self.regs.p.carry = if data & 0x80 == 0x80 { 1 } else { 0 };
        let shifted_data = data << 1;
        self.regs.a = shifted_data | self.regs.a;
        self.regs.p.zero = if self.regs.a == 0 { 1 } else { 0 };
        self.regs.p.negative = if self.regs.a & 0x80 == 0x80 { 1 } else { 0 };
        self.memory[ address ] = shifted_data;
        self.cycles += 4;
        if memAt!(self, self.pc ) == 0x1F
        {
            self.cycles -= 1;
        }
    }

    fn rla( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let old_p = self.regs.p.carry;
        let address = self.dataFetch(false) as usize;
        self.regs.p.carry = if data & 0x80 == 0x80 { 1 } else { 0 };
        let shifted_data = data << 1 | old_p;
        self.regs.a = self.regs.a & shifted_data;
        set_zn!(self, self.regs.a );
        self.memory[ address ] = shifted_data;
        self.cycles += 4;
        if memAt!( self, self.pc ) == 0x3F
        {
            self.cycles -= 1;
        }
    }

    fn sre( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let address = self.dataFetch(false) as usize;
        self.regs.p.carry = if data & 0x1 == 0x1 { 1 } else { 0 };
        let shifted_data = data >> 1;
        self.regs.a = self.regs.a ^ shifted_data;
        set_zn!(self, self.regs.a );
        self.memory[ address ] = shifted_data;
        self.cycles += 4;
        if memAt!( self, self.pc ) == 0x5F
        {
            self.cycles -= 1;
        }
    }

    fn rra( &mut self ) {
        let data = self.dataFetch(true) as u8;
        let address = self.dataFetch(false) as usize;
        let old_p = self.regs.p.carry;
        self.regs.p.carry = if data & 0x1 == 0x1 { 1 } else { 0 };
        let shifted_data = data >> 1 | old_p << 7;
        let temp = ovop!(+=, u16, shifted_data as u16, self.regs.a as u16, self.regs.p.carry as u16);
        self.regs.p.zero = if temp & 0xFF == 0 { 1 } else { 0 };
        self.regs.p.negative = if temp & 0x80 == 0x80 { 1 } else { 0 };
        self.regs.p.overflow = overflow!( self.regs.a, shifted_data, temp as u8 );
        self.regs.p.carry = if temp & 0x100 == 0x100 { 1 } else  { 0 };
        self.regs.a = temp as u8;
        self.memory[ address ] = shifted_data;
        self.cycles += 4;
        if memAt!( self, self.pc ) == 0x7F
        {
            self.cycles -= 1;
        }   

    }
}    

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let addressing_mode = ADDRESSING_MODE[self.memory[ self.pc as usize ] as usize ];
        match addressing_mode {
            0 => write!(f, "[{:x}]{} {} \t", self.pc, self.regs, name!(self)),
            1 => write!(f, "[{:x}]{} {} #{:x}\t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            2 | 12 => write!(f, "[{:x}]{} {} ${:x}\t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            3 | 16 => write!(f, "[{:x}]{} {} ${:x},X\t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            4 | 17 => write!(f, "[{:x}]{} {} ${:x},Y \t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            5 | 18 => write!(f, "[{:x}]{} {} (${:x}),X \t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            6 | 19 => write!(f, "[{:x}]{} {} (${:x}),Y \t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            7 | 13=> write!(f, "[{:x}]{} {} ${:x}{:x}\t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
            8 | 14 => write!(f, "[{:x}]{} {} {:x}{:x}, X\t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
            9 | 15 => write!(f, "[{:x}]{} {} {:x}{:x}, Y \t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
            10 => write!(f, "[{:x}]{} {} $({:x}{:x}) \t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 2], self.memory[self.pc as usize + 1]),
            11 => write!(f, "[{:x}]{} {} {:x} \t", self.pc, self.regs, name!(self), self.memory[self.pc as usize + 1]),
            _ => write!(f, "Specified wrong format"),
        }
    }
}
