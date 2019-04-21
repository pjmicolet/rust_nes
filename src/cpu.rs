extern crate hex;
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
[ "BRK", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "PHP", "ORA", "ASL", "ANC", "NOP", "ORA", "ASL", "SLO",
  "BPL", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "CLC", "ORA", "NOP", "SLO", "NOP", "ORA", "ASL", "SLO",
  "JSR", "AND", "KIL", "RLA", "BIT", "AND", "ROL", "RLA", "PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "RLA",
  "BMI", "AND", "KIL", "RLA", "NOP", "AND", "ROL", "RLA", "SEC", "AND", "NOP", "RLA", "NOP", "AND", "ROL", "RLA",
  "RTI", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "SRE",
  "BVC", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "CLI", "EOR", "NOP", "SRE", "NOP", "EOR", "LSR", "SRE",
  "RTS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA",
  "BVS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "SEI", "ADC", "NOP", "RRA", "NOP", "ADC", "ROR", "RRA",
  "NOP", "STA", "NOP", "SAX", "STY", "STA", "STX", "SAX", "DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "SAX",
  "BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "SAX", "TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX",
  "LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX", "TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "LAX",
  "BCS", "LDA", "KIL", "LAX", "LDY", "LDA", "LDX", "LAX", "CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "LAX",
  "CPY", "CMP", "NOP", "DCP", "CPY", "CMP", "DEC", "DCP", "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP",
  "BNE", "CMP", "KIL", "DCP", "NOP", "CMP", "DEC", "DCP", "CLD", "CMP", "NOP", "DCP", "NOP", "CMP", "DEC", "DCP",
  "CPX", "SBC", "NOP", "ISC", "CPX", "SBC", "INC", "ISC", "INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISC",
  "BEQ", "SBC", "KIL", "ISC", "NOP", "SBC", "INC", "ISC", "SED", "SBC", "NOP", "ISC", "NOP", "SBC", "INC", "ISC" ];

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

    fn newArgs( new_pc : u16, new_instr : String, new_regs : Vec<u16>, new_cycles : u64 ) -> DebugInfo {
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
            p_reg[i] = ( ( ( new_regs[3] >> i ) as u8 ) & 0x1 );
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
        let p_flag = self.p.carry | ( self.p.zero  << 1 ) | ( self.p.interrupt << 2 ) | ( self.p.decimal << 3 ) | (self.p.s1 << 4 ) | (self.p.s2 << 5) | (self.p.overflow << 6) | (self.p.negative << 7 ); 
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
                data = $x.memory[ ( ( data + $y as u16 ) as usize ) ] as u16;
             )+
            data as u16
        }
    };
}

macro_rules! branchOn {
    ( $self:ident, $reg:ident, $val:expr ) => {
        let displacement = memAt!( $self, $self.pc + 1 );
        if( $self.regs.p.$reg == $val )
        { 
            $self.cycles += 3;
            if( ( ( displacement + 0xFF & ( $self.pc + 2 ) ) & 0x100 ) != 0 ) { // we cross page boundary 
                $self.cycles += 2;
            }
            $self.pc += displacement + 2;
        }
        else
        {
            $self.cycles += 2;
            $self.nextPc();
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
        names[$self.memory[ $self.pc as usize ] as usize ]
    };
}

macro_rules! debugName {
    ( $self:expr ) => {
        $self.debug_data[ $self.debug_iter ].instr
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

    pub fn loadRom( &mut self, data : Vec<u8>, debug : bool, debug_path : &str ) -> Result<()> {
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

        if( debug ) {
            let path = Path::new( debug_path );
            // Open the path in read-only mode, returns `io::Result<File>`
            let file = File::open(path)?;
            let mut s = String::new();
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
                self.debug_data.push( DebugInfo::newArgs( pc, instr, regs, cycle ) );

            }
        }
        Ok(())
    }
    
    pub fn debugValidate( & mut self ){
        if name!( self ) != debugName!( self ) {
            panic!( "Hey this isn't right Got: {} Expected: {}", name!( self ) , debugName!( self ) );
        }
        if self.regs != self.debug_data[ self.debug_iter ].regs {
            panic!( "regs\nGot:{:x}{}\nExpected:{:x}{}", self.pc, self.regs,self.debug_data[self.debug_iter].pc, self.debug_data[ self.debug_iter].regs );
        }
        if self.pc != self.debug_data[ self.debug_iter ].pc {
            panic!( "PC is different Got: {} Expected: {}", self.pc, self.debug_data[ self.debug_iter].pc );
        }
    }

    pub fn execute(&mut self) {
        while( self.pc < 0xFFFF ) {
            let memIndex : usize = self.pc as usize;
            let index : usize = self.memory[memIndex] as usize;
            if self.debug_data.len() > 1 {
                self.debugValidate();
                self.debug_iter = self.debug_iter + 1;
            }
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
            0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => self.sta(),
            0x24 | 0x2C => self.bit(),
            0x38 => self.sec(),
            0x20 => { self.jsr(); return },
            0x10 => { self.bpl(); return },
            0x30 => { self.bmi(); return },
            0x50 => { self.bvc(); return },
            0x70 => { self.bvs(); return },
            0xB0 => { self.bcs(); return },
            0x90 => { self.bcc(); return },
            0xF0 => { self.beq(); return },
            0xD0 => { self.bne(); return },
            0x60 => { self.rts(); return },
            0x18 => self.clc(),
            _ => panic!( "Hey you haven't implemented {}", name!(self)),
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
        self.regs.p.zero = isZer!( self.regs.a );
        self.regs.p.negative = isNeg!( self.regs.a );
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

    fn sta( &mut self ) {
        let memory_address = self.dataFetch() as usize;
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
        let bit_data = self.dataFetch() as u8;
        let a_anded = bit_data & self.regs.a;
        self.regs.p.zero = isZer!( a_anded );
        self.regs.p.overflow = ( ( a_anded & 0x20 ) == 0x20 ) as u8;
        self.regs.p.negative = isNeg!( a_anded);
        self.cycles += 2;
    }

}    

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let addressingMode = addressing_mode[self.memory[ self.pc as usize ] as usize ];
        match addressingMode {
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
