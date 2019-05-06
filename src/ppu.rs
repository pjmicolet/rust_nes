struct PPUCTRL {
    v : u8,
    p : u8,
    h : u8,
    b : u8,
    s : u8,
    i : u8,
    n1 : u8,
    n2 : u8
}

impl PPUCTRL {
    fn new() -> PPUCTRL {
        PPUCTRL {
            v : 0,
            p : 0,
            h : 0,
            b : 0,
            s : 0,
            i : 0,
            n1 : 0,
            n2 : 0,
        }
    }
}

struct PPUMASK {
    B : u8,
    G : u8,
    R : u8,
    s : u8,
    b : u8,
    M : u8,
    m : u8,
    Gr : u8
}

impl PPUMASK {
    fn new() -> PPUMASK {
        PPUMASK {
            B : 0,
            G : 0,
            R : 0,
            s : 0,
            b : 0,
            M : 0,
            m : 0,
            Gr : 0
        }
    }
}

struct PPUSTATUS {
    V : u8,
    S : u8,
    O : u8
}

impl PPUSTATUS {
    fn new() -> PPUSTATUS {
        PPUSTATUS { 
            V : 0,
            S : 0,
            O : 0,
        }
    }
}

struct PPURegs {
    ppuctrl : PPUCTRL,
    ppumask : PPUMASK,
    ppustatus : PPUSTATUS,
    oamaddr : u8,
    oamdata_low : u8,
    ppuscroll : u8,
    ppuaddr : u8,
    ppudata : u8,
    oamdata_high : u8,
}

impl PPURegs {
    fn new() -> PPURegs {
        PPURegs { 
            ppuctrl : PPUCTRL::new(),
            ppumask : PPUMASK::new(),
            ppustatus : PPUSTATUS::new(),
            oamaddr : 0,
            oamdata_low : 0,
            ppuscroll : 0,
            ppuaddr : 0,
            ppudata : 0,
            oamdata_high : 0,
        }
    }
}

pub struct PPU<'mem> {
    regs : PPURegs,
    memory : &'mem mut [u8],
}

impl<'mem> PPU<'mem> {
    pub fn new( mem : &'mem mut [u8] ) -> PPU<'mem> {
        PPU {
            regs : PPURegs::new(),
            memory : mem,
        }
    }
}


