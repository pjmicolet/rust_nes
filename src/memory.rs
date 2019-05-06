pub struct MemoryController {
    memory : [u8; 0x10000],
}

impl MemoryController {
    
    pub fn new() -> MemoryController {
        MemoryController {
            memory : [0;0x10000],
        }
    }

    // The difference between a read and a load here
    // is that reads will not trigger any side effects
    // such as setting PPU flags.
    //
    // It should be mainly used for debugging
    pub fn read(&self, address : usize ) -> u8 {
        self.memory[ address as usize ]
    }

    pub fn write(&mut self, address : usize , value : u8 ) {
        self.memory[ address as usize ] = value as u8;
    }

    // Execute load for CPU (fetches from RAM)
    // This will also trigger specific operations in case
    // we read from specific areas.
    pub fn load(&self, address : usize ) -> u16 {
        self.memory[ address as usize ] as u16
    }

    pub fn store(&mut self, address : usize, value : u8 ) {
        self.memory[ address as usize ] = value as u8;
    }
}
