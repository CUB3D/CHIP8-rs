const CHIP8_DEFAULT_FONT: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

#[derive(Debug, Eq, PartialOrd, PartialEq, Copy, Clone)]
pub(crate) enum Register {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9,
    VA,
    VB,
    VC,
    VD,
    VE,
    VF,
    I,
}

impl From<u8> for Register {
    fn from(r: u8) -> Self {
        match r {
            0x0 => Register::V0,
            0x1 => Register::V1,
            0x2 => Register::V2,
            0x3 => Register::V3,
            0x4 => Register::V4,
            0x5 => Register::V5,
            0x6 => Register::V6,
            0x7 => Register::V7,
            0x8 => Register::V8,
            0x9 => Register::V9,
            0xA => Register::VA,
            0xB => Register::VB,
            0xC => Register::VC,
            0xD => Register::VD,
            0xE => Register::VE,
            0xF => Register::VF,
            _ => unimplemented!(),
        }
    }
}

impl From<Register> for usize {
    fn from(r: Register) -> Self {
        r.value() as usize
    }
}

impl Register {
    fn value(&self) -> u8 {
        match self {
            Register::V0 => 0x0,
            Register::V1 => 0x1,
            Register::V2 => 0x2,
            Register::V3 => 0x3,
            Register::V4 => 0x4,
            Register::V5 => 0x5,
            Register::V6 => 0x6,
            Register::V7 => 0x7,
            Register::V8 => 0x8,
            Register::V9 => 0x9,
            Register::VA => 0xA,
            Register::VB => 0xB,
            Register::VC => 0xC,
            Register::VD => 0xD,
            Register::VE => 0xE,
            Register::VF => 0xF,
            Register::I => unimplemented!(),
        }
    }
}

#[derive(Debug, Eq, PartialOrd, PartialEq)]
enum Instruction {
    Unknown,
    CallSub {
        address: u16,
    },
    SetRegister {
        register: Register,
        value: u16,
    },
    DrawSprite {
        x: Register,
        y: Register,
        height: u8,
    },
    SetDelayTimer {
        value: Register,
    },
    SetSoundTimer {
        value: Register,
    },
    GetDelay {
        dest: Register,
    },
    GetKey {
        dest: Register,
    },
    IncrementRegisterByRegister {
        dest: Register,
        by: Register,
    },
    IncrementRegisterByImmediate {
        dest: Register,
        by: u8,
    },
    GetSpriteAddress {
        src: Register,
    },
    BCD {
        src: Register,
    },
    RegisterDump {
        dest: Register,
    },
    RegisterLoad {
        dest: Register,
    },
    ClearDisplay,
    Return,
    CallRCA {
        address: u16,
    },
    IfEq {
        a: Register,
        b: u8,
    },
    IfRegisterEq {
        a: Register,
        b: Register,
    },
    IfNeq {
        a: Register,
        b: u8,
    },
    Jmp {
        address: u16,
    },
    Rand {
        dest: Register,
        modulus: u8,
    },
    IfKeyEq {
        comp: Register,
    },
    IfKeyNeq {
        comp: Register,
    },
    SetRegisterRegister {
        src: Register,
        dest: Register,
    },
    BitwiseOr {
        a: Register,
        b: Register,
    },
    BitwiseAnd {
        a: Register,
        b: Register,
    },
    BitwiseXor {
        a: Register,
        b: Register,
    },
    DecrementRegisterByRegister {
        a: Register,
        b: Register,
    },
    ShiftRight {
        a: Register,
    },
    Subtract {
        a: Register,
        b: Register,
    },
    ShiftLeft {
        a: Register,
    },
}

impl From<u16> for Instruction {
    fn from(i: u16) -> Self {
        let nnn = i & 0b0000_1111_1111_1111;
        let nn = (i & 0b0000_0000_1111_1111) as u8;
        let n = (i & 0b0000_0000_0000_1111) as u8;
        let x = ((i & 0b0000_1111_0000_0000) >> 8) as u8;
        let y = ((i & 0b0000_0000_1111_0000) >> 4) as u8;

        let opcode: u8 = ((i & 0b1111_0000_0000_0000) >> 12) as u8;

        let ii = match opcode {
            0x0 => match nn {
                0xE0 => Instruction::ClearDisplay,
                0xEE => Instruction::Return,
                _ => Instruction::CallRCA { address: nnn },
            },
            0x1 => Instruction::Jmp { address: nnn },
            0x2 => Instruction::CallSub { address: nnn },
            0x3 => Instruction::IfEq {
                a: Register::from(x),
                b: nn,
            },
            0x4 => Instruction::IfNeq {
                a: Register::from(x),
                b: nn,
            },
            0x5 => Instruction::IfRegisterEq {
                a: Register::from(x),
                b: Register::from(y),
            },
            0x6 => Instruction::SetRegister {
                register: Register::from(x),
                value: nn as u16,
            },
            0x7 => Instruction::IncrementRegisterByImmediate {
                dest: Register::from(x),
                by: nn,
            },
            0x8 => {
                let reg_x = Register::from(x);
                let reg_y = Register::from(y);

                match n {
                    0x0 => Instruction::SetRegisterRegister {
                        src: reg_x,
                        dest: reg_y,
                    },
                    0x1 => Instruction::BitwiseOr { a: reg_x, b: reg_y },
                    0x2 => Instruction::BitwiseAnd { a: reg_x, b: reg_y },
                    0x3 => Instruction::BitwiseXor { a: reg_x, b: reg_y },
                    0x4 => Instruction::IncrementRegisterByRegister {
                        dest: reg_x,
                        by: reg_y,
                    },
                    0x5 => Instruction::DecrementRegisterByRegister { a: reg_x, b: reg_y },
                    0x6 => Instruction::ShiftRight { a: reg_x },
                    0x7 => Instruction::Subtract { a: reg_x, b: reg_y },
                    0xE => Instruction::ShiftLeft { a: reg_x },
                    _ => Instruction::Unknown,
                }
            }
            0xA => Instruction::SetRegister {
                register: Register::I,
                value: nnn,
            },
            0xC => Instruction::Rand {
                dest: Register::from(x),
                modulus: nn,
            },
            0xD => Instruction::DrawSprite {
                x: Register::from(x),
                y: Register::from(y),
                height: n,
            },
            0xE => match nn {
                0x9E => Instruction::IfKeyEq {
                    comp: Register::from(x),
                },
                0xA1 => Instruction::IfKeyNeq {
                    comp: Register::from(x),
                },
                _ => Instruction::Unknown,
            },
            0xF => {
                let reg_x = Register::from(x);

                match nn {
                    0x07 => Instruction::GetDelay { dest: reg_x },
                    0x0A => Instruction::GetKey { dest: reg_x },
                    0x15 => Instruction::SetDelayTimer { value: reg_x },
                    0x18 => Instruction::SetSoundTimer { value: reg_x },
                    0x1E => Instruction::IncrementRegisterByRegister {
                        dest: Register::I,
                        by: reg_x,
                    },
                    0x29 => Instruction::GetSpriteAddress { src: reg_x },
                    0x33 => Instruction::BCD { src: reg_x },
                    0x55 => Instruction::RegisterDump { dest: reg_x },
                    0x65 => Instruction::RegisterLoad { dest: reg_x },

                    _ => Instruction::Unknown,
                }
            }
            _ => Instruction::Unknown,
        };

        if Instruction::Unknown == ii {
            println!("instruction {:x}", i);
        }

        ii
    }
}

pub trait PlatformBackend {
    fn is_key_pressed(&self, key: u8) -> bool;
}

pub(crate) const SCREEN_HEIGHT: usize = 32;
pub(crate) const SCREEN_WIDTH: usize = 64;

pub(crate) struct Emu {
    pub(crate) address_register: usize,
    pub(crate) program_counter: u16,
    memory: [u8; 4096],
    pub(crate) registers: [u8; 16],
    pub(crate) stack: Vec<u16>,
    pub(crate) delay_timer: u8,
    pub(crate) sound_timer: u32,
    pub(crate) screen_buffer: [u8; SCREEN_HEIGHT * SCREEN_WIDTH],
    pub(crate) keys: [bool; 16],
}

impl Emu {
    pub(crate) fn new() -> Self {
        let mut initial_memory = [0; 4096];
        // copy the font into mem
        let font_base = 432;
        for i in 0..CHIP8_DEFAULT_FONT.len() {
            initial_memory[font_base + i] = CHIP8_DEFAULT_FONT[i];
        }

        Self {
            program_counter: 0x200,
            address_register: 0,
            memory: initial_memory,
            registers: [0; 16],
            stack: Vec::new(),
            delay_timer: 0,
            sound_timer: 0,
            screen_buffer: [0; 64 * 32],
            keys: [false; 16],
        }
    }

    pub(crate) fn load_rom(&mut self, data: &[u8]) {
        for i in 0..data.len() {
            self.memory[0x200 + i] = data[i];
        }
    }

    fn read_instruction(&self) -> Instruction {
        let ins = ((*self.memory.get(self.program_counter as usize).unwrap() as u16) << 8)
            | (*self.memory.get(self.program_counter as usize + 1).unwrap() as u16);
        Instruction::from(ins)
    }

    fn get_register(&self, reg: Register) -> u8 {
        self.registers[Into::<usize>::into(reg)]
    }
    fn set_register(&mut self, reg: Register, val: u8) {
        self.registers[Into::<usize>::into(reg)] = val;
    }

    fn is_key_pressed(&self, key: u8) -> bool {
        self.keys[key as usize]
    }

    fn run_instruction(&mut self) {
        let instruction = self.read_instruction();
        self.program_counter += 2;

        match instruction {
            Instruction::SetRegister { register, value } => {
                if register == Register::I {
                    self.address_register = value as usize;
                } else {
                    self.set_register(register, value as u8);
                }
            }
            Instruction::CallSub { address } => {
                self.stack.push(self.program_counter);
                self.program_counter = address;
            }
            Instruction::BCD { src } => {
                let v = self.get_register(src);
                self.memory[self.address_register] = v / 100;
                self.memory[self.address_register + 1] = (v / 10) % 10;
                self.memory[self.address_register + 2] = v % 10;
            }
            Instruction::IncrementRegisterByImmediate { dest, by } => {
                let (sum, _) = self.get_register(dest).overflowing_add(by);
                self.set_register(dest, sum);
            }
            Instruction::Return => {
                self.program_counter = self.stack.pop().unwrap();
            }
            Instruction::SetDelayTimer { value } => {
                self.delay_timer = self.get_register(value);
            }
            Instruction::SetSoundTimer { value } => {
                self.sound_timer = self.get_register(value) as u32;
            }
            Instruction::GetDelay { dest } => self.set_register(dest, self.delay_timer),
            Instruction::IfEq { a, b } => {
                if self.get_register(a) == b {
                    self.program_counter += 2;
                }
            }
            Instruction::IfNeq { a, b } => {
                if self.get_register(a) != b {
                    self.program_counter += 2;
                }
            }
            Instruction::Jmp { address } => {
                self.program_counter = address;
            }
            Instruction::DrawSprite { x, y, height } => {
                let x = self.get_register(x) as usize;
                let y = self.get_register(y) as usize;

                self.set_register(Register::VF, 0);

                for dy in 0..height as usize {
                    let line = self.memory[self.address_register + dy];

                    for dx in 0usize..8 {
                        let pixel = line & (0b_1000_0000 >> dx);
                        let pos = (y + dy) * 64 + (x + dx);

                        if pixel != 0 {
                            let bit = 1 << (pos & 0b111);

                            if self.screen_buffer[pos] & bit > 0 {
                                self.set_register(Register::VF, 1);
                            }
                            self.screen_buffer[pos] ^= bit;
                        }
                    }
                }
            }
            Instruction::GetSpriteAddress { src } => {
                self.address_register = 432 + (self.get_register(src) as usize & 0xF) * 5;
            }
            Instruction::RegisterLoad { dest } => {
                for i in 0..dest.value() {
                    self.registers[i as usize] = self.memory[self.address_register + i as usize];
                }

                self.address_register += dest.value() as usize + 1;
            }
            Instruction::Rand { dest, modulus } => {
                self.set_register(dest, rand::random::<u8>() % modulus)
            }
            Instruction::IfKeyEq { comp } => {
                if self.is_key_pressed(self.get_register(comp)) {
                    self.program_counter += 2;
                }
            }
            Instruction::IfKeyNeq { comp } => {
                if !self.is_key_pressed(self.get_register(comp)) {
                    self.program_counter += 2;
                }
            }
            Instruction::SetRegisterRegister { dest, src } => {
                self.set_register(dest, self.get_register(src));
            }
            Instruction::BitwiseOr { a, b } => {
                self.set_register(a, self.get_register(a) | self.get_register(b))
            }
            Instruction::BitwiseAnd { a, b } => {
                self.set_register(a, self.get_register(a) & self.get_register(b))
            }
            Instruction::BitwiseXor { a, b } => {
                self.set_register(a, self.get_register(a) ^ self.get_register(b))
            }
            Instruction::IncrementRegisterByRegister { dest, by } => {
                self.set_register(Register::VF, 0);
                let (sum, overflow) = self
                    .get_register(dest)
                    .overflowing_add(self.get_register(by));
                if overflow {
                    self.set_register(Register::VF, 1);
                }
                self.set_register(dest, sum);
            }
            Instruction::DecrementRegisterByRegister { a, b } => {
                self.set_register(Register::VF, 1);
                let (sum, overflow) = self.get_register(b).overflowing_sub(self.get_register(a));
                if overflow {
                    self.set_register(Register::VF, 0);
                }
                self.set_register(Register::VF, sum);
            }
            Instruction::Unknown => {
                panic!("Unknown instruction")
            }
            _ => {
                println!("{:?}", instruction);
            }
        }
    }

    pub(crate) fn tick(&mut self) {
        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }

        if self.sound_timer > 0 {
            self.sound_timer -= 1;
            if self.sound_timer == 0 {
                println!("BEEP");
            }
        }

        self.run_instruction();
    }
}
