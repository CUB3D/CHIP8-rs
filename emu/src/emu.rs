use crate::graph::GraphManager;
use crate::jit::{NativeFunction, JIT};
use std::collections::HashMap;
use std::fs::File;
use std::ops::{Index, IndexMut};
use std::slice::SliceIndex;
use std::time::{Duration, Instant};

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

#[derive(Debug, Eq, PartialOrd, PartialEq, Copy, Clone, Hash)]
pub enum Register {
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
    pub(crate) fn value(&self) -> u8 {
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
        }
    }
}

#[derive(Debug, Eq, PartialOrd, PartialEq, Copy, Clone)]
pub enum Instruction {
    Unknown,
    CallSub {
        address: u16,
    },
    SetI {
        value: u16,
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
    IncrementIByRegister {
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
    IfRegisterNeq {
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
        a: Register,
        b: Register,
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
    IncrementPc {
        offset: u16,
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
                    0x0 => Instruction::SetRegisterRegister { a: reg_x, b: reg_y },
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
            0x9 => Instruction::IfRegisterNeq {
                a: Register::from(x),
                b: Register::from(y),
            },
            0xA => Instruction::SetI { value: nnn },
            0xB => Instruction::IncrementPc { offset: nnn },
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
                    0x1E => Instruction::IncrementIByRegister { by: reg_x },
                    0x29 => Instruction::GetSpriteAddress { src: reg_x },
                    0x33 => Instruction::BCD { src: reg_x },
                    0x55 => Instruction::RegisterDump { dest: reg_x },
                    0x65 => Instruction::RegisterLoad { dest: reg_x },

                    _ => Instruction::Unknown,
                }
            }
            _ => Instruction::Unknown,
        };

        ii
    }
}

pub trait PlatformBackend {
    fn is_key_pressed(&self, key: u8) -> bool;
}

#[derive(Debug, Copy, Clone, PartialOrd, Eq, PartialEq)]
pub struct InstructionReference {
    pub op: Instruction,
    pub pos: u16,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Block {
    ops: Vec<InstructionReference>,
    native_func: Option<NativeFunction>,
}

impl Block {
    fn empty() -> Self {
        Self {
            ops: Vec::new(),
            native_func: None,
        }
    }

    fn compile(&mut self, jit: &mut JIT) -> NativeFunction {
        if self.native_func.is_none() {
            self.native_func = Some(jit.compile_native(self.ops.clone()));
        }

        self.native_func.unwrap()
    }

    fn can_be_jitted(&self) -> bool {
        !self.ops.iter().any(|i| {
            matches!(i.op, Instruction::DrawSprite { height, x, y })
                || matches!(i.op, Instruction::GetDelay { dest })
                || matches!(
                    i.op,
                    Instruction::IfKeyNeq { comp } | Instruction::IfKeyEq { comp }
                )
        })
    }

    /// Detect if a block is a full loop from start to finish, if so this is a jit candidate
    fn is_loop(&self) -> bool {
        if self.ops.is_empty() {
            return false;
        }

        let first = self.ops.first().unwrap();
        let last = self.ops.last().unwrap();

        match last.op {
            Instruction::Jmp { address } => {
                if address == first.pos {
                    return true;
                }
            }
            _ => {}
        }

        false
    }

    /// Find a loop from start to end, format is Option<(start, end)>, None if no loops exist
    fn find_loop(&self) -> Option<(usize, usize)> {
        for (pos, i) in self.ops.iter().enumerate() {
            match i.op {
                Instruction::IfEq { a: _, b: _ } => match self.ops.iter().nth(pos + 1) {
                    Some(i2) => match i2.op {
                        Instruction::Jmp { address } => {
                            if let Some(end) = self.ops.iter().position(|i3| i3.pos == address) {
                                return Some((end, pos + 1));
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                },
                Instruction::IfNeq { a: _, b: _ } => match self.ops.iter().nth(pos + 1) {
                    Some(i2) => match i2.op {
                        Instruction::Jmp { address } => {
                            if let Some(end) = self.ops.iter().position(|i3| i3.pos == address) {
                                return Some((pos, end));
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                },
                Instruction::Jmp { address } => {
                    if let Some(end) = self.ops.iter().position(|i3| i3.pos == address) {
                        return Some((pos, end));
                    }
                }
                _ => {}
            }
        }

        None
    }

    fn cleave(&mut self, range: (usize, usize)) -> Block {
        let mut newblk = Block::empty();

        for i in range.0..range.1 + 1 {
            let i = *self.ops.get(i).unwrap();
            newblk.ops.push(i);
        }

        for i in range.0..range.1 + 1 {
            self.ops.remove(range.0);
        }

        newblk
    }

    fn contains_loop(&self) -> bool {
        self.find_loop().is_some()
    }
}

struct LoopFinder {
    possible_loops: Vec<Block>,
    jitted: HashMap<u16, NativeFunction>,
}

impl LoopFinder {
    fn new() -> Self {
        Self {
            possible_loops: vec![Block::empty()],
            jitted: HashMap::new(),
        }
    }

    fn trace(&mut self, i: InstructionReference) {
        //println!("{:?}", i);
        // Have we seen this ir before
        if self.possible_loops.iter().any(|pl| pl.ops.contains(&i)) {
            return;
        }

        let blk = self.possible_loops.last_mut().unwrap();

        if blk.ops.is_empty() {
            blk.ops.push(i);
        } else {
            let lst = blk.ops.last().unwrap();
            if i.pos == lst.pos + 2 {
                blk.ops.push(i);
                self.break_loops();
            } else {
                // println!("Block complete {:?}, isloop: {}", blk, blk.contains_loop());
                self.possible_loops.push(Block::empty());
            }
        }
    }

    /// Takes all the possible loops found so far, extracts full loops from them and throws away non loops
    /// Keeps the last block as that one is still being built (otherwise it would be thrown away every time)
    fn break_loops(&mut self) {
        let mut new_possible_loops = Vec::new();
        let last = self.possible_loops.last().cloned();

        for blk in self.possible_loops.iter_mut() {
            if !blk.is_loop() {
                if let Some((start, end)) = blk.find_loop() {
                    //TODO: this will throw away the bits outside, should we keep them
                    let lop = blk.cleave((start, end));
                    assert_eq!(lop.is_loop(), true, "The bit we removed must be a loop");
                    new_possible_loops.push(lop);
                } else {
                    // If it has no loops, then keep the last block as its not done being built yet
                    if Some(blk.clone()) == last {
                        new_possible_loops.push(blk.clone());
                    }
                }
            } else {
                // Keep old loops
                new_possible_loops.push(blk.clone());
            }
        }

        self.possible_loops = new_possible_loops;

        //self.dump_loops();
    }

    fn dump_loops(&self) {
        for blk in self.possible_loops.iter() {
            if blk.is_loop() {
                println!("{:?}", blk);
            }
        }
        println!("--------");
    }

    fn compile_loops(&mut self, jit: &mut JIT) {
        //self.dump_loops();

        for blk in self.possible_loops.iter_mut() {
            if blk.is_loop()
                && blk.can_be_jitted()
                && self.jitted.get(&blk.ops.first().unwrap().pos).is_none()
            {
                self.jitted
                    .insert(blk.ops.first().unwrap().pos, blk.compile(jit));
            }
        }
    }

    fn has_jitted(&mut self, pc: u16) -> Option<NativeFunction> {
        self.jitted.get(&pc).cloned()
    }
}

pub enum InstructionExecution {
    Emulated(Instruction),
    Native(NativeFunction),
}

pub struct Memory {
    data: [u8; MEMORY_SIZE],
    loop_finder: LoopFinder,
    jit: JIT,
    pub jit_enabled: bool,
}

impl Memory {
    fn new() -> Self {
        let mut initial_memory = [0; MEMORY_SIZE];
        // copy the font into mem
        let font_base = 432;
        for i in 0..CHIP8_DEFAULT_FONT.len() {
            initial_memory[font_base + i] = CHIP8_DEFAULT_FONT[i];
        }

        Self {
            data: initial_memory,
            loop_finder: LoopFinder::new(),
            jit: JIT::default(),
            jit_enabled: true,
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn inner(&self) -> [u8; MEMORY_SIZE] {
        self.data
    }

    fn get(&self, index: u16) -> Option<&u8> {
        self.data.get(index as usize)
    }

    pub fn read_instruction(&mut self, pc: u16) -> Option<InstructionExecution> {
        if self.jit_enabled {
            if let Some(nf) = self.loop_finder.has_jitted(pc) {
                return Some(InstructionExecution::Native(nf));
            }
        }

        if let (Some(a), Some(b)) = (self.get(pc), self.get(pc + 1)) {
            let ins = (*a as u16) << 8 | (*b as u16);
            let i = Instruction::from(ins);

            if self.jit_enabled {
                self.loop_finder
                    .trace(InstructionReference { pos: pc, op: i });

                self.loop_finder.compile_loops(&mut self.jit);
            }

            Some(InstructionExecution::Emulated(i))
        } else {
            None
        }
    }
}

impl Index<u16> for Memory {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.data[index as usize]
    }
}

impl Index<usize> for Memory {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

pub const SCREEN_HEIGHT: usize = 32;
pub const SCREEN_WIDTH: usize = 64;
pub const MEMORY_SIZE: usize = 4096;

pub struct Emu {
    pub address_register: usize,
    pub program_counter: u16,
    pub memory: Memory,
    pub registers: [u8; 16],
    pub stack: Vec<u16>,
    pub delay_timer: u8,
    pub sound_timer: u8,
    pub screen_buffer: [[u8; SCREEN_WIDTH]; SCREEN_HEIGHT],
    pub keys: [bool; 16],
    pub halted: bool,

    fps_timer: Instant,
    pub instructions_per_second: u32,
    instructions: u32,

    pub jit: JIT,

    debug: bool,

    graph: GraphManager,
}

impl Emu {
    pub fn new() -> Self {
        Self {
            program_counter: 0x200,
            address_register: 0,
            memory: Memory::new(),
            registers: [0; 16],
            stack: Vec::new(),
            delay_timer: 0,
            sound_timer: 0,
            screen_buffer: [[0; SCREEN_WIDTH]; SCREEN_HEIGHT],
            keys: [false; 16],
            fps_timer: Instant::now(),
            instructions_per_second: 0,
            instructions: 0,
            halted: false,
            jit: JIT::default(),
            debug: false,
            graph: GraphManager::default(),
        }
    }

    pub fn reset(&mut self) {
        self.halted = false;
        self.instructions = 0;
        self.instructions_per_second = 0;
        self.fps_timer = Instant::now();
        self.keys = [false; 16];
        self.screen_buffer = [[0; SCREEN_WIDTH]; SCREEN_HEIGHT];
        self.sound_timer = 0;
        self.delay_timer = 0;
        self.stack.clear();
        self.registers = [0; 16];
        self.memory = Memory::new();
        self.address_register = 0;
        self.program_counter = 0x200;
        self.jit = JIT::default();
    }

    pub fn load_rom(&mut self, data: &[u8]) {
        self.reset();
        for i in 0..data.len() {
            self.memory[0x200 + i] = data[i];
        }
    }

    pub fn read_instruction(address: u16, memory: &[u8; MEMORY_SIZE]) -> Option<Instruction> {
        if let (Some(a), Some(b)) = (
            memory.get(address as usize),
            memory.get(address as usize + 1),
        ) {
            let ins = (*a as u16) << 8 | (*b as u16);
            Some(Instruction::from(ins))
        } else {
            None
        }
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

    /// Dump the current state of the emulator into a human readable format, also used for constructing test cases
    pub fn dump_state(&self) -> String {
        // format!("Registers: {:?} I: {}, PC: {}; DT: {}, ST: {}", self.registers, self.address_register, self.program_counter, self.delay_timer, self.sound_timer)
        format!(
            "Registers: {:?} PC: {}",
            &self.registers, self.program_counter
        )
    }

    pub fn run_instruction(&mut self) {
        let instruction = self
            .memory
            .read_instruction(self.program_counter)
            .expect("Unable to read instruction");
        self.program_counter += 2;

        match instruction {
            InstructionExecution::Emulated(instruction) => self.execute_instruction(instruction),
            InstructionExecution::Native(nf) => self.registers = nf.call(self.registers),
        }
    }

    pub fn execute_instruction(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::SetRegister { register, value } => {
                self.set_register(register, value as u8);
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
                self.sound_timer = self.get_register(value);
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

                let mut modified = 0;

                for dy in 0..height as usize {
                    let line = self.memory[self.address_register + dy];

                    for dx in 0usize..8 {
                        let pixel = line & (0b_1000_0000 >> dx);

                        let current_pixel = self
                            .screen_buffer
                            .get_mut((y + dy) as usize)
                            .and_then(|row| row.get_mut((x + dx) as usize));

                        if let Some(current) = current_pixel {
                            if pixel != 0 {
                                if *current == 1 {
                                    modified = 1;
                                }
                                *current ^= 1;
                            }
                        }
                    }
                }

                self.set_register(Register::VF, modified);
            }
            Instruction::GetSpriteAddress { src } => {
                self.address_register = 432 + (self.get_register(src) as usize & 0xF) * 5;
            }
            Instruction::RegisterLoad { dest } => {
                for i in 0..=dest.value() {
                    self.registers[i as usize] = self.memory[self.address_register + i as usize];
                }

                self.address_register += dest.value() as usize + 1;
            }
            Instruction::Rand { dest, modulus } => {
                let r = 42; //rand::random::<u8>();
                self.set_register(dest, r % modulus)
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
            Instruction::SetRegisterRegister { a, b } => {
                self.set_register(a, self.get_register(b));
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
                let (sum, overflow) = self.get_register(a).overflowing_sub(self.get_register(b));
                if overflow {
                    self.set_register(Register::VF, 0);
                }
                self.set_register(a, sum);
            }
            Instruction::ShiftRight { a } => {
                self.set_register(Register::VF, self.get_register(a) & 0x01);
                self.set_register(a, self.get_register(a) >> 1);
            }
            Instruction::Subtract { a, b } => {
                self.set_register(Register::VF, 1);
                let (sum, overflow) = self.get_register(a).overflowing_sub(self.get_register(b));
                self.set_register(a, sum);
                if overflow {
                    self.set_register(Register::VF, 0);
                }
            }
            Instruction::ShiftLeft { a } => {
                self.set_register(Register::VF, self.get_register(a) & 0b1000);
                self.set_register(a, self.get_register(a) << 1);
            }
            Instruction::Unknown => {
                panic!("Unknown instruction")
            }
            Instruction::SetI { value } => {
                self.address_register = value as usize;
            }
            Instruction::ClearDisplay => self.screen_buffer = [[0; SCREEN_WIDTH]; SCREEN_HEIGHT],
            Instruction::RegisterDump { dest } => {
                for i in 0..=dest.value() {
                    self.memory[self.address_register + i as usize] = self.registers[i as usize];
                }

                self.address_register += dest.value() as usize + 1;
            }
            Instruction::IncrementIByRegister { by } => {
                self.address_register += self.get_register(by) as usize;
            }
            Instruction::GetKey { dest } => {
                if let Some((i, _k)) = self.keys.iter().enumerate().find(|(i, k)| **k) {
                    self.set_register(dest, i as u8);
                } else {
                    // keep looping this instruction until we get a key
                    self.program_counter -= 2;
                }
            }
            Instruction::IfRegisterEq { a, b } => {
                if self.get_register(a) == self.get_register(b) {
                    self.program_counter += 2;
                }
            }
            Instruction::IfRegisterNeq { a, b } => {
                if self.get_register(a) != self.get_register(b) {
                    self.program_counter += 2;
                }
            }
            Instruction::IncrementPc { offset } => {
                self.program_counter += self.get_register(Register::V0) as u16 + offset;
            }
            Instruction::CallRCA { .. } => {
                println!("CallRCA, halted");
                self.halted = true;
            }
        }
    }

    pub fn tick(&mut self) {
        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }

        self.run_instruction();
        self.instructions += 1;

        if self.fps_timer.elapsed() > Duration::from_secs(1) {
            self.fps_timer = Instant::now();
            self.instructions_per_second = self.instructions;
            self.instructions = 0;
        }
    }

    pub fn should_beep(&mut self) -> bool {
        if self.sound_timer > 0 {
            self.sound_timer -= 1;
            if self.sound_timer == 0 {
                return true;
            }
        }
        false
    }
}
