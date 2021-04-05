use emu::emu::{Emu, Instruction, InstructionExecution};
use std::fs::File;
use std::io::Read;

fn main() {
    let mut e = Emu::new();

    let mut f = File::open(std::env::args().skip(1).nth(0).unwrap()).unwrap();
    let mut b = Vec::new();
    f.read_to_end(&mut b);
    e.load_rom(&b);
    e.memory.jit_enabled = false;

    let mut pc = 512;
    while let Some(i) = e.memory.read_instruction(pc) {
        let i = match i {
            InstructionExecution::Emulated(i) => i,
            InstructionExecution::Native(_) => unimplemented!(),
        };
        if i == (Instruction::CallRCA { address: 0 }) {
            return;
        }

        println!("{:X} - {:0} - {:?}", pc, pc, i);
        pc += 2;
    }
}
