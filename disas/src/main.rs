use emu::emu::{Emu, Instruction};
use std::fs::File;
use std::io::Read;

fn main() {
    let mut e = Emu::new();

    let mut f = File::open(std::env::args().skip(1).nth(0).unwrap()).unwrap();
    let mut b = Vec::new();
    f.read_to_end(&mut b);
    e.load_rom(&b);

    let mut pc = 512;
    while let Some(i) = Emu::read_instruction(pc, &e.memory) {
        if i == (Instruction::CallRCA { address: 0 }) {
            return;
        }

        println!("{:X} - {:0} - {:?}", pc, pc, i);
        pc += 2;
    }
}
