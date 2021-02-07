use criterion::{black_box, criterion_group, criterion_main, Criterion};

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

use emu::emu::Emu;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("test_jit_perf", |b| {
        let mut emu = Emu::new();
        b.iter(|| {
            emu.load_rom(include_bytes!("./test_big_loop.ch8"));
            while !emu.halted {
                emu.run_instruction();
            }
        })
    });

    c.bench_function("test_jit_addr", |b| {
        let mut emu = Emu::new();
        b.iter(|| {
            emu.load_rom(include_bytes!("./test_big_addr_loop.ch8"));
            while !emu.halted {
                emu.run_instruction();
            }
        })
    });
}
