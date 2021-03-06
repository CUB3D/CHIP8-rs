use crate::emu::{Emu, PlatformBackend};
use crate::emu_ui::EmuUi;
use crate::ui::{init, System};
use glium::glutin::event::VirtualKeyCode;
use rodio::Source;
use std::io::Cursor;
use include_dir::{include_dir, Dir};
use std::time::{Instant, Duration};

pub mod emu;
pub mod emu_ui;
pub mod ui;

const ROM_DIR: Dir = include_dir!("rom");


fn main() {
    let system = init(file!());

    let (stream, stream_handle) = rodio::OutputStream::try_default().unwrap();
    let sound_bytes = include_bytes!("../beep.wav");

    let mut emu = Emu::new();
    let mut emu_ui = EmuUi::default();

    let mut tick_timer = Instant::now();

    emu.load_rom(ROM_DIR.files().first().expect("No roms in rom dir").contents());

    system.main_loop(move |_, ui| {
        let keys = ui.io().keys_down;
        emu.keys = [
            keys[VirtualKeyCode::Key0 as usize],
            keys[VirtualKeyCode::Key1 as usize],
            keys[VirtualKeyCode::Key2 as usize],
            keys[VirtualKeyCode::Key3 as usize],
            keys[VirtualKeyCode::Q as usize],
            keys[VirtualKeyCode::W as usize],
            keys[VirtualKeyCode::E as usize],
            keys[VirtualKeyCode::R as usize],
            keys[VirtualKeyCode::A as usize],
            keys[VirtualKeyCode::S as usize],
            keys[VirtualKeyCode::D as usize],
            keys[VirtualKeyCode::F as usize],
            keys[VirtualKeyCode::Z as usize],
            keys[VirtualKeyCode::X as usize],
            keys[VirtualKeyCode::C as usize],
            keys[VirtualKeyCode::V as usize],
        ];

        emu_ui.draw(ui, &emu);


        if tick_timer.elapsed() > Duration::from_millis(1000 / 120) && emu_ui.run_step() {
            tick_timer = Instant::now();
            emu.tick();
        }

        if emu.should_beep() {
            let source = rodio::Decoder::new(Cursor::new(sound_bytes)).unwrap();
            stream_handle.play_raw(source.convert_samples());
        }

        if let Some(data) = emu_ui.new_row() {
            emu = Emu::new();
            emu.load_rom(&data);
        }
    });
}
