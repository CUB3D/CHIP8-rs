use crate::emu::{Emu, PlatformBackend};
use crate::ui::{init, System};
use glium::glutin::event::VirtualKeyCode;

pub mod emu;
pub mod emu_ui;
pub mod ui;

fn main() {
    let system = init(file!());

    let mut emu = Emu::new();

    emu.load_rom(include_bytes!("../rom/pong.rom"));

    system.main_loop(move |_, ui| {
        emu.tick();

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

        emu_ui::draw(ui, &emu);
    });
}
