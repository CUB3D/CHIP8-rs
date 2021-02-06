use crate::emu::{Emu, Register};
use imgui::im_str;
use imgui::{Ui, Window};

pub(crate) fn draw(ui: &Ui, emu: &Emu) {
    Window::new(im_str!("Registers")).build(ui, || {
        for (i, val) in emu.registers.iter().enumerate() {
            let r = Register::from(i as u8);
            ui.text(im_str!("{:?}: {}", r, val));
        }
        ui.text(im_str!("Address: {}", emu.address_register));
        ui.text(im_str!("PC: {}", emu.program_counter));
        ui.text(im_str!("Sound Timer: {}", emu.sound_timer));
        ui.text(im_str!("Delay Timer: {}", emu.delay_timer));
    });

    Window::new(im_str!("Stack")).build(ui, || {
        ui.text(im_str!("Size: {}", emu.stack.len()));
        for i in &emu.stack {
            ui.text(im_str!("{}", i));
        }
    });

    Window::new(im_str!("Display")).build(ui, || {
        for y in 0..crate::emu::SCREEN_HEIGHT {
            for x in 0..crate::emu::SCREEN_WIDTH {
                if emu.screen_buffer[y * crate::emu::SCREEN_WIDTH + x] != 0 {
                    ui.text("#");
                } else {
                    ui.text(".");
                }
                ui.same_line(10.0 * x as f32);
            }
            ui.new_line();
        }
    });
}
