use emu::emu::{Emu, Register, SCREEN_HEIGHT, SCREEN_WIDTH};
use imgui::{im_str, Direction};
use imgui::{Ui, Window};
use std::ops::Deref;
use std::sync::Mutex;

pub(crate) struct EmuUi {
    step: bool,
    pub(crate) debug: bool,

    registers_ui: bool,
    playback_ui: bool,
    memory_ui: bool,
    stack_ui: bool,
    new_rom: Option<Vec<u8>>,
}
impl Default for EmuUi {
    fn default() -> Self {
        Self {
            step: false,
            debug: true,
            registers_ui: false,
            playback_ui: false,
            memory_ui: false,
            stack_ui: false,
            new_rom: None,
        }
    }
}

impl EmuUi {
    pub(crate) fn draw(&mut self, ui: &Ui, emu: &mut Emu, jit: bool) {
        ui.main_menu_bar(|| {
            ui.menu(im_str!("File"), true, || {
                for entry in crate::ROM_DIR.files() {
                    if ui.checkbox(
                        &im_str!("{}", entry.path().file_name().unwrap().to_string_lossy()),
                        &mut false,
                    ) {
                        self.new_rom = Some(entry.contents().to_vec());
                    }
                }
            });

            ui.menu(im_str!("View"), true, || {
                ui.checkbox(im_str!("Registers"), &mut self.registers_ui);
                ui.checkbox(im_str!("Playback"), &mut self.playback_ui);
                ui.checkbox(im_str!("Memory"), &mut self.memory_ui);
                ui.checkbox(im_str!("Stack"), &mut self.stack_ui);
            });

            ui.text(im_str!("IPS: {}", emu.instructions_per_second));
        });

        if self.registers_ui {
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
        }

        if self.stack_ui {
            Window::new(im_str!("Stack")).build(ui, || {
                ui.text(im_str!("Size: {}", emu.stack.len()));
                for i in &emu.stack {
                    ui.text(im_str!("{}", i));
                }
            });
        }

        Window::new(im_str!("Display")).build(ui, || {
            if jit {
                //                let mut r = emu::cl_emu::SCREEN_BUFFER.lock().unwrap();

                // for y in 0..SCREEN_HEIGHT {
                //     let text: String = (0..SCREEN_WIDTH)
                //         .map(|x| r[y][x])
                //         .map(|pixel| if pixel == 0 { "." } else { "#" })
                //         .collect();
                //     ui.text(text);
                // }
            } else {
                for y in 0..SCREEN_HEIGHT {
                    let text: String = (0..SCREEN_WIDTH)
                        .map(|x| emu.screen_buffer[y][x])
                        .map(|pixel| if pixel == 0 { "." } else { "#" })
                        .collect();
                    ui.text(text);
                }
            }
        });

        if self.playback_ui {
            Window::new(im_str!("Playback Controls")).build(ui, || {
                ui.checkbox(im_str!("Halted"), &mut emu.halted);
                if ui.arrow_button(im_str!("Step"), Direction::Right) {
                    self.step = true;
                }
            });
        }

        if self.memory_ui {
            Window::new(im_str!("Memory")).build(ui, || {
                let row_size = 32;

                for y in 0..emu.memory.len() / row_size {
                    let mut row = String::new();

                    for x in 0..row_size {
                        row.push_str(&format!("{:02x} ", emu.memory[y * row_size + x]));
                    }
                    ui.text(row);
                }
            });
        }
    }

    pub(crate) fn run_step(&mut self, emu: &Emu) -> bool {
        if !emu.halted {
            return true;
        } else {
            if self.step {
                self.step = false;
                return true;
            }
        }

        false
    }

    pub(crate) fn new_row(&mut self) -> Option<Vec<u8>> {
        let r = self.new_rom.clone();
        self.new_rom = None;
        r
    }
}
