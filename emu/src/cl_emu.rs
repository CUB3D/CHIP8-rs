use crate::emu;
use crate::emu::{Emu, Instruction, Register};
use core::mem;
use cranelift::codegen::binemit::{CodeOffset, NullTrapSink, TrapSink};
use cranelift::codegen::ir::{FuncRef, SourceLoc};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;

#[derive(Default)]
pub struct JITManager {
    can_be_jitted: HashMap<u16, bool>,
    jitted_functions: HashMap<u16, (*const u8, u16)>,
    jit: JIT,
}

impl JITManager {
    /// Compile a chip8 function at a given address to a native function using cranelift, returns (ptr_to_function, new_pc)
    /// new_pc is the point where the emulator should resume normal interpreted execution (the new value for the program counter)
    pub(crate) fn compile_function(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) {
        let func = self.jit.compile(address, memory);
        self.jitted_functions.insert(address, (func, 0));
    }

    pub fn call_function(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) -> u16 {
        if !self.jitted_functions.contains_key(&address) {
            self.compile_function(address, memory);
        }

        let (func, new_pc) = self.jitted_functions.get(&address).unwrap();
        let code_fn = unsafe { mem::transmute::<_, fn() -> u32>(*func) };
        let _ = code_fn();
        *new_pc
    }

    pub(crate) fn call_function_direct(
        &mut self,
        address: u16,
        memory: &[u8; emu::MEMORY_SIZE],
    ) -> u32 {
        if !self.jitted_functions.contains_key(&address) {
            self.compile_function(address, memory);
        }

        let (func, new_pc) = self.jitted_functions.get(&address).unwrap();
        let code_fn = unsafe { mem::transmute::<_, fn() -> u32>(*func) };
        code_fn()
    }

    pub(crate) fn can_function_be_jitted(
        &mut self,
        address: u16,
        memory: &[u8; emu::MEMORY_SIZE],
    ) -> bool {
        if let Some(x) = self.can_be_jitted.get(&address) {
            return *x;
        } else {
            let mut address = address;
            let mut res = true;
            loop {
                let ins = Emu::read_instruction(address, memory);
                address += 2;

                // Not really in scope for v1
                if matches!(
                    ins,
                    Instruction::DrawSprite { .. }
                        | Instruction::CallSub { .. }
                        | Instruction::SetDelayTimer { .. }
                        | Instruction::SetSoundTimer { .. }
                        | Instruction::GetDelay { .. }
                        | Instruction::IfKeyEq { .. }
                        | Instruction::IfKeyNeq { .. }
                        | Instruction::Unknown
                        | Instruction::SetI { .. }
                        | Instruction::ClearDisplay
                        | Instruction::GetKey { .. }
                ) {
                    res = false;
                    break;
                }

                // Can probably be done in future, needs support for modifying memory/non Vx registers
                if matches!(
                    ins,
                    Instruction::BCD { .. }
                        | Instruction::RegisterLoad { .. }
                        | Instruction::RegisterDump { .. }
                        | Instruction::IncrementIByRegister { .. }
                        | Instruction::IncrementPc { .. }
                ) {
                    res = false;
                    break;
                }

                if ins == Instruction::Return {
                    break;
                }
            }
            self.can_be_jitted.insert(address, res);

            res
        }
    }
}

pub struct EmuTrap {}

impl TrapSink for EmuTrap {
    fn trap(&mut self, _offset: CodeOffset, _srcloc: SourceLoc, _code: TrapCode) {
        println!("Got a trap {} {} {}", _offset, _srcloc, _code);
    }
}

use lazy_static::lazy_static;
use std::cell::RefCell;
use std::ops::Deref;
use std::sync::Mutex;

lazy_static! {
    pub static ref SCREEN_BUFFER: Mutex<[[u8; emu::SCREEN_WIDTH]; emu::SCREEN_HEIGHT]> =
        Mutex::new([[0; emu::SCREEN_WIDTH]; emu::SCREEN_HEIGHT]);
}

lazy_static! {
    pub static ref MEMORY: Mutex<[u8; emu::MEMORY_SIZE]> = Mutex::new([0; emu::MEMORY_SIZE]);
}

pub fn draw_func(address_register: u16, x: u8, y: u8, height: u8) -> u8 {
    let mut sb = SCREEN_BUFFER.lock().unwrap();

    let memory = MEMORY.lock().unwrap();

    let mut modified = 0;

    for dy in 0..height as usize {
        let line = memory[address_register as usize + dy];

        for dx in 0usize..8 {
            let pixel = line & (0b_1000_0000 >> dx);

            let current_pixel = sb
                .get_mut((y as usize + dy) as usize)
                .and_then(|row| row.get_mut((x as usize + dx) as usize));

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

    //TODO: return result for VF

    0
}

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());
        builder.symbol("draw_native", draw_func as *const u8);
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

impl JIT {
    fn compile(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) -> *const u8 {
        *(MEMORY.lock().unwrap()) = *memory;

        let name = "main";

        self.translate(address, memory);

        let id = self
            .module
            .declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        self.module
            .define_function(id, &mut self.ctx, &mut EmuTrap {})
            .unwrap();

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();

        let code = self.module.get_finalized_function(id);

        code
    }

    /// Translate a list of instructions into cranelift ir for execution,
    /// TODO: currently doesn't use the current state of registers so results won't be correct
    ///  However it also doesn't extract the final state either so in effect this is the same as skipping the function execution when invoked from the emulator
    fn translate(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) {
        let byte = Type::int(8).unwrap();
        let short = Type::int(16).unwrap();

        self.ctx.func.signature.returns.push(AbiParam::new(byte));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let draw_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(short));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));

            native_func_sig.returns.push(AbiParam::new(byte));
            let native_func = self
                .module
                .declare_function("draw_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self
                .module
                .declare_func_in_func(native_func, &mut builder.func);
            native_func_ref
        };

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        //
        // TODO: Streamline the API here.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        let mut register_map = HashMap::new();

        let registers = vec![
            Register::V0,
            Register::V1,
            Register::V2,
            Register::V3,
            Register::V4,
            Register::V5,
            Register::V6,
            Register::V7,
            Register::V8,
            Register::V9,
            Register::VA,
            Register::VB,
            Register::VC,
            Register::VD,
            Register::VE,
            Register::VF,
        ];

        for (index, r) in registers.into_iter().enumerate() {
            let var = Variable::new(index);
            register_map.insert(r, var);
            builder.declare_var(var, byte);
        }

        let address_register = {
            let var = Variable::new(16);
            builder.declare_var(var, short);
            var
        };

        let mut blocks = HashMap::new();

        let mut trans = InstructionTranslator {
            byte,
            short,
            builder,
            registers: &register_map,
            address_register: &address_register,
            module: &mut self.module,
            memory,
            blocks: &mut blocks,
            draw_func,
        };

        let mut new_address = address;
        loop {
            let ins = Emu::read_instruction(new_address, memory);
            new_address += 2;
            if trans.translate_instruction(ins) {
                break;
            }
        }

        trans.builder.seal_all_blocks();

        let ret = trans
            .builder
            .use_var(*register_map.get(&Register::V1).unwrap());
        trans.builder.ins().return_(&[ret]);

        trans.builder.finalize();
    }
}

struct InstructionTranslator<'a> {
    byte: types::Type,
    short: types::Type,
    builder: FunctionBuilder<'a>,
    registers: &'a HashMap<Register, Variable>,
    address_register: &'a Variable,
    module: &'a mut JITModule,
    memory: &'a [u8; emu::MEMORY_SIZE],
    blocks: &'a mut HashMap<u16, Block>,
    draw_func: FuncRef,
}

impl<'a> InstructionTranslator<'a> {
    pub fn translate_instruction(&mut self, ins: Instruction) -> bool {
        println!("Translating {:?}", ins);
        match ins {
            Instruction::IncrementRegisterByImmediate { by, dest } => {
                let var = *self.registers.get(&dest).unwrap();
                let by_const = self.builder.ins().iconst(self.byte, by as i64);
                let dest_value = self.builder.use_var(var);
                let new_value = self.builder.ins().iadd(dest_value, by_const);
                self.builder.def_var(var, new_value);
            }
            Instruction::SetRegister { register, value } => {
                let var = *self.registers.get(&register).unwrap();
                let value_const = self.builder.ins().iconst(self.byte, value as i64);
                self.builder.def_var(var, value_const);
            }
            Instruction::IncrementRegisterByRegister { dest, by } => {
                let dest_var = *self.registers.get(&dest).unwrap();
                let by_var = *self.registers.get(&by).unwrap();

                let dest_value = self.builder.use_var(dest_var);
                let by_value = self.builder.use_var(by_var);

                let new_value = self.builder.ins().iadd(dest_value, by_value);
                self.builder.def_var(dest_var, new_value);
            }
            Instruction::SetI { value } => {
                let value_const = self.builder.ins().iconst(self.short, value as i64);
                self.builder.def_var(*self.address_register, value_const);
            }
            // Instruction::CallSub { address } => {
            //     //TODO: push on stack
            //     let current_block = self.builder.current_block().unwrap();
            //     let blk = self.builder.create_block();
            //     self.builder.ins().jump(blk, &[]);
            //
            //     self.builder.switch_to_block(blk);
            //     self.builder.seal_block(blk);
            //
            //     let mut new_address = address;
            //     loop {
            //         let ins = Emu::read_instruction(new_address, self.memory);
            //         new_address += 2;
            //         self.translate_instruction(ins);
            //     }
            //
            //     self.builder.switch_to_block(current_block);
            // }
            Instruction::Jmp { address } => {
                //Have we already done this block
                if let Some(blk) = self.blocks.get(&address) {
                    self.builder.ins().jump(*blk, &[]);
                } else {
                    println!("Translating sub block jmp({})", address);
                    let blk = self.builder.create_block();
                    self.blocks.insert(address, blk);
                    self.builder.ins().jump(blk, &[]);

                    self.builder.switch_to_block(blk);

                    let mut new_address = address;
                    loop {
                        let ins = Emu::read_instruction(new_address, self.memory);
                        new_address += 2;
                        if self.translate_instruction(ins) {
                            break;
                        }
                    }

                    println!("Translating sub block jmp({}) done", address);
                }

                return true;
            }
            Instruction::Return => {
                return true;
            }
            Instruction::CallRCA { address } => {
                // panic!();
                return true;
            }
            Instruction::DrawSprite { x, y, height } => {
                let x_reg = *self.registers.get(&x).unwrap();
                let y_reg = *self.registers.get(&y).unwrap();

                let arg_address_reg = self.builder.use_var(*self.address_register);
                let arg_x = self.builder.use_var(x_reg);
                let arg_y = self.builder.use_var(y_reg);
                let arg_h = self.builder.ins().iconst(self.byte, height as i64);

                let ca = self
                    .builder
                    .ins()
                    .call(self.draw_func, &[arg_address_reg, arg_x, arg_y, arg_h]);

                let res = self.builder.inst_results(ca);
                println!("Draw res count: {:?}", res.len());

                // self.builder.ins().resumable_trap(TrapCode::Interrupt);
            }
            Instruction::IfNeq { b, a } => {
                println!("IfNeq always true");
            }
            Instruction::IfEq { a, b } => {
                println!("IfEq always true")
            }
            Instruction::IfRegisterEq { a, b } => {
                println!("IfRegisterEq always true")
            }
            Instruction::IfRegisterNeq { a, b } => {
                println!("IfRegisterNeq always true")
            }
            Instruction::CallSub { .. }
            | Instruction::SetRegisterRegister { .. }
            | Instruction::SetSoundTimer { .. }
            | Instruction::SetDelayTimer { .. }
            | Instruction::DecrementRegisterByRegister { .. }
            | Instruction::ShiftLeft { .. }
            | Instruction::ShiftRight { .. }
            | Instruction::RegisterDump { .. }
            | Instruction::RegisterLoad { .. } => {
                println!("Stub!");
            }
            Instruction::BitwiseOr { a, b } => {
                let a_reg = *self.registers.get(&a).unwrap();
                let b_reg = *self.registers.get(&b).unwrap();

                let a_val = self.builder.use_var(a_reg);
                let b_val = self.builder.use_var(b_reg);

                let res = self.builder.ins().bor(a_val, b_val);
                self.builder.def_var(a_reg, res);
            }
            Instruction::BitwiseAnd { a, b } => {
                let a_reg = *self.registers.get(&a).unwrap();
                let b_reg = *self.registers.get(&b).unwrap();

                let a_val = self.builder.use_var(a_reg);
                let b_val = self.builder.use_var(b_reg);

                let res = self.builder.ins().band(a_val, b_val);
                self.builder.def_var(a_reg, res);
            }
            Instruction::BitwiseXor { a, b } => {
                let a_reg = *self.registers.get(&a).unwrap();
                let b_reg = *self.registers.get(&b).unwrap();

                let a_val = self.builder.use_var(a_reg);
                let b_val = self.builder.use_var(b_reg);

                let res = self.builder.ins().bxor(a_val, b_val);
                self.builder.def_var(a_reg, res);
            }
            _ => {
                println!("Can't jit {:?}", ins);
                // let r = self.builder.ins().iconst(self.byte, 0 as i64);
                // self.builder.ins().return_(&[r]);
                return true;
            }
        }

        false
    }
}

#[test]
pub fn jit_test_big_loop() {
    let mut e = Emu::new();
    e.load_rom(include_bytes!("../benches/test_big_loop.ch8"));
    e.jit.call_function(512, &e.memory);
}

#[test]
pub fn jit_test_opcode() {
    let mut e = Emu::new();
    e.load_rom(include_bytes!("../../desktop/rom/test_opcode.ch8"));
    e.jit.call_function(512, &e.memory);
}

#[test]
pub fn jit_test_simple() {
    let mut e = Emu::new();
    e.load_rom(include_bytes!("../tests/roms/test_jit1.ch8"));
    let r = e.jit.call_function_direct(512, &e.memory);
    assert_eq!(r, 3);
}

#[test]
pub fn jit_test_jmp() {
    let mut e = Emu::new();
    e.load_rom(include_bytes!("../tests/roms/test_jit_jmp.ch8"));
    let r = e.jit.call_function_direct(512, &e.memory);
    assert_eq!(r, 3);
}

// #[test]
// pub fn can_jit_addr_loop() {
//     let mut e = Emu::new();
//     e.load_rom(include_bytes!("../benches/test_big_addr_loop.ch8"));
//     assert_eq!(true, e.jit.can_function_be_jitted(0x202, &e.memory));
// }
