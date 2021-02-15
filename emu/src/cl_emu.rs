use crate::emu;
use crate::emu::{Emu, Instruction, Register};
use crate::graph::*;
use core::mem;
use cranelift::codegen::binemit::{CodeOffset, NullTrapSink, TrapSink};
use cranelift::codegen::ir::{FuncRef, SourceLoc, Inst};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use lazy_static::lazy_static;
use rodio::{OutputStream, OutputStreamHandle, Source};
use std::cell::RefCell;
use std::collections::{HashMap, BTreeMap};
use std::fs::File;
use std::io::{Cursor, Write};
use std::ops::Deref;
use std::slice;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use cranelift_frontend::Switch;

pub struct EmuTrap {}

impl TrapSink for EmuTrap {
    fn trap(&mut self, _offset: CodeOffset, _srcloc: SourceLoc, _code: TrapCode) {
        // println!("Got a trap {} {} {}", _offset, _srcloc, _code);
    }
}

lazy_static! {
    pub static ref SCREEN_BUFFER: Mutex<[[u8; emu::SCREEN_WIDTH]; emu::SCREEN_HEIGHT]> =
        Mutex::new([[0; emu::SCREEN_WIDTH]; emu::SCREEN_HEIGHT]);
}

lazy_static! {
    pub static ref MEMORY: Mutex<[u8; emu::MEMORY_SIZE]> = Mutex::new([0; emu::MEMORY_SIZE]);
}

lazy_static! {
    pub static ref KEYS: Mutex<[bool; 16]> = Mutex::new([false; 16]);
}

lazy_static! {
    pub static ref STACK: Mutex<Vec<u16>> = Mutex::new(Vec::new());
}

pub fn push(v: u16) {
    STACK.lock().unwrap().push(v);
}

pub fn pop() -> u16 {
    STACK.lock().unwrap().pop().unwrap()
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

    modified
}

pub fn rand(modulus: u8) -> u8 {
    let r = 42; // rand::random::<u8>() ;
    r % modulus
}

pub fn memset(addr: u16, val: u8) {
    MEMORY.lock().unwrap()[addr as usize] = val;
}

pub fn memread(addr: u16) -> u8 {
    MEMORY.lock().unwrap()[addr as usize]
}

pub fn sleep() {
    let tps = 500;
    std::thread::sleep(Duration::from_millis(1000 / tps));
}

pub fn clear_display() {
    *SCREEN_BUFFER.lock().unwrap() = [[0; emu::SCREEN_WIDTH]; emu::SCREEN_HEIGHT];
}

pub fn wait_for_key() -> u8 {
    loop {
        if let Some((i, _k)) = KEYS.lock().unwrap().iter().enumerate().find(|(i, k)| **k) {
            return i as u8;
        }
    }
}

pub fn is_key_pressed(key: u8) -> u8 {
    assert!(key < 16);

    if KEYS.lock().unwrap()[key as usize] {
        1
    } else {
        0
    }
}

pub fn play_sound() {
    let (stream, stream_handle) = rodio::OutputStream::try_default().unwrap();
    let sound_bytes = include_bytes!("../../desktop/beep.wav");
    let source = rodio::Decoder::new(Cursor::new(sound_bytes)).unwrap();
    stream_handle.play_raw(source.convert_samples());
}

pub fn dump_state(
    pc: u16,
    r0: u8,
    r1: u8,
    r2: u8,
    r3: u8,
    r4: u8,
    r5: u8,
    r6: u8,
    r7: u8,
    r8: u8,
    r9: u8,
    r10: u8,
    r11: u8,
    r12: u8,
    r13: u8,
    r14: u8,
    r15: u8,
) {
    let registers = &[
        r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15,
    ];
    println!("Registers: {:?} PC: {}", registers, pc)
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
        builder.symbol("rand_native", draw_func as *const u8);
        builder.symbol("memset_native", memset as *const u8);
        builder.symbol("memread_native", memread as *const u8);
        builder.symbol("sleep_native", sleep as *const u8);
        builder.symbol("clear_display_native", clear_display as *const u8);
        builder.symbol("wait_for_key_native", wait_for_key as *const u8);
        builder.symbol("is_key_pressed_native", is_key_pressed as *const u8);
        builder.symbol("play_sound_native", play_sound as *const u8);

        builder.symbol("native_push", push as *const u8);
        builder.symbol("native_pop", pop as *const u8);


        builder.symbol("dump_state", dump_state as *const u8);

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
    pub fn call_function_direct(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) -> u32 {
        println!("Compiling");
        let func = self.compile(address, memory);
        println!("Compile done");
        let code_fn = unsafe { mem::transmute::<_, fn() -> u32>(func) };
        code_fn()
    }

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
    // TODO: use a jump table for jmp/callsub and a real stack (possibilly proxy out to a native Vec for poc), also make pc a register so we can use it as an index in the jumptable to know which block to go to
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
            let draw_func = self
                .module
                .declare_function("draw_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self
                .module
                .declare_func_in_func(draw_func, &mut builder.func);
            native_func_ref
        };

        let rand_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(byte));

            native_func_sig.returns.push(AbiParam::new(byte));
            let rand_func = self
                .module
                .declare_function("rand_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self
                .module
                .declare_func_in_func(rand_func, &mut builder.func);
            native_func_ref
        };

        let memset_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(short));
            native_func_sig.params.push(AbiParam::new(byte));

            let memset_func = self
                .module
                .declare_function("memset_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self
                .module
                .declare_func_in_func(memset_func, &mut builder.func);
            native_func_ref
        };

        let memread_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(short));

            native_func_sig.returns.push(AbiParam::new(byte));

            let memread_func = self
                .module
                .declare_function("memread_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self
                .module
                .declare_func_in_func(memread_func, &mut builder.func);
            native_func_ref
        };

        let sleep_func = {
            let mut native_func_sig = self.module.make_signature();

            let func = self
                .module
                .declare_function("sleep_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let clear_display_func = {
            let mut native_func_sig = self.module.make_signature();

            let func = self
                .module
                .declare_function("clear_display_func", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let wait_for_key_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.returns.push(AbiParam::new(byte));

            let func = self
                .module
                .declare_function("wait_for_key_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let is_key_pressed_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(byte));

            native_func_sig.returns.push(AbiParam::new(byte));

            let func = self
                .module
                .declare_function("is_key_pressed_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let dump_state_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(short));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));
            native_func_sig.params.push(AbiParam::new(byte));

            let func = self
                .module
                .declare_function("dump_state", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let play_sound_native_func = {
            let mut native_func_sig = self.module.make_signature();

            let func = self
                .module
                .declare_function("play_sound_native", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let push_native_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.params.push(AbiParam::new(short));

            let func = self
                .module
                .declare_function("native_push", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
            native_func_ref
        };

        let pop_native_func = {
            let mut native_func_sig = self.module.make_signature();
            native_func_sig.returns.push(AbiParam::new(short));

            let func = self
                .module
                .declare_function("native_pop", Linkage::Import, &native_func_sig)
                .unwrap();
            let native_func_ref = self.module.declare_func_in_func(func, &mut builder.func);
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

        let delay_timer = {
            let var = Variable::new(17);
            builder.declare_var(var, byte);
            var
        };

        let sound_timer = {
            let var = Variable::new(18);
            builder.declare_var(var, byte);
            var
        };

        let mut blocks = HashMap::new();

        let mut gm = GraphManager::default();

        let mut trans = InstructionTranslator {
            byte,
            short,
            builder,
            registers: &register_map,
            address_register: &address_register,
            delay_timer,
            sound_timer,
            module: &mut self.module,
            memory,
            blocks: &mut blocks,
            draw_func,
            rand_func,
            memset_func,
            memread_func,
            sleep_func,
            clear_display_func,
            wait_for_key_func,
            is_key_pressed_func,
            stack: Vec::new(),
            dump_state_func,
            play_sound_native_func,
            graph: &mut gm,
            push_func,
        };

        let mut blocks = BTreeMap::new();
        let mut callsub_placeholders = Vec::new();

        let mut new_address = address;
        loop {
            let ins = Emu::read_instruction(new_address, memory).unwrap();
            if trans.translate_instruction(ins, new_address, &mut blocks, &mut callsub_placeholders) {
                new_address += 2;
                break;
            }
            new_address += 2;
        }

        // Use the blocks map to create a jump table
        let mut jtd = JumpTableData::new();
        for (index, blk) in trans.blocks.iter() {
            jtd.push_entry(*blk);
        }

        let jt = trans.builder.create_jump_table(jtd);

        // Take all the callsub blocks and translate them into a push + jumptable lookup
        for blk in callsub_placeholders {
            trans.builder.switch_to_block(blk);

            // Get the next pc
            let (blk_pc, _) = trans.blocks.iter().find(|(pc, b)| **b == blk).unwrap();
            let next_pc_imm = trans.builder.ins().iconst(short, *blk_pc + 2);
            // Push the return address
            trans.push(next_pc_imm);

            // Jump to the block using the jump table
            let instruction = Emu::read_instruction(*blk_pc, memory).unwrap();
            assert_eq!(matches!(instruction, Instruction::CallSub { ..}), true);
            if let (Instruction::CallSub { address }) = instruction {
                // Find the position of the callsub address in the blocks table, this will be the same as its index in the jump table
                let blk_pos = trans.blocks.iter().position(|(p, b)| *p == address).unwrap();
                let blk_pos_val = trans.builder.ins().iconst(short, blk_pos);
                // If this block is non existant (should panic from the unwraps above, go to self (loop))
                let default_block = blk;
                trans.builder.ins().br_table(blk_pos_val, default_block, jt);
            } else {
                panic!("This should be a callsub");
            }
        }

        trans.builder.seal_all_blocks();

        let ret = trans
            .builder
            .use_var(*register_map.get(&Register::V1).unwrap());
        trans.builder.ins().return_(&[ret]);

        let mut f = File::create("example-jit.dot").unwrap();
        trans.builder.finalize();

        gm.render(&mut f);
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
    delay_timer: Variable,
    sound_timer: Variable,
    rand_func: FuncRef,
    memset_func: FuncRef,
    memread_func: FuncRef,
    sleep_func: FuncRef,
    clear_display_func: FuncRef,
    wait_for_key_func: FuncRef,
    is_key_pressed_func: FuncRef,
    play_sound_native_func: FuncRef,
    stack: Vec<u16>,
    dump_state_func: FuncRef,
    graph: &'a mut GraphManager,
    push_func: FuncRef,
}

impl<'a> InstructionTranslator<'a> {
    pub fn set_register(&mut self, reg: Register, value: Value) {
        let var = *self.registers.get(&reg).unwrap();
        self.builder.def_var(var, value);
    }

    pub fn get_register(&mut self, reg: Register) -> Value {
        let var = *self.registers.get(&reg).unwrap();
        self.builder.use_var(var)
    }

    pub fn push(&mut self, val: Value) {
        let ca = self.builder.ins().call(self.push_func, &[val]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 0);
    }

    pub fn memset(&mut self, mem_loc: Value, val: Value) {
        let ca = self.builder.ins().call(self.memset_func, &[mem_loc, val]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 0);
    }

    pub fn memread(&mut self, mem_loc: Value) -> Value {
        let ca = self.builder.ins().call(self.memread_func, &[mem_loc]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 1);

        res[0]
    }

    pub fn sleep(&mut self) {
        let ca = self.builder.ins().call(self.sleep_func, &[]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 0);
    }

    pub fn clear_display(&mut self) {
        let ca = self.builder.ins().call(self.clear_display_func, &[]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 0);
    }

    pub fn wait_for_key(&mut self) -> Value {
        let ca = self.builder.ins().call(self.wait_for_key_func, &[]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 1);

        res[0]
    }

    pub fn is_key_pressed(&mut self, key_id: Value) -> Value {
        let ca = self.builder.ins().call(self.is_key_pressed_func, &[key_id]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 1);

        res[0]
    }

    pub fn play_sound(&mut self) {
        let ca = self.builder.ins().call(self.play_sound_native_func, &[]);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 0);
    }

    pub fn dec_delay_timer(&mut self) {
        let one = self.builder.ins().iconst(self.byte, 1 as i64);

        let delay_val = self.builder.use_var(self.delay_timer);

        let true_block = self.builder.create_block();
        let false_block = self.builder.create_block();

        /*
           if delay_val > 0 {
               delay_val = delay_val - 1;
           }
        */
        self.builder.ins().brnz(delay_val, true_block, &[]);
        self.builder.ins().jump(false_block, &[]);
        self.builder.switch_to_block(true_block);
        let new_delay = self.builder.ins().isub(delay_val, one);
        self.builder.def_var(self.delay_timer, new_delay);
        self.builder.ins().jump(false_block, &[]);
        self.builder.switch_to_block(false_block);
    }

    pub fn play_sound_if_timer_is_zero(&mut self) {
        let true_block = self.builder.create_block();
        let false_block = self.builder.create_block();

        let sound_val = self.builder.use_var(self.sound_timer);

        /*
           if sound_val == 0 {
               play_sound();
           }
        */
        self.builder.ins().brz(sound_val, true_block, &[]);
        self.builder.ins().jump(false_block, &[]);
        self.builder.switch_to_block(true_block);
        self.play_sound();
        self.builder.ins().jump(false_block, &[]);
        self.builder.switch_to_block(false_block);
    }

    pub fn dec_sound_timer(&mut self) {
        let one = self.builder.ins().iconst(self.byte, 1 as i64);

        let sound_val = self.builder.use_var(self.sound_timer);

        let true_block = self.builder.create_block();
        let false_block = self.builder.create_block();

        /*
           if sound_val > 0 {
               sound_val = sound_val - 1;
           }
        */

        self.builder.ins().brnz(sound_val, true_block, &[]);
        self.builder.ins().jump(false_block, &[]);
        self.builder.switch_to_block(true_block);
        let new_sound = self.builder.ins().isub(sound_val, one);
        self.builder.def_var(self.sound_timer, new_sound);
        self.builder.ins().jump(false_block, &[]);
        self.builder.switch_to_block(false_block);
    }

    pub fn dec_timers(&mut self) {
        self.dec_delay_timer();
        self.dec_sound_timer();

        self.sleep();
    }

    pub fn dump_state(&mut self, pc: u16) {
        let pc = self.builder.ins().iconst(types::I16, pc as i64);

        let mut args = vec![pc];

        for x in 0..16 {
            let r = self.get_register(Register::from(x));
            args.push(r);
        }

        let ca = self.builder.ins().call(self.dump_state_func, &args);

        let res = self.builder.inst_results(ca);
        assert_eq!(res.len(), 0);
    }

    pub fn translate_instruction(&mut self, ins: Instruction, pc: u16, blocks: &mut BTreeMap<u16, Block>, callsub_placeholder: &mut Vec<Block>) -> bool {
        let this_node = self.graph.add_node(format!("{}: {:?}", pc, ins));
        self.graph.link(&self.graph.parent(&this_node), &this_node);

        println!("Translating {:?}", ins);

        // If this block is old then just use the existing one
        let blk = self.blocks.get(&pc).copied();
        if let Some(blk) = blk {
            let new_blk = self.builder.create_block();
            // println!("Using cached instruction block");
            self.dump_state(pc);

            self.dec_timers();
            self.builder.ins().jump(blk, &[]);
            self.builder.switch_to_block(new_blk);

            return true;
        }

        self.dump_state(pc);

        self.dec_timers();

        // println!("Creating jump to new instruction block");
        let blk = self.builder.create_block();
        self.blocks.insert(pc, blk);
        self.builder.ins().jump(blk, &[]);
        self.builder.switch_to_block(blk);

        match ins {
            Instruction::SetRegister { register, value } => {
                let var = *self.registers.get(&register).unwrap();
                let value_const = self.builder.ins().iconst(self.byte, value as i64);
                self.builder.def_var(var, value_const);
            }
            Instruction::IncrementRegisterByImmediate { by, dest } => {
                let dest_val = self.get_register(dest);
                let by_const = self.builder.ins().iconst(self.byte, by as i64);
                let new_value = self.builder.ins().iadd(dest_val, by_const);
                self.set_register(dest, new_value);
            }
            Instruction::IncrementRegisterByRegister { dest, by } => {
                let dest_val = self.get_register(dest);
                let by_val = self.get_register(by);

                // Do the addition in 64bit
                let dest_val_sextend = self.builder.ins().sextend(types::I64, dest_val);
                let by_val_sextend = self.builder.ins().sextend(types::I64, by_val);
                let r = self.builder.ins().iadd(dest_val_sextend, by_val_sextend);

                // Do the addition in 8 bit
                let new_value = self.builder.ins().iadd(dest_val, by_val);

                // If the numbers aren't equal then we overflowed in 8bit
                let new_value_sextend = self.builder.ins().sextend(types::I64, new_value);
                let overflow = self
                    .builder
                    .ins()
                    .icmp(IntCC::NotEqual, r, new_value_sextend);
                let one = self.builder.ins().iconst(types::I8, 1 as i64);
                let zero = self.builder.ins().iconst(types::I8, 0 as i64);
                let overflow_flag = self.builder.ins().select(overflow, one, zero);

                self.set_register(dest, new_value);
                self.set_register(Register::VF, overflow_flag);
            }
            Instruction::DecrementRegisterByRegister { a, b } => {
                let dest_value = self.get_register(a);
                let b_value = self.get_register(b);

                let new_value = self.builder.ins().isub(dest_value, b_value);

                let borrow = self
                    .builder
                    .ins()
                    .icmp(IntCC::UnsignedLessThan, dest_value, b_value);

                // Convert the borrow flag into a byte, 0 if there is no overflow and a 1 if there is
                let one = self.builder.ins().iconst(self.byte, 1 as i64);
                let zero = self.builder.ins().iconst(self.byte, 0 as i64);
                let borrow_flag = self.builder.ins().select(borrow, zero, one);

                self.set_register(a, new_value);
                self.set_register(Register::VF, borrow_flag);
            }
            Instruction::SetRegisterRegister { a, b } => {
                let b_value = self.get_register(b);
                self.set_register(a, b_value);
            }
            Instruction::SetI { value } => {
                let value_const = self.builder.ins().iconst(self.short, value as i64);
                self.builder.def_var(*self.address_register, value_const);
            }
            Instruction::CallSub { address } => {
                // self.stack.push(pc + 2);
                // println!("Translating sub block callsub({})", address);
                builder.ins().nop();
                callsub_placeholder.push(blk);

                // let true_node = self.graph.add_node(format!("{} - SUB", pc));
                // self.graph.link(&this_node, &true_node);
                //
                let mut new_address = address;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    //TODO: we should indicate that this instruction shouldn't follow on from the current one, we only want to make sure the block is in the blocks table

                    //tODO: should we do a pass over the whole binary and either encode
                    // each instruction into a block or put it in a list to do in a second pass because it needs to be able to do jumps, then all jumps can use the jumptable and we don't need any of the hideous recursion or translate instruction loops
                    if self.translate_instruction(ins, new_address, &mut switch_table) {
                        println!("Found a terminating block before a return, ignoring, this might cause bugs");
                        break;
                    }
                    new_address += 2;
                }

                // println!("Translating sub block callsub({}) done", address);
            }
            Instruction::Jmp { address } => {
                // println!("Translating sub block jmp({})", address);

                let mut new_address = address;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address, &mut blocks) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                // println!("Translating sub block jmp({}) done", address);

                return true;
            }
            Instruction::Return => {
                // let new_pc = self.stack.pop().unwrap();

                // let mut new_address = new_pc;
                // loop {
                //     let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                //     if self.translate_instruction(ins, new_address) {
                //         break;
                //     }
                //     new_address += 2;
                // }

                return true;
            }
            Instruction::CallRCA { address } => {
                return true;
                // println!("CallRCA({}) not supported", address);
            }
            Instruction::Rand { dest, modulus } => {
                let mod_arg = self.builder.ins().iconst(self.byte, modulus as i64);

                let ca = self.builder.ins().call(self.rand_func, &[mod_arg]);

                let res = self.builder.inst_results(ca);
                assert_eq!(res.len(), 1);

                self.set_register(dest, res[0]);
            }
            Instruction::DrawSprite { x, y, height } => {
                let arg_x = self.get_register(x);
                let arg_y = self.get_register(y);

                let arg_address_reg = self.builder.use_var(*self.address_register);
                let arg_h = self.builder.ins().iconst(self.byte, height as i64);

                let ca = self
                    .builder
                    .ins()
                    .call(self.draw_func, &[arg_address_reg, arg_x, arg_y, arg_h]);

                let res = self.builder.inst_results(ca);
                assert_eq!(res.len(), 1);

                self.set_register(Register::VF, res[0]);
            }
            Instruction::IfNeq { b, a } => {
                let true_block = self.builder.create_block();
                let false_block = self.builder.create_block();

                let a_val = self.get_register(a);
                let b_val = self.builder.ins().iconst(self.byte, b as i64);

                // If we are true then go to the true block
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, b_val);

                // If we are true then go to the true block
                self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(true_block);
                self.translate_instruction(
                    Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    pc + 2,
                    &mut blocks
                );
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(false_block);
                let mut new_address = pc + 4;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address, &mut blocks) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                return true;
            }
            Instruction::IfEq { a, b } => {
                let true_block = self.builder.create_block();
                let false_block = self.builder.create_block();

                let a_val = self.get_register(a);
                let b_val = self.builder.ins().iconst(self.byte, b as i64);

                // If we are true then go to the true block
                let cmp = self.builder.ins().icmp(IntCC::Equal, a_val, b_val);

                // If we are true then go to the true block
                self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(true_block);

                let true_node = self.graph.add_node(format!("{} - True", pc));
                self.graph.link(&this_node, &true_node);

                self.translate_instruction(
                    Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    pc + 2,
                    &mut blocks
                );
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(false_block);

                let false_node = self.graph.add_node(format!("{} - False", pc));
                self.graph.link(&this_node, &false_node);
                self.graph
                    .link(&self.graph.parent(&false_node), &false_node);

                let mut new_address = pc + 4;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address, &mut blocks) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                return true;
            }
            Instruction::IfRegisterEq { a, b } => {
                let true_block = self.builder.create_block();
                let false_block = self.builder.create_block();

                let a_val = self.get_register(a);
                let b_val = self.get_register(b);

                // If we are true then go to the true block
                let cmp = self.builder.ins().icmp(IntCC::Equal, a_val, b_val);

                // If we are true then go to the true block
                self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(true_block);
                self.translate_instruction(
                    Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    pc + 2,
                    &mut blocks
                );
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(false_block);
                let mut new_address = pc + 4;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address, &mut blocks) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                return true;
            }
            Instruction::IfRegisterNeq { a, b } => {
                let true_block = self.builder.create_block();
                let false_block = self.builder.create_block();

                let a_val = self.get_register(a);
                let b_val = self.get_register(b);

                // If we are true then go to the true block
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, b_val);

                // If we are true then go to the true block
                self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(true_block);
                self.translate_instruction(
                    Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    pc + 2,
                    &mut blocks
                );
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(false_block);
                let mut new_address = pc + 4;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address, &mut blocks) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                return true;
            }
            Instruction::SetDelayTimer { value } => {
                let a_reg = *self.registers.get(&value).unwrap();
                let a_val = self.builder.use_var(a_reg);
                self.builder.def_var(self.delay_timer, a_val);
            }
            Instruction::SetSoundTimer { value } => {
                let a_reg = *self.registers.get(&value).unwrap();
                let a_val = self.builder.use_var(a_reg);
                self.builder.def_var(self.sound_timer, a_val);
            }
            Instruction::GetDelay { dest } => {
                let dest_reg = *self.registers.get(&dest).unwrap();

                let delay_val = self.builder.use_var(self.delay_timer);

                self.builder.def_var(dest_reg, delay_val);
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
            Instruction::ShiftLeft { a } => {
                let a_val = self.get_register(a);
                let high_bit = self.builder.ins().band_imm(a_val, 0b10000);

                let r = self.builder.ins().ishl_imm(a_val, 1);

                self.set_register(a, r);
                self.set_register(Register::VF, high_bit);
            }
            Instruction::ShiftRight { a } => {
                let a_val = self.get_register(a);
                let low_bit = self.builder.ins().band_imm(a_val, 0x01);

                let r = self.builder.ins().ushr_imm(a_val, 1);

                self.set_register(a, r);
                self.set_register(Register::VF, low_bit);
            }
            Instruction::RegisterDump { dest } => {
                for i in 0..=dest.value() {
                    let address_val = self.builder.use_var(*self.address_register);
                    let i_val = self.builder.ins().iconst(self.short, i as i64);
                    let mem_loc = self.builder.ins().iadd(address_val, i_val);
                    let r_val = self.get_register(Register::from(i));

                    self.memset(mem_loc, r_val);
                }

                let new_addr = self
                    .builder
                    .ins()
                    .iconst(self.short, dest.value() as i64 + 1);
                self.builder.def_var(*self.address_register, new_addr);
            }
            Instruction::RegisterLoad { dest } => {
                for i in 0..=dest.value() {
                    let address_val = self.builder.use_var(*self.address_register);
                    let i_val = self.builder.ins().iconst(self.short, i as i64);
                    let mem_loc = self.builder.ins().iadd(address_val, i_val);
                    let mem_val = self.memread(mem_loc);
                    self.set_register(Register::from(i), mem_val);
                }
                let new_addr = self
                    .builder
                    .ins()
                    .iconst(self.short, dest.value() as i64 + 1);
                self.builder.def_var(*self.address_register, new_addr);
            }
            Instruction::BCD { src } => {
                let v = self.get_register(src);

                let hudreds = self.builder.ins().udiv_imm(v, 100);
                let tens = {
                    let div = self.builder.ins().udiv_imm(v, 10);
                    self.builder.ins().urem_imm(div, 10)
                };
                let units = self.builder.ins().urem_imm(v, 10);

                let address_val = self.builder.use_var(*self.address_register);
                let address_val_offset_1 = self.builder.ins().iadd_imm(address_val, 1);
                let address_val_offset_2 = self.builder.ins().iadd_imm(address_val, 2);

                self.memset(address_val, hudreds);
                self.memset(address_val_offset_1, tens);
                self.memset(address_val_offset_2, units);
            }
            Instruction::ClearDisplay => {
                self.clear_display();
            }
            Instruction::IncrementIByRegister { by } => {
                let address_current = self.builder.use_var(*self.address_register);
                let offset = self.get_register(by);
                let offset_sign_extend = self.builder.ins().sextend(self.short, offset);
                let new_address = self.builder.ins().iadd(address_current, offset_sign_extend);
                self.builder.def_var(*self.address_register, new_address);
            }
            Instruction::GetKey { dest } => {
                let key_code = self.wait_for_key();
                self.set_register(dest, key_code);
            }

            Instruction::IfKeyEq { comp } => {
                let true_block = self.builder.create_block();
                let false_block = self.builder.create_block();

                let key_id_val = self.get_register(comp);
                let is_pressed = self.is_key_pressed(key_id_val);
                let one = self.builder.ins().iconst(self.byte, 1 as i64);

                // If we are true then go to the true block
                let cmp = self.builder.ins().icmp(IntCC::Equal, is_pressed, one);

                // If we are true then go to the true block
                self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(true_block);
                self.translate_instruction(
                    Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    pc + 2,
                );
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(false_block);
                let mut new_address = pc + 4;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                return true;
            }
            Instruction::IfKeyNeq { comp } => {
                let true_block = self.builder.create_block();
                let false_block = self.builder.create_block();

                let key_id_val = self.get_register(comp);
                let is_pressed = self.is_key_pressed(key_id_val);
                let one = self.builder.ins().iconst(self.byte, 1 as i64);

                // If we are true then go to the true block
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, is_pressed, one);

                // If we are true then go to the true block
                self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(true_block);
                self.translate_instruction(
                    Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    pc + 2,
                    &mut blocks
                );
                self.builder.ins().jump(false_block, &[]);

                self.builder.switch_to_block(false_block);
                let mut new_address = pc + 4;
                loop {
                    let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    if self.translate_instruction(ins, new_address, &mut blocks) {
                        new_address += 2;
                        break;
                    }
                    new_address += 2;
                }

                return true;
            }
            Instruction::IncrementPc { .. } => {
                panic!("Cannot handle changing PC");
            }
            Instruction::GetSpriteAddress { src } => {
                let src_val = self.get_register(src);
                let src_val = self.builder.ins().sextend(self.short, src_val);
                let src_val = self.builder.ins().band_imm(src_val, 0xF);
                let src_val = self.builder.ins().imul_imm(src_val, 5);
                let src_val = self.builder.ins().iadd_imm(src_val, 432);

                self.builder.def_var(*self.address_register, src_val);
            }
            Instruction::Subtract { a, b } => {
                let a_val = self.get_register(a);
                let b_val = self.get_register(b);

                let borrow = self
                    .builder
                    .ins()
                    .icmp(IntCC::UnsignedLessThan, a_val, b_val);

                // Convert the borrow flag into a byte, 0 if there is no overflow and a 1 if there is
                let one = self.builder.ins().iconst(self.byte, 1 as i64);
                let zero = self.builder.ins().iconst(self.byte, 0 as i64);
                let borrow_flag = self.builder.ins().select(borrow, zero, one);

                let r = self.builder.ins().isub(a_val, b_val);
                self.set_register(a, r);
                self.set_register(Register::VF, borrow_flag);
            }
            Instruction::Unknown => {
                println!("Unknown instruction, emitting NOP")
            }
        }

        false
    }
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
