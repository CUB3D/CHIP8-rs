use crate::emu::InstructionReference;
use crate::emu::{Instruction, Register};

use core::mem;
use cranelift::codegen::binemit::{CodeOffset, StackMap, StackMapSink, TrapSink};
use cranelift::codegen::ir::{FuncRef, Inst, SourceLoc, ValueLabel};
use cranelift::prelude::*;

use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use std::collections::HashMap;

pub struct EmuTrap;
impl TrapSink for EmuTrap {
    fn trap(&mut self, _offset: CodeOffset, _srcloc: SourceLoc, _code: TrapCode) {
        // println!("Got a trap {} {} {}", _offset, _srcloc, _code);
    }
}
impl StackMapSink for EmuTrap {
    fn add_stack_map(&mut self, _: u32, _: StackMap) {}
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

#[derive(Debug)]
#[repr(packed)]
pub struct RegisterSet([u8; 16]);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct NativeFunction {
    func: *const u8,
}

impl NativeFunction {
    pub fn call(&self, mut registers: [u8; 16]) -> [u8; 16] {
        //println!("Calling jitted function");
        //println!("Input args={:?}", registers);
        let code_fn = unsafe { mem::transmute::<_, fn(RegisterSet) -> RegisterSet>(self.func) };
        let ret = code_fn(RegisterSet(registers));
        // println!("Returned {:?}", ret);
        registers[1] = ret.0[0];

        registers
    }
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
    pub fn compile_native(&mut self, ops: Vec<InstructionReference>) -> NativeFunction {
        let name = format!(
            "jit_from_{}_to_{}_{}",
            ops.first().unwrap().pos,
            ops.last().unwrap().pos,
            rand::random::<u8>()
        );
        NativeFunction {
            func: self.compile(ops, &name),
        }
    }

    pub fn compile(&mut self, ops: Vec<InstructionReference>, name: &str) -> *const u8 {
        self.translate(ops);

        /*self.ctx.func.collect_debug_info();
        println!("{}", self.ctx.func.display(Some(self.module.isa())));*/

        let id = self
            .module
            .declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        let mut trap = EmuTrap {};
        let mut stack_trap = EmuTrap {};

        self.module
            .define_function(id, &mut self.ctx, &mut trap, &mut stack_trap)
            .unwrap();

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();

        let code = self.module.get_finalized_function(id);

        code
    }

    /// Translate a list of instructions into cranelift ir for execution,
    fn translate(&mut self, ops: Vec<InstructionReference>) {
        let byte = Type::int(8).unwrap();
        let short = Type::int(16).unwrap();

        // The jit function will take the full state of the vm as args = 16 registers
        for _ in 1..=16 {
            self.ctx.func.signature.params.push(AbiParam::new(byte));
        }
        assert_eq!(self.ctx.func.signature.params.len(), 16);

        // The jit function will return the full state of the vm = 16 registers
        //TODO: pretty sure cranelift has a bug here when you have > 4 return values for a function :/, function runs fine but segfaults on return
        for _ in 1..=1 {
            self.ctx.func.signature.returns.push(AbiParam::new(byte));
        }
        //assert_eq!(self.ctx.func.signature.returns.len(), 16);

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

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

        for (index, r) in registers.iter().enumerate() {
            let var = Variable::new(index);
            register_map.insert(r.clone(), var);
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

        let mut trans = InstructionTranslator {
            byte,
            short,
            builder,
            registers: &register_map,
            address_register: &address_register,
            delay_timer,
            sound_timer,
            module: &mut self.module,
            dump_state_func,
            blocks: Vec::new(),
        };

        // Create the entry block, to start emitting code in.
        let entry_block = trans.builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        //
        // TODO: Streamline the API here.
        trans
            .builder
            .append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        trans.builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        trans.builder.seal_block(entry_block);

        // Create the actual first block, to store the first jitted instruction onward
        let first_blk = trans.new_block_for_pc(ops.first().expect("No instructions?").pos);

        // function prelude
        trans.init_registers(entry_block);

        trans.builder.ins().jump(first_blk, &[]);
        trans.builder.switch_to_block(first_blk);

        // Actual first jitted instructions
        let mut ops = ops.clone().into_iter().rev().collect();
        trans.translate_instruction(&mut ops);

        trans.builder.seal_all_blocks();

        // let return_vals = registers.iter().copied().map(|i| trans.get_register(i)).collect::<Vec<_>>();

        let return_vals = &[trans.get_register(Register::V1)];
        trans.builder.ins().return_(return_vals);

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
    delay_timer: Variable,
    sound_timer: Variable,
    dump_state_func: FuncRef,
    blocks: Vec<(u16, Block)>,
}

impl<'a> InstructionTranslator<'a> {
    pub fn init_registers(&mut self, entry_block: Block) {
        // Get the args to the function, set each register to the correct val
        let args = self
            .builder
            .block_params(entry_block)
            .iter()
            .copied()
            .collect::<Vec<_>>();
        for i in 0..16 {
            self.set_register(Register::from(i), args[i as usize]);
        }
    }

    pub fn set_register(&mut self, reg: Register, value: Value) {
        let var = *self.registers.get(&reg).unwrap();
        self.builder.def_var(var, value);
    }

    pub fn get_register(&mut self, reg: Register) -> Value {
        let var = *self.registers.get(&reg).unwrap();
        self.builder.use_var(var)
    }

    pub fn new_block_for_pc(&mut self, pc: u16) -> Block {
        let blk = self.builder.create_block();
        self.blocks.push((pc, blk));
        blk
    }

    fn memset(&self, _loc: Value, _val: Value) {
        unimplemented!()
    }
    fn memread(&self, _loc: Value) -> Value {
        unimplemented!()
    }

    fn play_sound(&self) {
        unimplemented!()
    }
    fn sleep(&self) {
        unimplemented!()
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

    pub fn translate_instruction(&mut self, instructions: &mut Vec<InstructionReference>) {
        while let Some(ins) = instructions.pop() {
            self.builder.set_srcloc(SourceLoc::new(ins.pos as u32));

            //self.dump_state(ins.pos);

            //self.dec_timers();

            match ins.op {
                Instruction::SetRegister { register, value } => {
                    let value_const = self.builder.ins().iconst(self.byte, value as i64);
                    self.set_register(register, value_const);
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

                    let borrow =
                        self.builder
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
                Instruction::CallSub { address: _ } => {
                    unimplemented!()
                }
                Instruction::Jmp { address } => {
                    let (_, blk) = self
                        .blocks
                        .iter()
                        .find(|(bpc, _blk)| *bpc == address)
                        .expect("No target block for jmp");
                    self.builder.ins().jump(*blk, &[]);
                    let new_blk = self.new_block_for_pc(ins.pos);
                    self.builder.switch_to_block(new_blk);
                }
                Instruction::Return => {
                    unimplemented!()
                }
                Instruction::CallRCA { address: _ } => {
                    unimplemented!()
                }
                Instruction::Rand {
                    dest: _,
                    modulus: _,
                } => {
                    // let mod_arg = self.builder.ins().iconst(self.byte, modulus as i64);
                    //
                    // let ca = self.builder.ins().call(self.rand_func, &[mod_arg]);
                    //
                    // let res = self.builder.inst_results(ca);
                    // assert_eq!(res.len(), 1);
                    //
                    // self.set_register(dest, res[0]);
                    unimplemented!()
                }
                Instruction::DrawSprite {
                    x: _,
                    y: _,
                    height: _,
                } => {
                    unimplemented!()
                }
                Instruction::IfNeq { b: _, a: _ } => {
                    // let true_block = self.builder.create_block();
                    // let false_block = self.builder.create_block();
                    //
                    // let a_val = self.get_register(a);
                    // let b_val = self.builder.ins().iconst(self.byte, b as i64);
                    //
                    // // If we are true then go to the true block
                    // let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, b_val);
                    //
                    // // If we are true then go to the true block
                    // self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                    // self.builder.ins().jump(false_block, &[]);
                    //
                    // self.builder.switch_to_block(true_block);
                    // self.translate_instruction(
                    //     Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    //     pc + 2,
                    //     blocks,
                    //     callsub_placeholder,
                    // );
                    // self.builder.ins().jump(false_block, &[]);
                    //
                    // self.builder.switch_to_block(false_block);
                    // let mut new_address = pc + 4;
                    // loop {
                    //     let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    //     if self.translate_instruction(ins, new_address, blocks, callsub_placeholder) {
                    //         new_address += 2;
                    //         break;
                    //     }
                    //     new_address += 2;
                    // }

                    // return true;
                    unimplemented!()
                }
                Instruction::IfEq { a, b } => {
                    let true_block = self.builder.create_block();
                    let false_block = self.builder.create_block();

                    let a_val = self.get_register(a);
                    let b_val = self.builder.ins().iconst(self.byte, b as i64);

                    // If we are true then go to the true block
                    let cmp = self.builder.ins().icmp(IntCC::Equal, a_val, b_val);

                    // If we are true then go to the true block
                    self.builder.ins().brz(cmp, true_block, &[]);
                    // Otherwise skip it and go to the false block
                    self.builder.ins().jump(false_block, &[]);

                    self.builder.switch_to_block(true_block);

                    let true_instruction = instructions.pop().expect("No target for ifEq true");
                    self.translate_instruction(&mut vec![true_instruction]);

                    self.builder.ins().jump(false_block, &[]);

                    self.builder.switch_to_block(false_block);
                }
                Instruction::IfRegisterEq { a: _, b: _ } => {
                    // let true_block = self.builder.create_block();
                    // let false_block = self.builder.create_block();
                    //
                    // let a_val = self.get_register(a);
                    // let b_val = self.get_register(b);
                    //
                    // // If we are true then go to the true block
                    // let cmp = self.builder.ins().icmp(IntCC::Equal, a_val, b_val);
                    //
                    // // If we are true then go to the true block
                    // self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                    // self.builder.ins().jump(false_block, &[]);
                    //
                    // self.builder.switch_to_block(true_block);
                    // self.translate_instruction(
                    //     Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    //     pc + 2,
                    //     blocks,
                    //     callsub_placeholder,
                    // );
                    // self.builder.ins().jump(false_block, &[]);
                    //
                    // self.builder.switch_to_block(false_block);
                    // let mut new_address = pc + 4;
                    // loop {
                    //     let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    //     if self.translate_instruction(ins, new_address, blocks, callsub_placeholder) {
                    //         new_address += 2;
                    //         break;
                    //     }
                    //     new_address += 2;
                    // }
                    //
                    // return true;
                    unimplemented!()
                }
                Instruction::IfRegisterNeq { a: _, b: _ } => {
                    // let true_block = self.builder.create_block();
                    // let false_block = self.builder.create_block();
                    //
                    // let a_val = self.get_register(a);
                    // let b_val = self.get_register(b);
                    //
                    // // If we are true then go to the true block
                    // let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, b_val);
                    //
                    // // If we are true then go to the true block
                    // self.builder.ins().brz(cmp, true_block, &[]); // Otherwise skip it and go to the true block
                    // self.builder.ins().jump(false_block, &[]);
                    //
                    // self.builder.switch_to_block(true_block);
                    // self.translate_instruction(
                    //     Emu::read_instruction(pc + 2, &self.memory).unwrap(),
                    //     pc + 2,
                    //     blocks,
                    //     callsub_placeholder,
                    // );
                    // self.builder.ins().jump(false_block, &[]);
                    //
                    // self.builder.switch_to_block(false_block);
                    // let mut new_address = pc + 4;
                    // loop {
                    //     let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                    //     if self.translate_instruction(ins, new_address, blocks, callsub_placeholder) {
                    //         new_address += 2;
                    //         break;
                    //     }
                    //     new_address += 2;
                    // }
                    //
                    // return true;
                    unimplemented!()
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
                    // self.clear_display();
                    unimplemented!()
                }
                Instruction::IncrementIByRegister { by } => {
                    let address_current = self.builder.use_var(*self.address_register);
                    let offset = self.get_register(by);
                    let offset_sign_extend = self.builder.ins().sextend(self.short, offset);
                    let new_address = self.builder.ins().iadd(address_current, offset_sign_extend);
                    self.builder.def_var(*self.address_register, new_address);
                }
                Instruction::GetKey { dest: _ } => {
                    // let key_code = self.wait_for_key();
                    // self.set_register(dest, key_code);
                    unimplemented!()
                }

                Instruction::IfKeyEq { comp: _ } => {
                    /*let true_block = self.builder.create_block();
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
                        blocks,
                        callsub_placeholder,
                    );
                    self.builder.ins().jump(false_block, &[]);

                    self.builder.switch_to_block(false_block);
                    let mut new_address = pc + 4;
                    loop {
                        let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                        if self.translate_instruction(ins, new_address, blocks, callsub_placeholder) {
                            new_address += 2;
                            break;
                        }
                        new_address += 2;
                    }

                    return true;*/
                    unimplemented!()
                }
                Instruction::IfKeyNeq { comp: _ } => {
                    /*let true_block = self.builder.create_block();
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
                        blocks,
                        callsub_placeholder,
                    );
                    self.builder.ins().jump(false_block, &[]);

                    self.builder.switch_to_block(false_block);
                    let mut new_address = pc + 4;
                    loop {
                        let ins = Emu::read_instruction(new_address, self.memory).unwrap();
                        if self.translate_instruction(ins, new_address, blocks, callsub_placeholder) {
                            new_address += 2;
                            break;
                        }
                        new_address += 2;
                    }

                    return true;*/
                    unimplemented!()
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
        }
    }
}
