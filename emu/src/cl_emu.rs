use crate::emu;
use crate::emu::{Emu, Instruction, Register};
use core::mem;
use cranelift::codegen::binemit::NullTrapSink;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;

#[derive(Default)]
pub(crate) struct JITManager {
    can_be_jitted: HashMap<u16, bool>,
    jitted_functions: HashMap<u16, (*const u8, u16)>,
    jit: JIT,
}

impl JITManager {
    /// Compile a chip8 function at a given address to a native function using cranelift, returns (ptr_to_function, new_pc)
    /// new_pc is the point where the emulator should resume normal interpreted execution (the new value for the program counter)
    pub(crate) fn compile_function(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) {
        let mut instructions = Vec::new();

        let mut new_address = address;
        loop {
            let ins = Emu::read_instruction(new_address, memory);
            new_address += 2;
            if ins == Instruction::Return {
                new_address -= 2;
                break;
            }
            instructions.push(ins);
        }

        let func = self.jit.compile(instructions);
        self.jitted_functions.insert(address, (func, new_address));
    }

    pub(crate) fn call_function(&mut self, address: u16, memory: &[u8; emu::MEMORY_SIZE]) -> u16 {
        if !self.jitted_functions.contains_key(&address) {
            self.compile_function(address, memory);
        }

        let (func, new_pc) = self.jitted_functions.get(&address).unwrap();
        let code_fn = unsafe { mem::transmute::<_, fn() -> u32>(*func) };
        let _ = code_fn();
        *new_pc
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
                if matches!(ins, Instruction::Jmp { .. } | Instruction::CallSub { .. } | Instruction::SetDelayTimer { .. } | Instruction::SetSoundTimer { .. } | Instruction::GetDelay { .. } | Instruction::DrawSprite { .. } | Instruction::IfKeyEq { .. } | Instruction::IfKeyNeq { .. } | Instruction::Unknown | Instruction::SetI { .. } | Instruction::ClearDisplay | Instruction::GetKey { .. } | Instruction::CallRCA { .. })
                {
                    res = false;
                    break;
                }

                // Can probably be done in future, needs support for modifying memory/non Vx registers
                if matches!(ins, Instruction::BCD { .. } | Instruction::RegisterLoad { ..} | Instruction::RegisterDump { .. } | Instruction::IncrementIByRegister { .. } | Instruction::IncrementPc { .. })
                {
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
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
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
    fn compile(&mut self, instructions: Vec<Instruction>) -> *const u8 {
        let name = "main";

        self.translate(instructions);

        let id = self
            .module
            .declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        self.module
            .define_function(id, &mut self.ctx, &mut NullTrapSink {})
            .unwrap();

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();

        let code = self.module.get_finalized_function(id);

        code
    }

    /// Translate a list of instructions into cranelift ir for execution,
    /// TODO: currently doesn't use the current state of registers so results won't be correct
    ///  However it also doesn't extract the final state either so in effect this is the same as skipping the function execution when invoked from the emulator
    fn translate(&mut self, instructions: Vec<Instruction>) {
        //TODO: this should be 16 bit
        let byte = Type::int(8).unwrap();

        self.ctx.func.signature.returns.push(AbiParam::new(byte));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
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

        let reg_v1 = {
            let var = Variable::new(1);
            register_map.insert(Register::V1, var);
            builder.declare_var(var, byte);
            var
        };

        for ins in instructions {
            match ins {
                Instruction::IncrementRegisterByImmediate { by, dest } => {
                    let var = *register_map.get(&dest).unwrap();
                    let by_const = builder.ins().iconst(byte, by as i64);
                    let dest_value = builder.use_var(var);
                    let new_value = builder.ins().iadd(dest_value, by_const);
                    builder.def_var(var, new_value);
                }
                _ => unimplemented!("Can't jit {:?}", ins),
            }
        }

        let ret = builder.use_var(*register_map.get(&Register::V1).unwrap());
        builder.ins().return_(&[ret]);

        builder.finalize();
    }
}

#[test]
pub fn test_jit_increment_register_by_imm() {
    let mut j = JIT::default();
    let code = j.compile(vec![Instruction::IncrementRegisterByImmediate {
        dest: Register::V1,
        by: 1,
    }]);
    let code_fn = unsafe { mem::transmute::<_, fn() -> u32>(code) };
    let res = code_fn();
    assert_eq!(1, res);
}

#[test]
pub fn test_jit_increment_register_by_imm2() {
    let mut j = JIT::default();
    let code = j.compile(vec![Instruction::IncrementRegisterByImmediate {
        dest: Register::V1,
        by: 0,
    }]);
    let code_fn = unsafe { mem::transmute::<_, fn() -> u32>(code) };
    let res = code_fn();
    assert_eq!(0, res);
}

#[test]
pub fn can_jit_big_loop() {
    let mut e = Emu::new();
    e.load_rom(include_bytes!("../benches/test_big_loop.ch8"));
    assert_eq!(true, e.jit.can_function_be_jitted(0x202, &e.memory));
}

#[test]
pub fn can_jit_addr_loop() {
    let mut e = Emu::new();
    e.load_rom(include_bytes!("../benches/test_big_addr_loop.ch8"));
    assert_eq!(true, e.jit.can_function_be_jitted(0x202, &e.memory));
}
