/*use crate::emu::{Emu, Instruction};
use crate::graph::{GraphManager, Node};
use std::collections::HashMap;
use std::fs::File;
use std::process::exit;

pub fn mk_tree() {
    let mut emu = Emu::new();
    let rom = include_bytes!("../../desktop/rom/15PUZZLE");
    emu.load_rom(&rom.to_vec());

    let mut gm = GraphManager::default();

    let mut visited = Vec::new();

    let mut pc_to_node = HashMap::new();
    pc_to_node.insert(0, 0);

    let mut unvisited = Vec::new();
    unvisited.push((0u16, 0x200u16, "ROOT".to_string()));

    let mut stk = Vec::new();

    while let Some((_requester_pc, pc, postfix)) = unvisited.pop() {
        let i = Emu::read_instruction(pc, &emu.memory);
        if i == None {
            break;
        }
        if visited.contains(&pc) {
            let req_pc = pc_to_node.get(&_requester_pc).unwrap();
            let pc_node = pc_to_node.get(&pc).unwrap();
            gm.link2(*req_pc, *pc_node);
            continue;
        }
        visited.push(pc);
        let i = i.unwrap();

        let n = gm.add_node_2(Node {
            id: gm.next_node_id(),
            i: i.clone(),
            display_name: format!("{} - {:?} ({})", pc, i, postfix),
            name: format!("{} - {:?}", pc, i),
        });
        let requester_node = *pc_to_node.get(&_requester_pc).unwrap();
        gm.link2(requester_node, n.id);
        pc_to_node.insert(pc, n.id);

        match i {
            Instruction::IfNeq { .. }
            | Instruction::IfRegisterNeq { .. }
            | Instruction::IfKeyNeq { .. }
            | Instruction::IfEq { .. }
            | Instruction::IfRegisterEq { .. }
            | Instruction::IfKeyEq { .. } => {
                // The false instruction will visit the true instruction to resume the flow (unless if a jmp)
                if let Some(i) = Emu::read_instruction(pc + 2, &emu.memory) {
                    if !matches!(i, Instruction::Jmp { .. }) {
                        unvisited.push((pc + 2, pc + 4, "TRUE".to_string()));
                    }
                }

                // Visit the false and true instructions
                unvisited.push((pc, pc + 2, "FALSE".to_string()));
                unvisited.push((pc, pc + 4, "TRUE".to_string()));
            }
            Instruction::Jmp { address } => {
                unvisited.push((pc, address, "JMP".to_string()));
            }
            Instruction::CallSub { address } => {
                stk.push(pc);
                unvisited.push((pc, address, "CALLSUB".to_string()));
            }
            Instruction::Return => {
                if let Some(a) = stk.pop() {
                    unvisited.push((pc, a + 2, "RET".to_string()));
                }
            }
            _ => {
                unvisited.push((pc, pc + 2, "".to_string()));
            }
        }
    }

    let mut f = File::create("example-tree.dot").unwrap();
    gm.render(&mut f);
}
*/
