use crate::graph::{GraphManager, Node};
use crate::emu::{Emu, Instruction};
use std::fs::File;
use std::process::exit;
use std::collections::HashMap;

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
        visited.push(pc);
        let i = i.unwrap();

        let n = gm.add_node_2(Node {
            id: gm.next_node_id(),
            i: i.clone(),
            name: format!("{} - {:?} ({})", pc, i, postfix)
        });
        let requester_node = *pc_to_node.get(&_requester_pc).unwrap();
        gm.link2(requester_node, n.id);
        pc_to_node.insert(pc, n.id);

        match i {
            Instruction::IfNeq { ..} | Instruction::IfRegisterNeq {..} | Instruction::IfKeyNeq {..} | Instruction::IfEq { ..} | Instruction::IfRegisterEq {..} | Instruction::IfKeyEq {..} => {
                // The false instruction will visit the true instruction to resume the flow (unless if a jmp)
                if let Some(i) = Emu::read_instruction(pc + 2, &emu.memory) {
                    if !matches!(i, Instruction::Jmp { ..}) {
                        unvisited.push((pc + 2, pc + 4,  "TRUE".to_string()));
                    }
                }

                // Visit the false and true instructions
                unvisited.push((pc, pc + 2,  "FALSE".to_string()));
                unvisited.push((pc, pc + 4,"TRUE".to_string()));
            }
            Instruction::Jmp { address } => {
                unvisited.push((pc, address, "JMP".to_string()));
            }
            Instruction::CallSub { address } => {
                stk.push(address);
                unvisited.push((pc, address, "CALLSUB".to_string()));
            }
            Instruction::Return => {
                if let Some(a) = stk.pop() {
                    unvisited.push((pc, a, "RET".to_string()));
                }
            }
            _ => {
                if visited.len() < 600 {
                    unvisited.push((pc, pc + 2, "".to_string()))
                }
            }
            /*Instruction::IfRegisterNeq { a, b } => {
                let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
                let false_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
                });
                gm.link(&n, &false_node);
                pc += 2;

                let true_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
                });
                gm.link(&n, &true_node);

                pc += 2;
            }
            Instruction::IfKeyNeq { comp } => {
                let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
                let false_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
                });
                gm.link(&n, &false_node);
                pc += 2;

                let true_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
                });
                gm.link(&n, &true_node);

                pc += 2;
            }
            Instruction::IfEq { a, b } => {
                let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
                let false_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
                });
                gm.link(&n, &false_node);
                pc += 2;

                let true_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
                });
                gm.link(&n, &true_node);

                pc += 2;
            }
            Instruction::IfRegisterEq { a, b } => {
                let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
                let false_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
                });
                gm.link(&n, &false_node);
                pc += 2;

                let true_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
                });
                gm.link(&n, &true_node);

                pc += 2;
            }
            Instruction::IfKeyEq { comp } => {
                let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
                let false_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
                });
                gm.link(&n, &false_node);
                pc += 2;

                let true_node = gm.add_node_2(Node {
                    id: gm.next_node_id(),
                    i: ins_false,
                    name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
                });
                gm.link(&n, &true_node);

                pc += 2;
            }*/
        }
    }


    // let mut pc = 0x200;
    // while let Some(ins) = Emu::read_instruction(pc, &emu.memory) {
    //     if visited_pc.contains(&pc) {
    //         continue;
    //     }
    //
    //     visited_pc.push(pc);
    //
    //     let n = gm.add_node_2(Node {
    //         id: gm.next_node_id(),
    //         i: ins,
    //         name: format!("{} - {:?}", pc, ins)
    //     });
    //     gm.link(&gm.parent(&n), &n);
    //
    //
    //     match ins {
    //         Instruction::IfNeq { a, b } => {
    //             let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
    //             let false_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &false_node);
    //             pc += 2;
    //
    //             let true_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &true_node);
    //
    //             pc += 2;
    //
    //         }
    //         Instruction::IfRegisterNeq { a, b } => {
    //             let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
    //             let false_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &false_node);
    //             pc += 2;
    //
    //             let true_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &true_node);
    //
    //             pc += 2;
    //
    //         }
    //         Instruction::IfKeyNeq { comp} => {
    //             let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
    //             let false_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &false_node);
    //             pc += 2;
    //
    //             let true_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &true_node);
    //
    //             pc += 2;
    //         }
    //         Instruction::IfEq { a, b } => {
    //             let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
    //             let false_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &false_node);
    //             pc += 2;
    //
    //             let true_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &true_node);
    //
    //             pc += 2;
    //         }
    //         Instruction::IfRegisterEq { a, b } => {
    //             let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
    //             let false_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &false_node);
    //             pc += 2;
    //
    //             let true_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &true_node);
    //
    //             pc += 2;
    //         }
    //         Instruction::IfKeyEq { comp} => {
    //             let ins_false = Emu::read_instruction(pc + 2, &emu.memory).unwrap();
    //             let false_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (FALSE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &false_node);
    //             pc += 2;
    //
    //             let true_node = gm.add_node_2(Node {
    //                 id: gm.next_node_id(),
    //                 i: ins_false,
    //                 name: format!("{} - {:?} (TRUE)", pc + 2, ins_false)
    //             });
    //             gm.link(&n, &true_node);
    //
    //             pc += 2;
    //         }
    //         Instruction::Jmp { address } => {
    //             pc = address - 2;
    //         }
    //         _ => {}
    //     }
    //     pc += 2;
    // }

    let mut f = File::create("example-tree.dot").unwrap();
    gm.render(&mut f);
}
