use crate::emu::Instruction;
use std::collections::HashSet;
use std::io::Write;

type Nd = usize;
type Ed<'a> = &'a (usize, usize);

#[derive(Clone)]
pub struct Node {
    pub(crate) name: String,
    pub(crate) display_name: String,
    pub(crate) id: usize,
    pub(crate) i: Instruction,
}

struct Graph {
    nodes: Vec<Node>,
    edges: Vec<(usize, usize)>,
}
impl<'a> dot::Labeller<'a, Nd, Ed<'a>> for Graph {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("example2").unwrap()
    }
    fn node_id(&'a self, n: &Nd) -> dot::Id<'a> {
        dot::Id::new(format!("N{}", n)).unwrap()
    }
    fn node_label<'b>(&'b self, n: &Nd) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(self.nodes[*n].display_name.clone().into())
    }
    fn edge_label<'b>(&'b self, _: &Ed) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr("&sube;".into())
    }
}

impl<'a> dot::GraphWalk<'a, Nd, Ed<'a>> for Graph {
    fn nodes(&self) -> dot::Nodes<'a, Nd> {
        (0..self.nodes.len()).collect()
    }
    fn edges(&'a self) -> dot::Edges<'a, Ed<'a>> {
        self.edges.iter().collect()
    }
    fn source(&self, e: &Ed) -> Nd {
        e.0
    }
    fn target(&self, e: &Ed) -> Nd {
        e.1
    }
}

pub struct GraphManager {
    nodes: Vec<Node>,
    pub edges: Vec<(usize, usize)>,
}

impl Default for GraphManager {
    fn default() -> Self {
        Self {
            nodes: vec![Node {
                name: "Main".to_string(),
                id: 0,
                i: Instruction::Unknown,
                display_name: "".to_string(),
            }],
            edges: Vec::new(),
        }
    }
}

impl GraphManager {
    pub fn add_node_2(&mut self, node: Node) -> Node {
        if let Some(n) = self.nodes.iter().find(|n| n.name == node.name) {
            return n.clone();
        }

        self.nodes.push(node.clone());
        node.clone()
    }

    pub fn add_node(&mut self, t: String) -> Node {
        if let Some(n) = self.nodes.iter().find(|n| n.name == t) {
            return n.clone();
        }

        let n = Node {
            name: t,
            display_name: "".to_string(),
            id: self.nodes.len(),
            i: Instruction::Unknown,
        };
        self.nodes.push(n.clone());
        n
    }

    pub fn next_node_id(&self) -> usize {
        self.nodes.len()
    }

    pub fn parent(&self, nd: &Node) -> Node {
        let mut i = self.nodes.iter().rev();

        while let Some(n) = i.next() {
            if n.id == nd.id {
                break;
            }
        }

        i.next().unwrap().clone()
    }

    pub fn link(&mut self, nd1: &Node, nd2: &Node) {
        if !self.edges.contains(&(nd1.id, nd2.id)) {
            self.edges.push((nd1.id, nd2.id));
        }
    }

    pub fn link2(&mut self, id1: usize, id2: usize) {
        if !self.edges.contains(&(id1, id2)) {
            self.edges.push((id1, id2));
        }
    }

    pub fn render<W: Write>(&self, output: &mut W) {
        dot::render(
            &Graph {
                nodes: self.nodes.clone(),
                edges: self.edges.clone(),
            },
            output,
        )
        .unwrap()
    }
}
