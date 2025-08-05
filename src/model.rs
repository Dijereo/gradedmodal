use serde::Serialize;
use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt::{self, Write},
    rc::Rc,
};

use crate::{
    tableau2::{DisplayTableau, TabChildren, TableauNode2},
    transit::{Transit, Transit4, Transit5, TransitB5, TransitKOr45, TransitT},
};

#[derive(Serialize)]
pub(crate) struct Graph {
    nodes: Vec<NodeData>,
    edges: Vec<EdgeData>,
}

#[derive(Serialize)]
struct NodeData {
    data: Node,
    position: NodePosition,
}

#[derive(Serialize)]
struct Node {
    id: String,
    label: String,
    extra: String,
}

#[derive(Serialize)]
struct NodePosition {
    x: usize,
    y: usize,
}

#[derive(Serialize)]
struct EdgeData {
    data: Edge,
}

#[derive(Serialize)]
struct Edge {
    source: String,
    target: String,
    label: String,
    extra: String,
}

impl<T: IntoModelGraph> DisplayTableau<T> {
    pub(crate) fn model_graph(
        self,
    ) -> Result<(Graph, String), fmt::Error> {
        let mut extra = String::new();
        write!(&mut extra, "{}", self)?;
        let mut nodes = vec![Node {
            id: "0".to_string(),
            label: "#0".to_string(),
            extra: String::new(),
        }];
        let mut edges = vec![];
        self.0.borrow().model_graph(0, &mut nodes, &mut edges);
        let graph = Graph {
            nodes: nodes
                .into_iter()
                .enumerate()
                .map(|(i, n)| NodeData {
                    data: n,
                    position: NodePosition {
                        x: 50 * (i % 5),
                        y: 50 * (i / 5),
                    },
                })
                .collect(),
            edges: edges.into_iter().map(|e| EdgeData { data: e }).collect(),
        };
        Ok((graph, extra))
    }
}

trait IntoModelGraph: Transit {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>);
}

impl<T: IntoModelGraph> TableauNode2<T> {
    fn model_graph(&self, selfi: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        match &self.children {
            TabChildren::Leaf => {}
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    Self::model_graph(&branch.node.borrow(), selfi, nodes, edges);
                }
            }
            TabChildren::Transition(transit) => transit.model_graph_rec(selfi, nodes, edges),
        }
    }
}

impl IntoModelGraph for TransitKOr45 {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        let selfi = nodes.len();
        let selfid = selfi.to_string();
        nodes.push(Node {
            id: selfid.clone(),
            label: format!("#{selfi}"),
            extra: String::new(),
        });
        edges.push(Edge {
            source: parenti.to_string(),
            target: selfid,
            label: String::new(),
            extra: String::new(),
        });
        self.paraws.tab.borrow().model_graph(selfi, nodes, edges);
    }
}

impl IntoModelGraph for TransitT {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        if !self.reflexion {
            let selfi = nodes.len();
            let selfid = selfi.to_string();
            nodes.push(Node {
                id: selfid.clone(),
                label: format!("#{selfi}"),
                extra: String::new(),
            });
            edges.push(Edge {
                source: parenti.to_string(),
                target: selfid,
                label: String::new(),
                extra: String::new(),
            });
            self.paraws.tab.borrow().model_graph(selfi, nodes, edges);
        } else {
            self.paraws.tab.borrow().model_graph(parenti, nodes, edges);
        }
    }
}

impl IntoModelGraph for Transit4 {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        let selfi = nodes.len();
        let selfid = selfi.to_string();
        nodes.push(Node {
            id: selfid.clone(),
            label: format!("#{selfi}"),
            extra: String::new(),
        });
        edges.push(Edge {
            source: parenti.to_string(),
            target: selfid,
            label: String::new(),
            extra: String::new(),
        });
        self.paraws.tab.borrow().model_graph(selfi, nodes, edges);
    }
}

impl IntoModelGraph for Transit5 {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        let selfi = nodes.len();
        let selfid = selfi.to_string();
        nodes.push(Node {
            id: selfid.clone(),
            label: format!("#{selfi}"),
            extra: String::new(),
        });
        edges.push(Edge {
            source: parenti.to_string(),
            target: selfid,
            label: String::new(),
            extra: String::new(),
        });
        for pcq in &self.paracliques {
            pcq.spotws.tab.borrow().model_graph(selfi, nodes, edges);
            pcq.cliquews.tab.borrow().model_graph(selfi, nodes, edges);
        }
    }
}

impl IntoModelGraph for TransitB5 {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        let selfi = nodes.len();
        let selfid = selfi.to_string();
        nodes.push(Node {
            id: selfid.clone(),
            label: format!("#{selfi}"),
            extra: String::new(),
        });
        edges.push(Edge {
            source: parenti.to_string(),
            target: selfid,
            label: String::new(),
            extra: String::new(),
        });
        self.paraws.tab.borrow().model_graph(selfi, nodes, edges);
        self.reflexion.tab.borrow().model_graph(selfi, nodes, edges);
    }
}

pub(crate) fn mock_graph(extra: String) -> (Graph, String) {
    (
        Graph {
            nodes: vec![
                NodeData {
                    data: Node {
                        id: "0".to_string(),
                        label: "#0".to_string(),
                        extra: String::new(),
                    },
                    position: NodePosition { x: 100, y: 100 },
                },
                NodeData {
                    data: Node {
                        id: "3".to_string(),
                        label: "#3".to_string(),
                        extra: String::new(),
                    },
                    position: NodePosition { x: 200, y: 100 },
                },
            ],
            edges: vec![EdgeData {
                data: Edge {
                    source: "0".to_string(),
                    target: "3".to_string(),
                    label: "Edge 03".to_string(),
                    extra: String::new(),
                },
            }],
        },
        extra,
    )
}
