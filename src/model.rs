use serde::Serialize;
use std::{fmt::Write, time::Instant};

use crate::{
    api::{ServerOutput, ServerResponse, ServerTimes},
    tableau2::{DisplayTableau, TabChildren, TableauNode2},
    transit::{BaseTransit, DisplayTransit, Transit4, TransitB5, TransitTB},
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
pub(crate) struct Node {
    pub(crate) id: String,
    pub(crate) label: String,
    pub(crate) extra: String,
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
pub(crate) struct Edge {
    pub(crate) source: String,
    pub(crate) target: String,
    pub(crate) label: String,
    pub(crate) extra: String,
}

pub(crate) trait IntoModelGraph: BaseTransit + DisplayTransit {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>);
}

impl<T: IntoModelGraph> DisplayTableau<T> {
    pub(crate) fn model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        symmetric: bool,
    ) -> ServerResponse {
        let tabw_start = Instant::now();
        let mut tableau = String::new();
        if let Err(e) = write!(&mut tableau, "{}", self) {
            eprintln!("Error writing tableau.");
            eprintln!("{e}");
            return ServerResponse::ServerErr;
        }
        let tabwrite_time = format!("{:.3?}", tabw_start.elapsed());
        let graph_start = Instant::now();
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
        let graph_time = format!("{:.3?}", graph_start.elapsed());
        ServerResponse::Ok(ServerOutput {
            formula: formula_str,
            times: ServerTimes {
                server_time: String::new(),
                parse_time,
                solve_time,
                tabwrite_time,
                graph_time,
            },
            graph,
            tableau,
            symmetric,
            success: !self.0.borrow().is_closed(),
        })
    }
}

impl<T: IntoModelGraph> TableauNode2<T> {
    pub(crate) fn model_graph(&self, selfi: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
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

impl IntoModelGraph for TransitTB {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        todo!()
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
