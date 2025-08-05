use serde::Serialize;
use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt::{self, Write},
    rc::Rc,
};

use crate::{
    tableau2::{DisplayTableau, TableauNode2},
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

pub(crate) trait IntoModelGraph: Transit {
    fn model_graph(tab: DisplayTableau<Self>) -> Result<(Graph, String), fmt::Error> {
        let mut extra = String::new();
        write!(&mut extra, "{}", tab)?;
        Ok(mock_graph(extra))
    }
}

impl<T: Transit> IntoModelGraph for T {}

pub(crate) fn mock_graph(extra: String) -> (Graph, String) {
    (
        Graph {
            nodes: vec![
                NodeData {
                    data: Node {
                        id: "1".to_string(),
                        label: "Node 1".to_string(),
                        extra: String::new(),
                    },
                    position: NodePosition { x: 100, y: 100 },
                },
                NodeData {
                    data: Node {
                        id: "2".to_string(),
                        label: "Node 3".to_string(),
                        extra: String::new(),
                    },
                    position: NodePosition { x: 200, y: 100 },
                },
            ],
            edges: vec![EdgeData {
                data: Edge {
                    source: "1".to_string(),
                    target: "2".to_string(),
                    label: "Edge 12".to_string(),
                    extra: String::new(),
                },
            }],
        },
        extra,
    )
}
