use serde::Serialize;

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
}

pub(crate) fn mock_graph() -> Graph {
    Graph {
        nodes: vec![
            NodeData {
                data: Node {
                    id: "1".to_string(),
                    label: "Node 1".to_string(),
                },
                position: NodePosition { x: 100, y: 100 },
            },
            NodeData {
                data: Node {
                    id: "2".to_string(),
                    label: "Node 3".to_string(),
                },
                position: NodePosition { x: 200, y: 100 },
            },
        ],
        edges: vec![EdgeData {
            data: Edge {
                source: "1".to_string(),
                target: "2".to_string(),
                label: "Edge 12".to_string(),
            },
        }],
    }
}
