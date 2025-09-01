use std::{fmt::Write, time::Instant, vec};

use serde::Serialize;

use crate::{
    api::{ServerError, ServerOutput, ServerResult, ServerTimes},
    b5::TransitB5,
    frame::FrameCondition,
    k5::Transit5,
    k45::TransitKOr45,
    kb::TransitB,
    modelinner::{GraphInner, k45_to_graph},
    tableau2::{DisplayTableau, TabChildren, TableauNode2},
    transit::{BaseTransit, DisplayTransit},
    tt::TransitT,
};

#[derive(Serialize)]
pub(crate) struct GraphView {
    nodes: Vec<NodeViewData>,
    edges: Vec<EdgeViewData>,
}

#[derive(Serialize)]
struct NodeViewData {
    data: NodeView,
    position: NodePosition,
}

#[derive(Serialize)]
pub(crate) struct NodeView {
    pub(crate) id: String,
    pub(crate) label: String,
    pub(crate) formulae: String,
}

#[derive(Serialize)]
pub(crate) struct NodePosition {
    pub(crate) x: usize,
    pub(crate) y: usize,
}

#[derive(Serialize)]
struct EdgeViewData {
    data: EdgeView,
}

#[derive(Serialize)]
pub(crate) struct EdgeView {
    pub(crate) source: String,
    pub(crate) target: String,
    pub(crate) sym: String,
}

impl From<GraphInner> for GraphView {
    fn from(value: GraphInner) -> Self {
        let mut nodes = vec![];
        let mut edges = vec![];
        for (i, (node, adjlist)) in value.adjlist.into_iter().enumerate() {
            let mut extra = String::new();
            for f in node.formulae {
                writeln!(&mut extra, "{}", f.formula).unwrap();
            }
            nodes.push(NodeViewData {
                data: NodeView {
                    id: i.to_string(),
                    label: if node.count == 1 {
                        String::new()
                    } else {
                        format!("x{}", node.count)
                    },
                    formulae: extra,
                },
                position: node.position,
            });
            for edge in adjlist {
                edges.push(EdgeViewData {
                    data: EdgeView {
                        source: i.to_string(),
                        target: edge.target.to_string(),
                        sym: if edge.sym {
                            "triangle".to_string()
                        } else {
                            "none".to_string()
                        },
                    },
                });
            }
        }
        Self { nodes, edges }
    }
}

pub(crate) trait IntoModelGraph: BaseTransit + DisplayTransit {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<NodeView>, edges: &mut Vec<EdgeView>);
}

impl DisplayTableau<TransitT> {
    pub(crate) fn model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        framecond: FrameCondition,
        validate: bool,
    ) -> ServerResult {
        self.base_model(formula_str, solve_time, parse_time, framecond, validate)
    }
}

impl DisplayTableau<Transit5> {
    pub(crate) fn model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        framecond: FrameCondition,
        validate: bool,
    ) -> ServerResult {
        self.base_model(formula_str, solve_time, parse_time, framecond, validate)
    }
}

impl DisplayTableau<TransitB5> {
    pub(crate) fn model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        framecond: FrameCondition,
        validate: bool,
    ) -> ServerResult {
        self.base_model(formula_str, solve_time, parse_time, framecond, validate)
    }
}

impl<const R: bool> DisplayTableau<TransitB<R>> {
    pub(crate) fn model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        framecond: FrameCondition,
        validate: bool,
    ) -> ServerResult {
        self.base_model(formula_str, solve_time, parse_time, framecond, validate)
    }
}

impl<T: BaseTransit + DisplayTransit + IntoModelGraph> DisplayTableau<T> {
    pub(crate) fn base_model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        framecond: FrameCondition,
        validate: bool,
    ) -> ServerResult {
        let satisfiable = !self.0.borrow().is_closed();
        let tabw_start = Instant::now();
        let mut tableau = String::new();
        let res = match (validate, satisfiable) {
            (true, true) => writeln!(&mut tableau, "CounterSatisfiable\n"),
            (true, false) => writeln!(&mut tableau, "Theorem\n"),
            (false, true) => writeln!(&mut tableau, "Satisfiable\n"),
            (false, false) => writeln!(&mut tableau, "Unsatisfiable\n"),
        };
        if let Err(e) = write!(&mut tableau, "{}", self) {
            eprintln!("Error writing tableau.");
            eprintln!("{e}");
            return Err(ServerError::ServerErr);
        }
        let tabwrite_time = format!("{:.3?}", tabw_start.elapsed());
        let graph_start = Instant::now();
        let mut nodes = vec![NodeView {
            id: "0".to_string(),
            label: "1".to_string(),
            formulae: String::new(),
        }];
        let mut edges = vec![];
        self.0.borrow().model_graph(0, &mut nodes, &mut edges);
        let graph = if satisfiable {
            Some(GraphView {
                nodes: nodes
                    .into_iter()
                    .enumerate()
                    .map(|(i, n)| NodeViewData {
                        data: n,
                        position: NodePosition {
                            x: 50 * (i % 5),
                            y: 50 * (i / 5),
                        },
                    })
                    .collect(),
                edges: edges
                    .into_iter()
                    .map(|e| EdgeViewData { data: e })
                    .collect(),
            })
        } else {
            None
        };
        let graph_time = format!("{:.3?}", graph_start.elapsed());
        Ok(ServerOutput {
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
            success: satisfiable != validate,
        })
    }
}

impl DisplayTableau<TransitKOr45> {
    pub(crate) fn model(
        self,
        formula_str: String,
        solve_time: String,
        parse_time: String,
        framecond: FrameCondition,
        validate: bool,
    ) -> ServerResult {
        let tabw_start = Instant::now();
        let mut tableau = String::new();
        let satisfiable = !self.0.borrow().is_closed();
        let res = match (validate, satisfiable) {
            (true, true) => writeln!(&mut tableau, "CounterSatisfiable\n"),
            (true, false) => writeln!(&mut tableau, "Theorem\n"),
            (false, true) => writeln!(&mut tableau, "Satisfiable\n"),
            (false, false) => writeln!(&mut tableau, "Unsatisfiable\n"),
        };
        if let Err(e) = res.and(write!(&mut tableau, "{}", self)) {
            eprintln!("Error writing tableau.");
            eprintln!("{e}");
            return Err(ServerError::ServerErr);
        }
        let tabwrite_time = format!("{:.3?}", tabw_start.elapsed());
        let graph_start = Instant::now();
        let graph = if satisfiable {
            let mut graph = k45_to_graph(self);
            graph.set_pos();
            graph.set_frame_conds(framecond);
            Some(graph.into())
        } else {
            None
        };
        let graph_time = format!("{:.3?}", graph_start.elapsed());
        Ok(ServerOutput {
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
            success: satisfiable != validate,
        })
    }
}

impl<T: IntoModelGraph> TableauNode2<T> {
    pub(crate) fn model_graph(
        &self,
        selfi: usize,
        nodes: &mut Vec<NodeView>,
        edges: &mut Vec<EdgeView>,
    ) {
        match &self.children {
            TabChildren::Leaf => {}
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    Self::model_graph(&branch.node.borrow(), selfi, nodes, edges);
                }
            }
            TabChildren::Transition(transit) => {
                transit.model_graph_rec(selfi, nodes, edges);
            }
        }
    }
}
