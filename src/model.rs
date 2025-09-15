use std::{collections::HashMap, fmt::Write, time::Instant, vec};

use serde::Serialize;

use crate::{
    api::{ServerError, ServerOutput, ServerResult, ServerTimes},
    b5::TransitB5,
    frame::FrameCondition,
    k5::Transit5,
    k45::TransitKOr45,
    kb::TransitB,
    tableau2::{DisplayTableau, LabeledFormula, TabChildren, TableauNode2},
    transit::{BaseTransit, DisplayTransit, ModelTransit},
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

pub(crate) struct GraphInner {
    pub(crate) adjlist: Vec<(NodeInner, Vec<EdgeInner>)>,
}

pub(crate) struct NodeInner {
    pub(crate) count: usize,
    pub(crate) formulae: Vec<LabeledFormula>,
    pub(crate) position: NodePosition,
}

pub(crate) struct EdgeInner {
    pub(crate) target: usize,
    pub(crate) sym: bool,
}

impl GraphInner {
    pub(crate) fn set_pos(&mut self) {
        let miny = 0;
        let maxy = 400;
        let n = self.adjlist.len();
        let mut depth = vec![0; n];
        for i in 0..n {
            let d = depth[i];
            for child in &self.adjlist[i].1 {
                depth[child.target] = d + 1;
            }
        }
        let mut levels: HashMap<usize, Vec<usize>> = HashMap::new();
        for (i, &d) in depth.iter().enumerate() {
            levels.entry(d).or_default().push(i);
        }
        for (d, nodes) in levels {
            let count = nodes.len();
            for (j, &idx) in nodes.iter().enumerate() {
                self.adjlist[idx].0.position.x = 100 * (d + 1);
                self.adjlist[idx].0.position.y = miny + (maxy - miny) * (j + 1) / (count + 1);
            }
        }
    }

    pub(crate) fn set_frame_conds(&mut self, framecond: FrameCondition) {
        let n = self.adjlist.len();
        if framecond.symmetric() {
            for (node, edges) in self.adjlist.iter_mut() {
                for edge in edges.iter_mut() {
                    edge.sym = true;
                }
            }
        }
        if framecond.ray() {
            for (i, (node, edges)) in self.adjlist.iter_mut().enumerate() {
                if edges.is_empty() {
                    edges.push(EdgeInner {
                        target: i,
                        sym: false,
                    });
                }
            }
        } else if framecond.reflexive() {
            for (i, (node, edges)) in self.adjlist.iter_mut().enumerate() {
                edges.push(EdgeInner {
                    target: i,
                    sym: false,
                });
            }
        }
        if framecond.euclidean() {
            for i in 1..n {
                if i < n - 1 {
                    self.adjlist[i].1.push(EdgeInner {
                        target: i + 1,
                        sym: true,
                    });
                }
                self.adjlist[i].1.push(EdgeInner {
                    target: i,
                    sym: false,
                });
            }
        }
    }
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

impl<T: ModelTransit + BaseTransit + DisplayTransit> DisplayTableau<T> {
    pub(crate) fn serve(
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
            let mut graph = T::to_graph_inner(self);
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
