use std::collections::HashMap;

use crate::{
    formula::Formula,
    frame::FrameCondition,
    k45::TransitKOr45,
    model::NodePosition,
    tableau2::{DisplayTableau, LabeledFormula, TabChildren, TableauNode2},
    transit::{BaseTransit, DisplayTransit, ParallelWorlds},
};

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

pub(crate) fn k45_to_graph(tab: DisplayTableau<TransitKOr45>) -> GraphInner {
    let mut fruits = vec![];
    TableauNode2::get_fruits(&tab.0, &mut fruits);
    let mut graph = GraphInner { adjlist: vec![] };
    for fruit in fruits {
        if fruit.borrow().is_closed() {
            continue;
        }
        let mut formulae = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |f| {
            match f.formula.as_ref() {
                Formula::PropVar(_, _)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(_, _)
                | Formula::DiamondLe(_, _) => formulae.push(f.clone()),
                Formula::Not(formula) => match formula.as_ref() {
                    Formula::PropVar(_, _) => formulae.push(f.clone()),
                    _ => {}
                },
                _ => {}
            };
            true
        });
        let node = NodeInner {
            count: 1,
            formulae,
            position: NodePosition { x: 0, y: 0 },
        };
        graph.adjlist.push((node, vec![]));
        match &fruit.borrow().children {
            TabChildren::Leaf | TabChildren::Fork { .. } => {}
            TabChildren::Transition(transit) => k45_to_graph_rec(&transit, 0, &mut graph),
        }
        break;
    }
    graph
}

pub(crate) fn k45_to_graph_rec(transit: &TransitKOr45, parent_id: usize, graph: &mut GraphInner) {
    let mut fruits = vec![];
    TableauNode2::get_fruits(&transit.paraws.tab, &mut fruits);
    for (c, choices) in transit.solution.iter().zip(transit.paraws.choices.iter()) {
        if *c == 0 {
            continue;
        }
        for fruit in fruits.iter() {
            let mut fruitchoices = vec![];
            fruit
                .borrow()
                .get_choices(&mut fruitchoices, &transit.paraws.forkids);
            if &fruitchoices != choices {
                continue;
            }
            let target = graph.adjlist.len();
            graph.adjlist[parent_id]
                .1
                .push(EdgeInner { target, sym: false });
            let mut formulae = vec![];
            fruit.borrow().traverse_anc_formulae(&mut |f| {
                match f.formula.as_ref() {
                    Formula::PropVar(_, _)
                    | Formula::Box(_)
                    | Formula::Diamond(_)
                    | Formula::DiamondGe(_, _)
                    | Formula::DiamondLe(_, _) => formulae.push(f.clone()),
                    Formula::Not(formula) => match formula.as_ref() {
                        Formula::PropVar(_, _) => formulae.push(f.clone()),
                        _ => {}
                    },
                    _ => {}
                };
                true
            });
            let node = NodeInner {
                count: *c as usize,
                formulae,
                position: NodePosition { x: 0, y: 0 },
            };
            let currid = graph.adjlist.len();
            graph.adjlist.push((node, vec![]));
            match &fruit.borrow().children {
                TabChildren::Leaf | TabChildren::Fork { .. } => {}
                TabChildren::Transition(nexttransit) => {
                    k45_to_graph_rec(nexttransit, currid, graph)
                }
            }
            break;
        }
    }
}
