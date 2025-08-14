use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
    rc::Rc,
};

use good_lp::{Expression, ProblemVariables, Solution, SolverModel, solvers, variable};

use crate::{
    model::{Edge, IntoModelGraph, Node}, rules3::{Calculus, Feasibility}, tableau2::TableauNode2, transit::{
        self, BaseTransit, Constraints, DisplayTransit, Modals, ParallelWorlds, SolveTransit,
    }
};

pub(crate) struct TransitKOr45 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
}

impl BaseTransit for TransitKOr45 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        transit::general_transit(calc, fruit)
    }
}

impl SolveTransit for TransitKOr45 {
    fn recurse(&mut self, calc: &mut Calculus) {
        calc.transition(&self.paraws.tab)
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            paraws,
            constraints,
            solution: vec![],
        }
    }

    fn solve(&mut self) {
        self.paraws.set_choices(true);
        let mut problem = ProblemVariables::new();
        let mut exprs = HashMap::with_capacity(self.constraints.gradings.len());
        for c in &self.constraints.gradings {
            exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        }
        let vars = problem.add_vector(variable().integer().min(0), self.paraws.choices.len());
        for (world, var) in self.paraws.choices.iter().zip(vars.iter()) {
            for (forkid, branchid) in world {
                if *branchid == 1 {
                    exprs
                        .get_mut(forkid)
                        .expect("Forkid should have been entered into hashmap")
                        .2
                        .push(var);
                }
            }
        }
        let mut model = solvers::scip::scip(problem.minimise(vars.iter().sum::<Expression>()));
        for (_, (ge, count, worlds)) in exprs {
            let expr = worlds.into_iter().sum::<Expression>();
            let constr = if ge {
                expr.geq(count as f64)
            } else {
                expr.leq(count)
            };
            model.add_constraint(constr);
        }
        match model.solve() {
            Ok(solution) => {
                self.solution = vars.into_iter().map(|v| solution.value(v) as u32).collect();
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
    }
}

impl DisplayTransit for TransitKOr45 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        if self.feasibility.is_bad() {
            writeln!(f, "No solution")?
        } else {
            write!(f, "Solution: ")?;
            for (i, val) in self.solution.iter().enumerate() {
                write!(f, "{val}*w{i} ")?;
            }
            writeln!(f)?;
        }
        writeln!(f)
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
