use std::{cell::RefCell, collections::{HashMap, VecDeque}, fmt, rc::Rc};

use good_lp::{solvers, variable, Expression, ProblemVariables, Solution, SolverModel};

use crate::{model::{Edge, IntoModelGraph, Node}, rules3::{Calculus, Feasibility}, tableau2::{LabeledFormula, TableauNode2}, transit::{general_transit, BaseTransit, Constraints, DisplayTransit, Modals, ParallelWorlds, SolveTransit}};

pub(crate) struct TransitB5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) reflexion: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
    pub(crate) rfxsolution: usize,
}

impl BaseTransit for TransitB5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        general_transit(calc, fruit)
    }
}

impl SolveTransit for TransitB5 {
    fn recurse(&mut self, _calc: &mut Calculus) {}

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        let reflexion = if paraws.tab.borrow().is_closed() {
            ParallelWorlds::from_forks(vec![], vec![], Some(leaf), calc)
        } else {
            Self::get_reflexion(constraints.boxsubforms.clone(), &paraws, leaf, calc)
        };
        Self {
            feasibility,
            paraws,
            constraints,
            solution: vec![],
            rfxsolution: 0,
            reflexion,
        }
    }

    fn solve(&mut self) {
        let mut problem = ProblemVariables::new();
        self.paraws.set_choices(true);
        let vars = problem.add_vector(variable().integer().min(0), self.paraws.choices.len());
        self.reflexion.set_choices(true);
        let rvars = problem.add_vector(variable().binary(), self.reflexion.choices.len());
        let mut exprs = HashMap::with_capacity(self.constraints.gradings.len());
        for c in &self.constraints.gradings {
            exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        }
        for (world, var) in self
            .paraws
            .choices
            .iter()
            .zip(vars.iter())
            .chain(self.reflexion.choices.iter().zip(rvars.iter()))
        {
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
        model.add_constraint(rvars.iter().sum::<Expression>().eq(1));
        match model.solve() {
            Ok(solution) => {
                self.solution = vars.into_iter().map(|v| solution.value(v) as u32).collect();
                self.rfxsolution = rvars
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, v)| {
                        if solution.value(v) == 1.0 {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .next()
                    .expect("There must be one variable set to 1");
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
    }
}

impl TransitB5 {
    fn get_reflexion(
        mut boxsubforms: Vec<LabeledFormula>,
        paraws: &ParallelWorlds<Self>,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> ParallelWorlds<Self> {
        leaf.borrow().traverse_anc_formulae(&mut |l| {
            boxsubforms.push(l.clone());
            true
        });
        ParallelWorlds::from_forks(boxsubforms, paraws.forkids.clone(), Some(leaf), calc)
    }
}

impl DisplayTransit for TransitB5 {
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
        TableauNode2::display_root(&self.reflexion.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        for (i, choice) in self.reflexion.choices.iter().enumerate() {
            write!(f, "u{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        if self.is_closed() {
            writeln!(f, "No solution")?
        } else {
            write!(f, "Solution: ")?;
            for (i, val) in self.solution.iter().enumerate() {
                if i == self.rfxsolution {
                    write!(f, "{val}*w{i}+u ")?;
                } else {
                    write!(f, "{val}*w{i} ")?;
                }
            }
            writeln!(f)?;
        }
        writeln!(f)
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
