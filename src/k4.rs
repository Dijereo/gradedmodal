use std::{cell::RefCell, collections::{HashMap, VecDeque}, fmt, ops::RangeInclusive, rc::Rc};

use good_lp::{solvers, variable, Expression, ProblemVariables, Solution, SolverModel, Variable};

use crate::{model::{Edge, IntoModelGraph, Node}, rules3::{Calculus, Feasibility}, tableau2::{TabChildren, TableauNode2}, transit::{general_transit, BaseTransit, Constraints, DisplayTransit, Modals, ParallelWorlds, SolveTransit}};

pub(crate) struct Transit4 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) ranges: Vec<RangeInclusive<usize>>,
    pub(crate) vars: Vec<Variable>,
    pub(crate) solution: Vec<u32>,
}

impl BaseTransit for Transit4 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        general_transit(calc, fruit)
    }
}

impl Transit4 {
    fn from_diffraction(
        modals: Modals,
        mut ranges: Vec<RangeInclusive<usize>>,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Self {
        let (forkids, constraints) = modals.to_forks_constraints(&mut calc.forks);
        ranges.extend(forkids.into_iter());
        let paraws = ParallelWorlds::from_forks(
            constraints.boxsubforms.clone(),
            ranges.clone(),
            Some(fruit),
            calc,
        );
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            paraws,
            constraints,
            vars: vec![],
            solution: vec![],
            ranges,
        }
    }

    pub(crate) fn diffract(
        &self,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Option<Transit4> {
        let mut labels = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            labels.push(label.clone());
            true
        });
        labels.extend(self.constraints.boxsubforms.iter().cloned());
        let modals = Modals::new(labels.iter(), calc.framecond.ray(), false);
        // sleep(Duration::from_secs(3));
        if modals.ge.is_empty() {
            return None;
        }
        let mut subtransit = Self::from_diffraction(modals, self.ranges.clone(), fruit, calc);
        if subtransit.feasibility.is_bad() {
            return Some(subtransit);
        }
        subtransit.recurse(calc);
        if subtransit.feasibility.is_bad() {
            return Some(subtransit);
        }
        subtransit.check();
        Some(subtransit)
    }
}

impl SolveTransit for Transit4 {
    fn solve(&mut self) {
        let mut problem = ProblemVariables::new();
        let mut exprs = HashMap::new();
        let allvars = self.build_rec(&mut problem, &mut exprs);
        let mut model = solvers::scip::scip(problem.minimise(allvars.iter().sum::<Expression>()));
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
                self.solution = self
                    .vars
                    .iter()
                    .map(|v| solution.value(v.clone()) as u32)
                    .collect();
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
    }

    fn recurse(&mut self, calc: &mut Calculus) {
        if self.feasibility.is_bad() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        for flower in flowers {
            let subtransit = self.diffract(&flower, calc);
            if let Some(subtransit) = subtransit {
                flower.borrow_mut().feasibility = subtransit.feasibility;
                flower.borrow_mut().children = TabChildren::Transition(subtransit);
            }
        }
        self.feasibility = TableauNode2::set_feasibility_rec(&self.paraws.tab);
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
            ranges: paraws.forkids.clone(),
            paraws,
            constraints,
            vars: vec![],
            solution: vec![],
        }
    }
}

impl Transit4 {
    fn build_rec(
        &mut self,
        problem: &mut ProblemVariables,
        exprs: &mut HashMap<usize, (bool, u32, Vec<Variable>)>,
    ) -> Vec<Variable> {
        self.paraws.set_choices(false);
        exprs.reserve(self.constraints.gradings.len());
        for c in &self.constraints.gradings {
            exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        }
        self.vars = problem.add_vector(variable().integer().min(0), self.paraws.choices.len());
        let mut allvars = self.vars.clone();
        let mut fruits = vec![];
        TableauNode2::get_fruits(&self.paraws.tab, &mut fruits);
        for fruit in fruits {
            match &mut fruit.borrow_mut().children {
                TabChildren::Transition(child) => allvars.extend(child.build_rec(problem, exprs)),
                _ => {}
            }
        }
        for (world, var) in self.paraws.choices.iter().zip(self.vars.iter()) {
            for (forkid, branchid) in world {
                if *branchid == 1 {
                    exprs
                        .get_mut(forkid)
                        .expect("Forkid should have been entered into hashmap")
                        .2
                        .push(var.clone());
                }
            }
        }
        allvars
    }

    pub(crate) fn check(&mut self) {
        self.paraws.set_choices(false);
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
            Ok(_) => self.feasibility = Feasibility::Feasible,
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
    }
}

impl DisplayTransit for Transit4 {
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
