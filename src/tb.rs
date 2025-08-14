use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
    rc::Rc,
};

use good_lp::{Expression, ProblemVariables, Solution, SolverModel, Variable, solvers, variable};

use crate::{
    model::{Edge, IntoModelGraph, Node},
    rules3::{Calculus, Feasibility},
    tableau2::TableauNode2,
    transit::{BaseTransit, DisplayTransit, Grading, Modals, ParallelWorlds},
};

pub(crate) struct TransitTB {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: Vec<ParallelWorlds<TransitTB>>,
    pub(crate) constraints: Vec<Vec<Grading>>,
    pub(crate) solutions: Vec<TBSolution>,
}

pub(crate) enum TBSolution {
    None,
    Forward(Vec<u32>),
    Backward(Vec<(Vec<u32>, usize, Option<usize>)>),
}

impl BaseTransit for TransitTB {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<TransitTB>>>, calc: &mut Calculus) -> Option<Self> {
        let mut formulae = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            formulae.push(label.clone());
            true
        });
        let modals = Modals::new(formulae.iter(), false, false);
        if modals.ge.is_empty() {
            return None;
        }
        let (forks, constraintsets) = modals.to_deep_forks_constraints(&mut calc.forks);
        for formula in &mut formulae {
            formula.lemma = true;
        }
        let reflexion = ParallelWorlds::from_forks(formulae, forks[0].clone(), Some(fruit), calc);
        let feasibility = reflexion.tab.borrow().feasibility;
        let (gradings, boxsubforms): (Vec<_>, Vec<_>) = constraintsets
            .into_iter()
            .map(|cns| (cns.gradings, cns.boxsubforms))
            .unzip();
        let mut this = TransitTB {
            feasibility,
            paraws: vec![reflexion],
            constraints: gradings,
            solutions: vec![],
        };
        for (fks, formulae) in forks.into_iter().zip(boxsubforms) {
            let paraws = ParallelWorlds::from_forks(formulae, fks, None, calc);
            if paraws.tab.borrow().is_closed() {
                this.paraws.push(paraws);
                return Some(this);
            }
            this.paraws.push(paraws);
        }
        this.solve();
        Some(this)
    }
}

impl TransitTB {
    pub(crate) fn solve(&mut self) {
        self.solutions = (0..self.constraints.len())
            .map(|_| TBSolution::None)
            .collect();
        for pws in &mut self.paraws {
            pws.set_choices(true);
        }
        for i in (1..self.constraints.len()).rev() {
            if self.solve_forward(i) {
                continue;
            }
            self.solve_reflex(i);
            self.solve_backward(i);
        }
        if !self.solve_forward(0) {
            self.solve_reflex(0);
            self.solve_backward(0);
        }
    }

    pub(crate) fn solve_forward(&mut self, i: usize) -> bool {
        let mut exprs = HashMap::with_capacity(self.constraints[i].len());
        for c in &self.constraints[i] {
            let value = if !c.sense && c.value < 2 {
                return false;
            } else if !c.sense {
                c.value - 2
            } else {
                c.value
            };
            exprs.insert(c.forkid, (c.sense, value, vec![]));
        }
        if let Some(solution) = Self::solve_general(exprs, &self.paraws[i + 1], &[]) {
            self.solutions[i] = TBSolution::Forward(solution);
            true
        } else {
            false
        }
    }

    pub(crate) fn solve_reflex(&mut self, i: usize) {
        for (j, reflx_choice) in self.paraws[i].choices.iter().enumerate() {
            let mut exprs = HashMap::with_capacity(self.constraints[i].len());
            for c in &self.constraints[i] {
                let value = if !c.sense && c.value < 1 {
                    continue;
                } else if !c.sense {
                    c.value - 1
                } else {
                    c.value
                };
                exprs.insert(c.forkid, (c.sense, value, vec![]));
            }
            if let Some(solution) = Self::solve_general(exprs, &self.paraws[i + 1], &[reflx_choice])
            {
                if let TBSolution::Backward(solutions) = &mut self.solutions[i] {
                    solutions.push((solution, j, None));
                } else {
                    self.solutions[i] = TBSolution::Backward(vec![(solution, j, None)]);
                }
            }
        }
    }

    pub(crate) fn solve_backward(&mut self, i: usize) {
        let completed_js = if let TBSolution::Backward(solutions) = &self.solutions[i] {
            solutions.iter().map(|(_, j, _)| *j).collect()
        } else {
            vec![]
        };
        for (j, reflx_choice) in self.paraws[i].choices.iter().enumerate() {
            if completed_js.binary_search(&j).is_ok() {
                continue;
            }
            for (k, back_choice) in self.paraws[i - 1].choices.iter().enumerate() {
                let mut exprs = HashMap::with_capacity(self.constraints[i].len());
                for c in &self.constraints[i] {
                    exprs.insert(c.forkid, (c.sense, c.value, vec![]));
                }
                if let Some(solution) =
                    Self::solve_general(exprs, &self.paraws[i + 1], &[reflx_choice, back_choice])
                {
                    if let TBSolution::Backward(solutions) = &mut self.solutions[i] {
                        solutions.push((solution, j, Some(k)));
                    } else {
                        self.solutions[i] = TBSolution::Backward(vec![(solution, j, Some(k))]);
                    }
                }
            }
        }
    }

    pub(crate) fn solve_general(
        mut exprs: HashMap<usize, (bool, u32, Vec<Variable>)>,
        paraws: &ParallelWorlds<Self>,
        extra_choices: &[&[(usize, usize)]],
    ) -> Option<Vec<u32>> {
        let mut problem = ProblemVariables::new();
        let vars = problem.add_vector(variable().integer().min(0), paraws.choices.len());
        let extra_vars =
            problem.add_vector(variable().integer().min(1).max(1), extra_choices.len());
        for (world, var) in paraws.choices.iter().zip(vars.iter()) {
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
        for (choice, var) in extra_choices.iter().zip(extra_vars.iter()) {
            for (forkid, branchid) in *choice {
                if *branchid == 1 {
                    if let Some(entry) = exprs.get_mut(forkid) {
                        entry.2.push(var.clone());
                    }
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
            Ok(solution) => Some(vars.into_iter().map(|v| solution.value(v) as u32).collect()),
            Err(_) => None,
        }
    }
}

impl DisplayTransit for TransitTB {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        todo!();
    }
}

impl IntoModelGraph for TransitTB {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        todo!()
    }
}
