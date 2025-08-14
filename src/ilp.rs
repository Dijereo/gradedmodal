use good_lp::{Expression, ProblemVariables, Solution, SolverModel, Variable, solvers, variable};
use std::{cell::RefCell, collections::HashMap, ops::RangeInclusive, rc::Rc};

use crate::{
    rules3::{Calculus, Feasibility},
    tableau2::{LabeledFormula, TabChildren, TableauNode2},
    transit::{
        BaseTransit, Grading, Modals, ParaClique, ParallelWorlds, SolveTransit, TBSolution,
        Transit4, Transit5, TransitB, TransitB5, TransitTB,
    },
};

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

impl SolveTransit for Transit5 {
    fn recurse(&mut self, _calc: &mut Calculus) {}

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Self {
        let submodals = modals.submodals();
        let mut settings = vec![true; submodals.len()];
        let mut paracliques = vec![];
        let (spotranges, spotconstraints) = modals.to_forks_constraints(&mut calc.forks);
        let mut feasibility = Feasibility::Contradiction;
        // OPT: Get initial spotlight para worlds and break if contradiction
        loop {
            let paraclique = ParaClique::new(
                submodals.iter(),
                &settings,
                spotconstraints.boxsubforms.iter().cloned(),
                Vec::from_iter(spotranges.iter().cloned()),
                leaf,
                calc,
            );
            feasibility = feasibility.better(&paraclique.feasibility);
            paracliques.push(paraclique);
            if !Self::next_setting(&mut settings) {
                break;
            }
        }
        Self {
            submodals,
            spotconstraints,
            paracliques,
            feasibility,
        }
    }

    fn solve(&mut self) {
        let mut feasibility = Feasibility::NoSolution;
        for paraclique in &mut self.paracliques {
            paraclique.solve(&self.spotconstraints.gradings);
            feasibility = feasibility.better(&paraclique.feasibility);
        }
        self.feasibility = feasibility;
    }
}

impl Transit5 {
    fn next_setting(settings: &mut Vec<bool>) -> bool {
        for st in settings {
            *st = !*st;
            if !*st {
                return true;
            }
        }
        return false;
    }
}

impl<T: BaseTransit> ParaClique<T> {
    fn solve(&mut self, spotconstraints: &Vec<Grading>) {
        let mut problem = ProblemVariables::new();
        self.spotws.set_choices(true);
        self.cliquews.set_choices(true);
        let spotvars = problem.add_vector(variable().integer().min(0), self.spotws.choices.len());
        let cliqvars = problem.add_vector(variable().integer().min(0), self.cliquews.choices.len());
        let mut exprs =
            HashMap::with_capacity(spotconstraints.len() + self.cliqueconstraints.gradings.len());
        for c in spotconstraints
            .iter()
            .chain(self.cliqueconstraints.gradings.iter())
        {
            exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        }
        for (world, var) in self
            .spotws
            .choices
            .iter()
            .zip(spotvars.iter())
            .chain(self.cliquews.choices.iter().zip(cliqvars.iter()))
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
        let mut model =
            solvers::scip::scip(problem.minimise(
                spotvars.iter().sum::<Expression>() + cliqvars.iter().sum::<Expression>(),
            ));
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
                self.spotsolution = spotvars
                    .into_iter()
                    .map(|v| solution.value(v) as u32)
                    .collect();
                self.cliquesolution = cliqvars
                    .into_iter()
                    .map(|v| solution.value(v) as u32)
                    .collect();
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
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
