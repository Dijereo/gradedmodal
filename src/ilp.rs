use good_lp::{Expression, ProblemVariables, Solution, SolverModel, Variable, solvers, variable};
use std::collections::HashMap;

use crate::{
    rules3::Feasibility,
    tableau2::{TabChildren, TableauNode2},
    transit::{
        BTransit, Constraint, ParaClique, Transit, Transit4, Transit5, TransitB5, TransitKOr45,
        TransitT,
    },
};

impl Transit for TransitKOr45 {
    fn solve(&mut self) {
        self.paraws.set_choices(true);
        let mut problem = ProblemVariables::new();
        let mut exprs = HashMap::with_capacity(self.constraints.len());
        for c in &self.constraints {
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

impl Transit for TransitT {
    fn solve(&mut self) {
        todo!()
    }
}

impl Transit for Transit5 {
    fn solve(&mut self) {
        let mut feasibility = Feasibility::NoSolution;
        for paraclique in &mut self.paracliques {
            paraclique.solve(&self.spotconstraints);
            feasibility = feasibility.better(&paraclique.feasibility);
        }
        self.feasibility = feasibility;
    }
}

impl<T: BTransit> ParaClique<T> {
    fn solve(&mut self, spotconstraints: &Vec<Constraint>) {
        let mut problem = ProblemVariables::new();
        self.spotws.set_choices(true);
        self.cliquews.set_choices(true);
        let spotvars = problem.add_vector(variable().integer().min(0), self.spotws.choices.len());
        let cliqvars = problem.add_vector(variable().integer().min(0), self.cliquews.choices.len());
        let mut exprs =
            HashMap::with_capacity(spotconstraints.len() + self.cliqueconstraints.len());
        for c in spotconstraints.iter().chain(self.cliqueconstraints.iter()) {
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

impl Transit for Transit4 {
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
}

impl Transit4 {
    fn build_rec(
        &mut self,
        problem: &mut ProblemVariables,
        exprs: &mut HashMap<usize, (bool, u32, Vec<Variable>)>,
    ) -> Vec<Variable> {
        self.paraws.set_choices(false);
        exprs.reserve(self.constraints.len());
        for c in &self.constraints {
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
        let mut exprs = HashMap::with_capacity(self.constraints.len());
        for c in &self.constraints {
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

impl Transit for TransitB5 {
    fn solve(&mut self) {
        let mut problem = ProblemVariables::new();
        self.paraws.set_choices(true);
        let vars = problem.add_vector(variable().integer().min(0), self.paraws.choices.len());
        self.reflexion.set_choices(true);
        let rvars = problem.add_vector(variable().binary(), self.reflexion.choices.len());
        let mut exprs = HashMap::with_capacity(self.constraints.len());
        for c in &self.constraints {
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
