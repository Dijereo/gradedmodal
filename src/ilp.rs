use good_lp::{Expression, ProblemVariables, Solution, SolverModel, solvers, variable};
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufWriter, Write},
    u32,
};

use crate::rules3::{
    Feasibility, GradedKCalc, Modals, ParallelWorlds, Transit, TransitB5, TransitKOr45,
};

fn write_cip(
    ge_constraints: Vec<(u32, Vec<usize>)>,
    le_constraints: Vec<(u32, Vec<usize>)>,
    out_path: &str,
) -> std::io::Result<()> {
    let vars: Vec<_> = {
        let mut vars = HashSet::new();
        vars.extend(
            ge_constraints
                .iter()
                .chain(le_constraints.iter())
                .flat_map(|(_, vs)| vs.iter())
                .cloned(),
        );
        let mut vars = Vec::from_iter(vars.into_iter());
        vars.sort_unstable();
        vars
    };

    let file = File::create(out_path)?;
    let mut w = BufWriter::new(file);

    writeln!(w, "[SCIP Version]")?;
    writeln!(w, "")?;

    // Write variables
    writeln!(w, "variables")?;
    for &v in &vars {
        writeln!(w, "  x{}: binary", v)?;
    }
    writeln!(w)?;

    // Write constraints
    writeln!(w, "constraints")?;
    // for (i, (c, indices)) in constraints.iter().enumerate() {
    //     let terms: Vec<String> = indices.iter().map(|j| format!("x{}", j)).collect();
    //     let expr = terms.join(" + ");
    //     let op = if i < split_index { ">=" } else { "<=" };
    //     writeln!(w, "  cons{}: linear({} {} {})", i, expr, op, c)?;
    // }

    writeln!(w)?;
    writeln!(w, "end")?;

    Ok(())
}

impl Transit {
    pub(crate) fn solve(self) -> (Feasibility, Self) {
        match self {
            Transit::KOr45(transit) => transit.solve(),
            Transit::B5(transit) => transit.solve(),
        }
    }
}

impl TransitKOr45 {
    fn solve(mut self) -> (Feasibility, Transit) {
        self.paraws.set_choices();
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
                (Feasibility::Feasible, Transit::KOr45(self))
            }
            Err(_) => (Feasibility::NoSolution, Transit::KOr45(self)),
        }
    }
}

impl TransitB5 {
    fn solve(mut self) -> (Feasibility, Transit) {
        let mut problem = ProblemVariables::new();
        self.paraws.set_choices();
        let vars = problem.add_vector(variable().integer().min(0), self.paraws.choices.len());
        self.reflexion.set_choices();
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
                (Feasibility::Feasible, Transit::B5(self))
            }
            Err(_) => (Feasibility::NoSolution, Transit::B5(self)),
        }
    }
}
