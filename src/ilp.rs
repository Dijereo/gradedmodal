use good_lp::{Expression, ProblemVariables, Solution, SolverModel, solvers, variable};
use std::{
    collections::HashSet,
    fs::File,
    io::{BufWriter, Write},
    u32,
};

use crate::rules3::{Feasibility, GradedKCalc, Modals, ParallelWorlds};

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

pub(crate) fn check_feasibility(
    modals: Modals,
    mut paraws: ParallelWorlds,
    mut reflexion: Option<ParallelWorlds>,
) -> Feasibility {
    let mut problem = ProblemVariables::new();
    paraws.set_choices();
    let vars = problem.add_vector(variable().integer().min(0), paraws.choices.len());
    let mut rvars = vec![];
    let mut exprs = vec![];
    for (c, _) in &modals.ge {
        exprs.push((c, true, vec![], vec![]));
    }
    for (c, _) in &modals.le {
        exprs.push((c, false, vec![], vec![]));
    }
    for (world, var) in paraws.choices.iter().zip(vars.iter()) {
        for (forkid, branchid) in world {
            if *branchid == 1 {
                exprs[*forkid - paraws.minforkid].2.push(var)
            }
        }
    }
    // let objective = 0;
    let mut model = if let Some(rfxn) = &mut reflexion {
        rfxn.set_choices();
        rvars = problem.add_vector(variable().binary(), rfxn.choices.len());
        for (world, var) in rfxn.choices.iter().zip(rvars.iter()) {
            for (forkid, branchid) in world {
                if *branchid == 1 {
                    exprs[*forkid - paraws.minforkid].3.push(var)
                }
            }
        }
        let mut model = solvers::scip::scip(problem.minimise(vars.iter().sum::<Expression>()));
        model.add_constraint(rvars.iter().sum::<Expression>().eq(1));
        model
    } else {
        solvers::scip::scip(problem.minimise(vars.iter().sum::<Expression>()))
    };
    for (count, ge, worlds, rworlds) in exprs {
        let expr = worlds.into_iter().sum::<Expression>() + rworlds.into_iter().sum::<Expression>();
        let constr = if ge {
            expr.geq(*count as f64)
        } else {
            expr.leq(*count)
        };
        model.add_constraint(constr);
    }
    match model.solve() {
        Ok(solution) => {
            let soln = vars.into_iter().map(|v| solution.value(v) as u32).collect();
            let rsoln = rvars
                .into_iter()
                .map(|v| solution.value(v) as u32)
                .collect();
            Feasibility::Feasible(modals, paraws, reflexion, soln, rsoln)
        }
        Err(_) => Feasibility::NoSolution(modals, paraws, reflexion),
    }
}
