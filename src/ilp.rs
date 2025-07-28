use good_lp::{Expression, ProblemVariables, Solution, SolverModel, solvers, variable};
use std::{
    collections::HashSet,
    fs::File,
    io::{BufWriter, Write},
    u32,
};

use crate::rules3::{Feasibility, GradedTransit};

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

pub(crate) fn check_feasibility(transit: &mut GradedTransit) {
    let mut exprs = Vec::from_iter(
        transit
            .diamge
            .iter()
            .map(|(c, _)| (*c, true, vec![]))
            .chain(transit.diamle.iter().map(|(c, _)| (*c, false, vec![]))),
    );
    for (i, world) in transit.para_worlds.iter().enumerate() {
        for (forkid, branchid) in world {
            if *branchid == 1 {
                exprs[*forkid].2.push(i)
            }
        }
    }
    let mut problem = ProblemVariables::new();
    let vars = problem.add_vector(variable().integer().min(0), transit.para_worlds.len());
    let constrs = exprs.into_iter().map(|(count, sense, worlds)| {
        let expr = worlds.into_iter().map(|i| &vars[i]).sum::<Expression>();
        if sense {
            expr.geq(count as f64)
        } else {
            expr.leq(count)
        }
    });
    // let objective = 0;
    let mut model = solvers::scip::scip(problem.minimise(vars.iter().sum::<Expression>()));
    for constr in constrs {
        model.add_constraint(constr);
    }
    match model.solve() {
        Ok(solution) => {
            transit.outcome = Feasibility::Feasible;
            transit.solution = Some(vars.into_iter().map(|v| solution.value(v) as u32).collect());
        }
        Err(_) => {
            transit.outcome = Feasibility::NoSolution;
        }
    }
}
