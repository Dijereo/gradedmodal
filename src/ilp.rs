use std::{
    collections::HashSet,
    fs::File,
    io::{BufWriter, Write},
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
