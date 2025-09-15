use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
    rc::Rc,
};

use good_lp::{Expression, ProblemVariables, Solution, SolverModel, solvers, variable};

use crate::{
    formula::Formula,
    model::{EdgeInner, GraphInner, NodeInner, NodePosition},
    rules3::{Calculus, Feasibility},
    tableau2::{DisplayTableau, TabChildren, TableauNode2},
    timeout::{MayTimeout, TimeoutHandler},
    transit::{
        self, BaseTransit, Constraints, DisplayTransit, Modals, ModelTransit, ParallelWorlds,
        SolveTransit,
    },
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

    fn transit(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>> {
        transit::general_transit(calc, fruit, toh)
    }
}

impl SolveTransit for TransitKOr45 {
    fn recurse(&mut self, calc: &mut Calculus, toh: &impl TimeoutHandler) -> MayTimeout<()> {
        calc.transition(
            &self.paraws.tab,
            false,
            // Some((&self.paraws.forkids, &self.constraints)),
            None,
            toh,
        )
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self> {
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc, toh)?;
        let feasibility = paraws.tab.borrow().feasibility;
        Ok(Self {
            feasibility,
            paraws,
            constraints,
            solution: vec![],
        })
    }

    fn solve(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<()> {
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
        toh.timedout()?;
        match model.solve() {
            Ok(solution) => {
                self.solution = vars.into_iter().map(|v| solution.value(v) as u32).collect();
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
        Ok(())
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

impl ModelTransit for TransitKOr45 {
    fn to_graph_inner(this: DisplayTableau<Self>) -> GraphInner {
        let mut fruits = vec![];
        TableauNode2::get_fruits(&this.0, &mut fruits);
        let mut graph = GraphInner { adjlist: vec![] };
        for fruit in fruits {
            if fruit.borrow().is_closed() {
                continue;
            }
            let mut formulae = vec![];
            fruit.borrow().traverse_anc_formulae(&mut |f| {
                match f.formula.as_ref() {
                    Formula::PropVar(_, _)
                    | Formula::Box(_)
                    | Formula::Diamond(_)
                    | Formula::DiamondGe(_, _)
                    | Formula::DiamondLe(_, _) => formulae.push(f.clone()),
                    Formula::Not(formula) => match formula.as_ref() {
                        Formula::PropVar(_, _) => formulae.push(f.clone()),
                        _ => {}
                    },
                    _ => {}
                };
                true
            });
            let node = NodeInner {
                count: 1,
                formulae,
                position: NodePosition { x: 0, y: 0 },
            };
            graph.adjlist.push((node, vec![]));
            match &fruit.borrow().children {
                TabChildren::Fork { .. } => {
                    unreachable!("Fruit should not have fork children.")
                }
                TabChildren::Leaf => {}
                TabChildren::Transition(transit) => transit.to_graph_rec(0, &mut graph),
            }
            break;
        }
        graph
    }
}

impl TransitKOr45 {
    pub(crate) fn to_graph_rec(&self, parent_id: usize, graph: &mut GraphInner) {
        let mut fruits = vec![];
        TableauNode2::get_fruits(&self.paraws.tab, &mut fruits);
        for (c, choices) in self.solution.iter().zip(self.paraws.choices.iter()) {
            if *c == 0 {
                continue;
            }
            for fruit in fruits.iter() {
                let mut fruitchoices = vec![];
                fruit
                    .borrow()
                    .get_choices(&mut fruitchoices, &self.paraws.forkids);
                if &fruitchoices != choices {
                    continue;
                }
                let target = graph.adjlist.len();
                graph.adjlist[parent_id]
                    .1
                    .push(EdgeInner { target, sym: false });
                let mut formulae = vec![];
                fruit.borrow().traverse_anc_formulae(&mut |f| {
                    match f.formula.as_ref() {
                        Formula::PropVar(_, _)
                        | Formula::Box(_)
                        | Formula::Diamond(_)
                        | Formula::DiamondGe(_, _)
                        | Formula::DiamondLe(_, _) => formulae.push(f.clone()),
                        Formula::Not(formula) => match formula.as_ref() {
                            Formula::PropVar(_, _) => formulae.push(f.clone()),
                            _ => {}
                        },
                        _ => {}
                    };
                    true
                });
                let node = NodeInner {
                    count: *c as usize,
                    formulae,
                    position: NodePosition { x: 0, y: 0 },
                };
                graph.adjlist.push((node, vec![]));
                match &fruit.borrow().children {
                    TabChildren::Fork { .. } => {
                        unreachable!("Fruit should not have fork children.")
                    }
                    TabChildren::Leaf => {}
                    TabChildren::Transition(nexttransit) => nexttransit.to_graph_rec(target, graph),
                }
                break;
            }
        }
    }
}
