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
    tableau2::{DisplayTableau, LabeledFormula, TabChildren, TableauNode2},
    timeout::{MayTimeout, TimeoutHandler},
    transit::{
        BaseTransit, Constraints, DisplayTransit, Modals, ModelTransit, ParallelWorlds,
        SolveTransit, general_transit,
    },
};

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

    fn transit(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<TransitB5>> {
        general_transit(calc, fruit, toh)
    }
}

impl SolveTransit for TransitB5 {
    fn recurse(&mut self, _calc: &mut Calculus, _toh: &impl TimeoutHandler) -> MayTimeout<()> {
        Ok(())
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<TransitB5> {
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc, toh)?;
        let feasibility = paraws.tab.borrow().feasibility;
        let reflexion = if paraws.tab.borrow().is_closed() {
            ParallelWorlds::from_forks(vec![], vec![], Some(leaf), calc, toh)?
        } else {
            Self::get_reflexion(constraints.boxsubforms.clone(), &paraws, leaf, calc, toh)?
        };
        Ok(Self {
            feasibility,
            paraws,
            constraints,
            solution: vec![],
            rfxsolution: 0,
            reflexion,
        })
    }

    fn solve(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<()> {
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
        toh.timedout()?;
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
        Ok(())
    }
}

impl TransitB5 {
    fn get_reflexion(
        mut boxsubforms: Vec<LabeledFormula>,
        paraws: &ParallelWorlds<Self>,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<ParallelWorlds<Self>> {
        leaf.borrow().traverse_anc_formulae(&mut |l| {
            boxsubforms.push(l.clone());
            true
        });
        ParallelWorlds::from_forks(boxsubforms, paraws.forkids.clone(), Some(leaf), calc, toh)
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

impl ModelTransit for TransitB5 {
    fn to_graph_inner(this: DisplayTableau<Self>) -> GraphInner {
        let mut fruits = vec![];
        TableauNode2::get_fruits(&this.0, &mut fruits);
        let mut graph = GraphInner { adjlist: vec![] };
        for fruit in fruits {
            if fruit.borrow().is_closed() {
                continue;
            }
            match &fruit.borrow().children {
                TabChildren::Leaf => {}
                TabChildren::Fork { .. } => unreachable!("Fruit should not have fork children"),
                TabChildren::Transition(transit) => {
                    return transit.to_graph_starting();
                }
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
            break;
        }
        graph
    }
}

impl TransitB5 {
    fn to_graph_starting(&self) -> GraphInner {
        let mut graph = GraphInner { adjlist: vec![] };
        let mut fruits = vec![];
        TableauNode2::get_fruits(&self.reflexion.tab, &mut fruits);
        let choices = &self.reflexion.choices[self.rfxsolution];
        for fruit in fruits.iter() {
            let mut fruitchoices = vec![];
            fruit
                .borrow()
                .get_choices(&mut fruitchoices, &self.paraws.forkids);
            if &fruitchoices != choices {
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
            self.to_graph_clique(&mut graph);
            break;
        }
        graph
    }

    pub(crate) fn to_graph_clique(&self, graph: &mut GraphInner) {
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
                graph.adjlist[0].1.push(EdgeInner { target, sym: false });
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
                break;
            }
        }
    }
}
