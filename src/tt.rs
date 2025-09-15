use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
    ops::RangeInclusive,
    rc::Rc,
};

use good_lp::{Expression, ProblemVariables, Solution, SolverModel, Variable, solvers, variable};

use crate::{
    formula::Formula,
    model::{EdgeInner, EdgeView, GraphInner, IntoModelGraph, NodeInner, NodePosition, NodeView},
    rules3::{Calculus, Feasibility},
    tableau2::{DisplayTableau, LabeledFormula, TabChildren, TableauNode2},
    timeout::{MayTimeout, TimeoutHandler},
    transit::{
        BaseTransit, Constraints, DisplayTransit, Grading, Modals, ModelTransit, ParallelWorlds,
    },
};

pub(crate) struct TransitT {
    pub(crate) is_reflexive: bool,
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) ranges: Vec<RangeInclusive<usize>>,
    pub(crate) vars: Vec<Variable>,
    pub(crate) solution: Vec<u32>,
}

impl BaseTransit for TransitT {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>> {
        Self::reflect(fruit, vec![], None.into_iter(), calc, toh)
    }
}

impl TransitT {
    fn from_reflection(
        modals: Modals,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        mut labels: Vec<LabeledFormula>,
        mut ranges: Vec<RangeInclusive<usize>>,
        src_constraints: impl Iterator<Item = Grading>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self> {
        let (forkids, mut constraints) = modals.to_forks_constraints(&mut calc.forks);
        for lab in labels.iter_mut() {
            lab.lemma = true;
        }
        let paraws = ParallelWorlds::<Self>::from_forks(
            labels,
            forkids.clone().map_or(vec![], |f| vec![f]),
            Some(fruit),
            calc,
            toh,
        )?;
        ranges.extend(forkids.into_iter());
        constraints.gradings.extend(src_constraints);
        let feasibility = paraws.tab.borrow().feasibility;
        Ok(Self {
            is_reflexive: true,
            feasibility,
            paraws,
            constraints,
            vars: vec![],
            solution: vec![],
            ranges,
        })
    }

    fn recurse(&mut self, calc: &mut Calculus, toh: &impl TimeoutHandler) -> MayTimeout<()> {
        if self.is_closed() {
            return Ok(());
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        for flower in flowers {
            let subtransit = if self.is_reflexive {
                Self::reflect(
                    &flower,
                    self.ranges.clone(),
                    self.constraints.gradings.iter().cloned(),
                    calc,
                    toh,
                )?
            } else {
                Self::reflect(&flower, self.ranges.clone(), None.into_iter(), calc, toh)?
            };
            if let Some(subtransit) = subtransit {
                flower.borrow_mut().feasibility = subtransit.feasibility;
                flower.borrow_mut().children = TabChildren::Transition(subtransit);
            }
        }
        self.feasibility = TableauNode2::set_feasibility_rec(&self.paraws.tab);
        Ok(())
    }

    pub(crate) fn reflect(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        ranges: Vec<RangeInclusive<usize>>,
        constraints: impl Iterator<Item = Grading>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>> {
        let mut labels = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            labels.push(label.clone());
            true
        });
        let modals = Modals::new(labels.iter().filter(|lab| !lab.lemma), false, false, toh)?;
        if modals.ge.is_empty() && modals.le.is_empty() {
            if let Some(mut transit) = Self::transition(
                fruit,
                labels.iter(),
                ranges.clone(),
                constraints.collect(),
                calc,
                toh,
            )? {
                let mut choices = Vec::new();
                Self::get_choices(fruit, &mut choices, &ranges);
                // transit.set_choices();
                transit.full_solve(&choices, toh)?;
                return Ok(Some(transit));
            } else {
                return Ok(None);
            }
        }
        let mut transit =
            Self::from_reflection(modals, fruit, labels, ranges, constraints, calc, toh)?;
        if transit.is_closed() {
            return Ok(Some(transit));
        }
        transit.recurse(calc, toh)?;
        if transit.is_closed() {
            return Ok(Some(transit));
        }
        Ok(Some(transit))
    }

    fn transition<'a>(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        labels: impl Iterator<Item = &'a LabeledFormula>,
        ranges: Vec<RangeInclusive<usize>>,
        constraints: Vec<Grading>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>> {
        if ranges.is_empty() {
            return Ok(None);
        }
        let boxsubforms: Vec<_> = labels
            .filter_map(|lab| {
                if let Formula::Box(phi) = lab.formula.as_ref() {
                    Some(LabeledFormula {
                        formula: phi.clone(),
                        conflictset: lab.conflictset.clone(),
                        lemma: false,
                        expanded: false,
                    })
                } else {
                    None
                }
            })
            .collect();
        let paraws = ParallelWorlds::<Self>::from_forks(
            boxsubforms.clone(),
            ranges,
            Some(fruit),
            calc,
            toh,
        )?;
        let feasibility = paraws.tab.borrow().feasibility;
        let mut subtransit = Self {
            is_reflexive: false,
            feasibility,
            paraws,
            constraints: Constraints {
                gradings: constraints,
                boxsubforms,
            },
            vars: vec![],
            solution: vec![],
            ranges: vec![],
        };
        if subtransit.is_closed() {
            return Ok(Some(subtransit));
        }
        subtransit.recurse(calc, toh)?;
        if subtransit.is_closed() {
            return Ok(Some(subtransit));
        }
        Ok(Some(subtransit))
    }
}

impl TransitT {
    pub(crate) fn get_choices(
        tab: &Rc<RefCell<TableauNode2<TransitT>>>,
        choices: &mut Vec<(usize, usize)>,
        forkids: &Vec<RangeInclusive<usize>>,
    ) {
        // ? OPT: bin search + remove ?
        if let Some(parent) = tab.borrow().parent.upgrade() {
            match &parent.borrow().children {
                TabChildren::Transition(transit) if !transit.is_reflexive => {}
                _ => parent.borrow().get_choices(choices, forkids),
            }
        }
        choices.extend(
            tab.borrow()
                .choices
                .iter()
                .filter(|(forkid, _)| forkids.iter().any(|r| r.contains(forkid))),
        )
    }

    pub(crate) fn full_solve(
        &mut self,
        src_choices: &Vec<(usize, usize)>,
        // ranges: &Vec<RangeInclusive<usize>>,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<()> {
        self.paraws.set_choices(true);
        let mut problem = ProblemVariables::new();
        let mut exprs = HashMap::with_capacity(self.constraints.gradings.len());
        for c in &self.constraints.gradings {
            exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        }
        let vars = problem.add_vector(variable().integer().min(0), self.paraws.choices.len());
        let rflxvar = problem.add_vector(variable().integer().min(1).max(1).initial(1), 1)[0];
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
        for (forkid, branchid) in src_choices {
            if *branchid == 1 {
                exprs
                    .get_mut(forkid)
                    .expect("Forkid should have been entered into hashmap")
                    .2
                    .push(&rflxvar);
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

impl DisplayTransit for TransitT {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(
            f,
            "{rooti} {}: {}",
            if self.is_reflexive { "[self]" } else { "" },
            self.feasibility.symbol()
        )?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        if self.is_reflexive {
            return writeln!(f);
        }
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
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
                write!(f, "{val}*w{i} ")?;
            }
            writeln!(f)?;
        }
        writeln!(f)
    }
}

impl ModelTransit for TransitT {
    fn to_graph_inner(this: DisplayTableau<Self>) -> GraphInner {
        let mut fruits = vec![];
        TableauNode2::get_fruits(&this.0, &mut fruits);
        let mut graph = GraphInner { adjlist: vec![] };
        for fruit in fruits {
            if fruit.borrow().is_closed() {
                continue;
            }
            match &fruit.borrow().children {
                TabChildren::Fork { .. } => unreachable!("Fruit should not have fork children."),
                TabChildren::Transition(transit) if transit.is_reflexive => {
                    transit.to_graph_reflexive(None, 1, &mut graph);
                    break;
                }
                TabChildren::Transition(_) | TabChildren::Leaf => {}
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
                    unreachable!("This option should have been handled earlier.")
                }
                TabChildren::Transition(transit) if transit.is_reflexive => {
                    unreachable!("This option should have been handled earlier.")
                }
                TabChildren::Transition(transit) => transit.to_graph_rec(0, &mut graph),
                TabChildren::Leaf => {}
            }
            break;
        }
        graph
    }
}

impl TransitT {
    pub(crate) fn to_graph_reflexive(
        &self,
        parent_id: Option<usize>,
        count: usize,
        graph: &mut GraphInner,
    ) {
        let mut fruits = vec![];
        TableauNode2::get_fruits(&self.paraws.tab, &mut fruits);
        for fruit in fruits {
            if fruit.borrow().is_closed() {
                continue;
            }
            match &fruit.borrow().children {
                TabChildren::Fork { .. } => unreachable!("Fruit should not have fork children."),
                TabChildren::Transition(transit) if transit.is_reflexive => {
                    return transit.to_graph_reflexive(parent_id, count, graph);
                }
                TabChildren::Transition(_) | TabChildren::Leaf => {}
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
            let target = graph.adjlist.len();
            if let Some(parent_id) = parent_id {
                graph.adjlist[parent_id]
                    .1
                    .push(EdgeInner { target, sym: false });
            }
            let node = NodeInner {
                count,
                formulae,
                position: NodePosition { x: 0, y: 0 },
            };
            graph.adjlist.push((node, vec![]));
            match &fruit.borrow().children {
                TabChildren::Fork { .. } => {
                    unreachable!("Fruit should not have fork children.")
                }
                TabChildren::Transition(transit) if transit.is_reflexive => {
                    unreachable!("This option should have been handled earlier.")
                }
                TabChildren::Transition(transit) => transit.to_graph_rec(target, graph),
                TabChildren::Leaf => {}
            }
            break;
        }
    }

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
                match &fruit.borrow().children {
                    TabChildren::Fork { .. } => {
                        unreachable!("Fruit should not have fork children.")
                    }
                    TabChildren::Transition(transit) if transit.is_reflexive => {
                        transit.to_graph_reflexive(Some(parent_id), *c as usize, graph);
                        break;
                    }
                    TabChildren::Transition(_) | TabChildren::Leaf => {}
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
                    TabChildren::Transition(transit) if transit.is_reflexive => {
                        unreachable!("This option should have been handled earlier.")
                    }
                    TabChildren::Transition(transit) => transit.to_graph_rec(target, graph),
                    TabChildren::Leaf => {}
                }
                break;
            }
        }
    }
}
