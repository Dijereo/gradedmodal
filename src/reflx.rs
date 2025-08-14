use std::{cell::RefCell, collections::{HashMap, VecDeque}, fmt, ops::RangeInclusive, rc::Rc};

use good_lp::{solvers, variable, Expression, ProblemVariables, Solution, SolverModel, Variable};

use crate::{formula::Formula, model::{Edge, IntoModelGraph, Node}, rules3::{Calculus, Feasibility}, tableau2::{LabeledFormula, TabChildren, TableauNode2}, transit::{BaseTransit, Constraints, DisplayTransit, Grading, Modals, ParallelWorlds}};

pub(crate) struct TransitT {
    pub(crate) reflexion: bool,
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

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        Self::reflect(fruit, vec![], None.into_iter(), calc)
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
    ) -> Self {
        let (forkids, mut constraints) = modals.to_forks_constraints(&mut calc.forks);
        for lab in labels.iter_mut() {
            lab.lemma = true;
        }
        let paraws = ParallelWorlds::<Self>::from_forks(
            labels,
            forkids.clone().map_or(vec![], |f| vec![f]),
            Some(fruit),
            calc,
        );
        ranges.extend(forkids.into_iter());
        constraints.gradings.extend(src_constraints);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            reflexion: true,
            feasibility,
            paraws,
            constraints,
            vars: vec![],
            solution: vec![],
            ranges,
        }
    }

    fn recurse(&mut self, calc: &mut Calculus) {
        if self.is_closed() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        for flower in flowers {
            let subtransit = if self.reflexion {
                Self::reflect(
                    &flower,
                    self.ranges.clone(),
                    self.constraints.gradings.iter().cloned(),
                    calc,
                )
            } else {
                Self::reflect(&flower, self.ranges.clone(), None.into_iter(), calc)
            };
            if let Some(subtransit) = subtransit {
                flower.borrow_mut().feasibility = subtransit.feasibility;
                flower.borrow_mut().children = TabChildren::Transition(subtransit);
            }
        }
        self.feasibility = TableauNode2::set_feasibility_rec(&self.paraws.tab);
    }

    pub(crate) fn reflect(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        ranges: Vec<RangeInclusive<usize>>,
        constraints: impl Iterator<Item = Grading>,
        calc: &mut Calculus,
    ) -> Option<Self> {
        let mut labels = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            labels.push(label.clone());
            true
        });
        let modals = Modals::new(labels.iter().filter(|lab| !lab.lemma), false, false);
        if modals.ge.is_empty() && modals.le.is_empty() {
            if let Some(mut transit) = Self::transition(
                fruit,
                labels.iter(),
                ranges.clone(),
                constraints.collect(),
                calc,
            ) {
                let mut choices = Vec::new();
                Self::get_choices(fruit, &mut choices, &ranges);
                // transit.set_choices();
                transit.full_solve(&choices);
                return Some(transit);
            } else {
                return None;
            }
        }
        let mut transit = Self::from_reflection(modals, fruit, labels, ranges, constraints, calc);
        if transit.is_closed() {
            return Some(transit);
        }
        transit.recurse(calc);
        if transit.is_closed() {
            return Some(transit);
        }
        Some(transit)
    }

    fn transition<'a>(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        labels: impl Iterator<Item = &'a LabeledFormula>,
        ranges: Vec<RangeInclusive<usize>>,
        constraints: Vec<Grading>,
        calc: &mut Calculus,
    ) -> Option<Self> {
        if ranges.is_empty() {
            return None;
        }
        let boxsubforms: Vec<_> = labels
            .filter_map(|lab| {
                if let Formula::Box(phi) = lab.formula.as_ref() {
                    Some(LabeledFormula {
                        formula: phi.clone(),
                        conflictset: lab.conflictset.clone(),
                        lemma: false,
                    })
                } else {
                    None
                }
            })
            .collect();
        let paraws =
            ParallelWorlds::<Self>::from_forks(boxsubforms.clone(), ranges, Some(fruit), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        let mut subtransit = Self {
            reflexion: false,
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
            return Some(subtransit);
        }
        subtransit.recurse(calc);
        if subtransit.is_closed() {
            return Some(subtransit);
        }
        Some(subtransit)
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
                TabChildren::Transition(transit) if !transit.reflexion => {}
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

    pub(crate) fn set_choices(&mut self, forkids: &Vec<RangeInclusive<usize>>) {
        // let mut choices = vec![];
        // self.
        // .tab
        // .borrow()
        // .choices
        // .iter()
        // .filter(|(forkid, _)| forkids.iter().any(|r| r.contains(forkid)));
        todo!();
    }

    fn set_choices_rec(
        tab: &Rc<RefCell<TableauNode2<TransitT>>>,
        mut src_choices: Vec<(usize, usize)>,
        out_choices: &mut Vec<Vec<(usize, usize)>>,
        ranges: &Vec<RangeInclusive<usize>>,
    ) {
        // src_choices.extend(tab.borrow().choices.iter());
        // match &tab.borrow().children {
        //     TabChildren::Leaf => todo!(),
        //     TabChildren::Fork { branches, .. } => {
        //         for branch in branches {
        //             Self::set_choices_rec(&branch.node, src_choices.clone(), out_choices, ranges);
        //         }
        //     },
        //     TabChildren::Transition(transit) if transit.reflexion => {

        //     },
        // }
        // todo!();
    }

    pub(crate) fn full_solve(
        &mut self,
        src_choices: &Vec<(usize, usize)>,
        // ranges: &Vec<RangeInclusive<usize>>,
    ) {
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
        match model.solve() {
            Ok(solution) => {
                self.solution = vars.into_iter().map(|v| solution.value(v) as u32).collect();
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
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
            if self.reflexion { "[self]" } else { "" },
            self.feasibility.symbol()
        )?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        if self.reflexion {
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


impl IntoModelGraph for TransitT {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        if !self.reflexion {
            let selfi = nodes.len();
            let selfid = selfi.to_string();
            nodes.push(Node {
                id: selfid.clone(),
                label: format!("#{selfi}"),
                extra: String::new(),
            });
            edges.push(Edge {
                source: parenti.to_string(),
                target: selfid,
                label: String::new(),
                extra: String::new(),
            });
            self.paraws.tab.borrow().model_graph(selfi, nodes, edges);
        } else {
            self.paraws.tab.borrow().model_graph(parenti, nodes, edges);
        }
    }
}

