use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
    rc::{Rc, Weak},
};

use good_lp::{Expression, ProblemVariables, Solution, SolverModel, solvers, variable};

use crate::{
    formula::Formula,
    model::{EdgeView, IntoModelGraph, NodeView},
    rules3::{Calculus, Feasibility},
    tableau2::{DisplayTableau, LabeledFormula, TabChildren, TableauNode2},
    timeout::{MayTimeout, TimeoutHandler},
    transit::{
        self, BaseTransit, Constraints, DisplayTransit, Modals, ParallelWorlds, SolveTransit,
    },
};

pub(crate) struct TransitB<const R: bool> {
    pub(crate) feasibility: Feasibility,
    pub(crate) backworld: Weak<RefCell<TableauNode2<Self>>>,
    pub(crate) reflexworld: Weak<RefCell<TableauNode2<Self>>>,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
}

pub(crate) enum TransitResult<T> {
    None,
    BackContra(Rc<Formula>, Rc<Formula>),
    Transit(T),
}

impl<const R: bool> BaseTransit for TransitB<R> {
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

impl<const R: bool> SolveTransit for TransitB<R> {
    fn recurse(&mut self, calc: &mut Calculus, toh: &impl TimeoutHandler) -> MayTimeout<()> {
        if self.is_closed() {
            return Ok(());
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        for flower in flowers {
            match Self::full_transit(calc, &flower, self.reflexworld.clone(), toh)? {
                TransitResult::None => {}
                TransitResult::BackContra(x, y) => {
                    flower.borrow_mut().formulae.push(LabeledFormula {
                        formula: y.box_(),
                        conflictset: vec![],
                        lemma: false,
                        expanded: true,
                    });
                    flower.borrow_mut().add_check_dup_contra(LabeledFormula {
                        formula: Formula::bottom(),
                        conflictset: vec![],
                        lemma: false,
                        expanded: true,
                    });
                }
                TransitResult::Transit(transit) => {
                    flower.borrow_mut().feasibility = transit.feasibility();
                    flower.borrow_mut().children = TabChildren::Transition(transit);
                }
            }
        }
        self.feasibility = TableauNode2::set_feasibility_rec(&self.paraws.tab);
        Ok(())
    }

    fn solve(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<()> {
        Ok(())
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self> {
        let mut formulae = vec![];
        leaf.borrow().traverse_anc_formulae(&mut |formula| {
            formulae.push(formula.clone());
            true
        });
        for formula in &mut formulae {
            formula.lemma = true;
        }
        let mut metamodals = vec![modals];
        while let Some(currmodals) = metamodals.last()
            && !currmodals.ge.is_empty()
        {
            let subforms = currmodals.submodals();
            let newmodals = Modals::new(subforms.iter(), false, false, toh)?;
            metamodals.push(newmodals);
        }
        let constraints = Constraints {
            gradings: vec![],
            boxsubforms: vec![],
        };
        let mut forkids = vec![];
        for modal in metamodals
            .into_iter()
            .skip(if R { 0 } else { 1 })
            .step_by(if R { 1 } else { 2 })
        {
            let (fkids, cns, _) = modal.to_box_forks_constraints(&mut calc.forks);
            forkids.extend(fkids);
            // constraints.gradings.extend(cns.gradings);
        }
        let paraws = ParallelWorlds::from_forks(formulae, forkids, Some(leaf), calc, toh)?;
        let feasibility = paraws.tab.borrow().feasibility;
        Ok(Self {
            feasibility,
            backworld: Weak::new(),
            reflexworld: Weak::new(),
            paraws,
            constraints,
            solution: vec![],
        })
    }
}

impl<const R: bool> TransitB<R> {
    fn full_transit(
        calc: &mut Calculus,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        back_fruit: Weak<RefCell<TableauNode2<Self>>>,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<TransitResult<Self>> {
        let mut formulae = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |formula| {
            formulae.push(formula.clone());
            true
        });
        let modals = Modals::new(formulae.iter(), back_fruit.upgrade().is_none(), false, toh)?;
        if let Some(backfruit) = back_fruit.upgrade() {
            let mut backcontra = None;
            // for formula in modals.bx.iter() {
            //     println!("F: {}", formula.formula);
            // }
            backfruit
                .borrow()
                .traverse_anc_formulae(&mut |backformula| {
                    // println!("B: {}", backformula.formula);
                    let mut keeptrying = true;
                    for formula in modals.bx.iter() {
                        if backformula.formula.directly_contradicts(&formula.formula) {
                            backcontra =
                                Some((backformula.formula.clone(), formula.formula.clone()));
                            keeptrying = false;
                            break;
                        }
                    }
                    keeptrying
                });
            // println!();
            if let Some((f1, f2)) = backcontra {
                return Ok(TransitResult::BackContra(f1, f2));
            }
        }
        if modals.ge.is_empty() {
            return Ok(TransitResult::None);
        }
        let mut transit = Self::proper_from_modals(modals, fruit, calc, back_fruit, toh)?;
        if transit.is_closed() {
            return Ok(TransitResult::Transit(transit));
        }
        transit.recurse(calc, toh)?;
        if transit.is_closed() {
            return Ok(TransitResult::Transit(transit));
        }
        transit.full_solve(toh)?;
        Ok(TransitResult::Transit(transit))
    }

    fn proper_from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        back_fruit: Weak<RefCell<TableauNode2<Self>>>,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self> {
        let mut metamodals = vec![modals];
        while let Some(currmodals) = metamodals.last()
            && !currmodals.ge.is_empty()
        {
            let subforms = currmodals.submodals();
            let newmodals = Modals::new(subforms.iter(), false, false, toh)?;
            metamodals.push(newmodals);
        }
        let mut metamodals = metamodals.into_iter();
        let (forkids, mut constraints) = metamodals
            .next()
            .expect("Vec starts with one element")
            .to_forks_constraints(&mut calc.forks);
        let mut forkids: Vec<_> = forkids.into_iter().collect();
        for modal in metamodals
            .skip(if R { 0 } else { 1 })
            .step_by(if R { 1 } else { 2 })
        {
            let (fkids, cns, _) = modal.to_box_forks_constraints(&mut calc.forks);
            forkids.extend(fkids);
        }
        let paraws = ParallelWorlds::from_forks(
            constraints.boxsubforms.clone(),
            forkids,
            Some(leaf),
            calc,
            toh,
        )?;
        let feasibility = paraws.tab.borrow().feasibility;
        Ok(Self {
            feasibility,
            backworld: back_fruit,
            reflexworld: Rc::downgrade(leaf),
            paraws,
            constraints,
            solution: vec![],
        })
    }

    fn full_solve(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<()> {
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
                    let entry = exprs.get_mut(forkid);
                    if let Some(entry) = entry {
                        entry
                            // .expect("Forkid should have been entered into hashmap")
                            .2
                            .push(var);
                    } else {
                        // println!("P: {}", DisplayTableau(self.paraws.tab.clone()));
                        // panic!();
                    }
                }
            }
        }
        let backvar = problem.add_vector(variable().integer().min(1).max(1).initial(1), 1)[0];
        if let Some(backworld) = self.backworld.upgrade() {
            for c in &self.constraints.gradings {
                backworld
                    .borrow()
                    .traverse_anc_formulae(&mut |backformula| {
                        let mut keeptrying = true;
                        if backformula.formula.directly_contradicts(&c.formula) {
                            keeptrying = false;
                        } else if backformula.formula.directly_equivalent(&c.formula) {
                            keeptrying = false;
                            exprs
                                .get_mut(&c.forkid)
                                .expect("Forkid should have been entered into hashmap")
                                .2
                                .push(&backvar);
                        }
                        keeptrying
                    });
            }
        }
        let reflexvar;
        if R {
            reflexvar = problem.add_vector(variable().integer().min(1).max(1).initial(1), 1)[0];
            if let Some(reflexworld) = self.reflexworld.upgrade() {
                for c in &self.constraints.gradings {
                    reflexworld
                        .borrow()
                        .traverse_anc_formulae(&mut |reflexformula| {
                            let mut keeptrying = true;
                            if reflexformula.formula.directly_contradicts(&c.formula) {
                                keeptrying = false;
                            } else if reflexformula.formula.directly_equivalent(&c.formula) {
                                keeptrying = false;
                                exprs
                                    .get_mut(&c.forkid)
                                    .expect("Forkid should have been entered into hashmap")
                                    .2
                                    .push(&reflexvar);
                            }
                            keeptrying
                        });
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

impl<const R: bool> DisplayTransit for TransitB<R> {
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
            if self.reflexworld.strong_count() == 0 {
                "[self]"
            } else {
                ""
            },
            self.feasibility.symbol()
        )?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        if self.reflexworld.strong_count() == 0 {
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

impl<const R: bool> IntoModelGraph for TransitB<R> {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<NodeView>, edges: &mut Vec<EdgeView>) {
        if false {
            let selfi = nodes.len();
            let selfid = selfi.to_string();
            nodes.push(NodeView {
                id: selfid.clone(),
                label: format!("#{selfi}"),
                extra: String::new(),
            });
            edges.push(EdgeView {
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
