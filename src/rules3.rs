use std::{cell::RefCell, collections::VecDeque, fmt, mem, ops::RangeInclusive, rc::Rc};

use crate::{
    depth1::Depth1F,
    formula::Formula,
    frame::FrameCondition,
    tableau2::{Conflict, DupContra, Label, TabBranch, TabChildren, TableauNode2},
};

struct PropLinear;
struct PropFork;
struct TLinear;

#[derive(Clone, Debug)]
enum ForkType {
    Disjunction,
    ParallelWorlds,
}

#[derive(Clone, Debug)]
struct Branch {
    id: usize,
    labels: Vec<Label>,
}

#[derive(Clone, Debug)]
struct Fork {
    id: usize,
    fktype: ForkType,
    branches: Vec<Branch>,
}

pub(crate) struct GradedKCalc {
    framecond: FrameCondition,
    forks: ForkStore,
}

pub(crate) struct ForkStore {
    forks: Vec<Fork>,
}

// TODO: Combine No Solution and Infeasible 
#[derive(Clone, Copy)]
pub(crate) enum Feasibility {
    Feasible,
    Infeasible,
    NoSolution,
    Contradiction,
}

pub(crate) struct Modals {
    pub(crate) bx: Vec<Label>,
    pub(crate) ge: Vec<(u32, Label)>,
    pub(crate) le: Vec<(u32, Label)>,
}

#[derive(Debug)]
pub(crate) struct Constraint {
    pub(crate) forkid: usize,
    pub(crate) sense: bool,
    pub(crate) value: u32,
    pub(crate) formula: Rc<Formula>,
}

pub(crate) struct ParallelWorlds {
    pub(crate) tab: Rc<RefCell<TableauNode2>>,
    pub(crate) forkid_ranges: Vec<RangeInclusive<usize>>,
    pub(crate) choices: Vec<Vec<(usize, usize)>>,
}

pub(crate) enum Transit {
    KOr45(TransitKOr45),
    B5(TransitB5),
    K5(Transit5),
}

pub(crate) struct TransitKOr45 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitB5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds,
    pub(crate) reflexion: ParallelWorlds,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) solution: Vec<u32>,
    pub(crate) rfxsolution: usize,
}

pub(crate) struct Transit5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) modals: Modals,
    pub(crate) submodals: Vec<Label>,
    pub(crate) spotconstraints: Vec<Constraint>,
    pub(crate) paracliques: Vec<ParaClique>,
}

pub(crate) struct ParaClique {
    pub(crate) feasibility: Feasibility,
    pub(crate) settings: Vec<bool>,
    pub(crate) spotws: ParallelWorlds,
    pub(crate) cliquews: ParallelWorlds,
    pub(crate) cliqueconstraints: Vec<Constraint>,
    pub(crate) spotsolution: Vec<u32>,
    pub(crate) cliquesolution: Vec<u32>,
}

impl GradedKCalc {
    pub(crate) fn sat(
        mut formulae: Vec<Rc<Formula>>,
        framecond: FrameCondition,
    ) -> Rc<RefCell<TableauNode2>> {
        if framecond.luminal() {
            for f in formulae.iter_mut() {
                *f = Depth1F::from(f.clone()).into();
            }
        }
        let labels = formulae
            .into_iter()
            .map(|f| Label {
                formula: f,
                conflictset: vec![],
            })
            .collect();
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(labels, None)));
        let mut calc = Self {
            framecond,
            forks: ForkStore { forks: vec![] },
        };
        if tab.borrow().is_closed() {
            return tab;
        }
        calc.expand_static(&tab, VecDeque::new(), true);
        if tab.borrow().is_closed() {
            return tab;
        }
        calc.transition_rec(&tab);
        tab
    }

    fn transition_rec(&mut self, tab: &Rc<RefCell<TableauNode2>>) {
        if tab.borrow().is_closed() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(tab, &mut flowers);
        let mut feasibility = Feasibility::Infeasible;
        for flower in flowers {
            let transit = self.transit(&flower);
            match transit {
                Some(transit) => {
                    if let Feasibility::Feasible = transit.feasibility() {
                        feasibility = Feasibility::Feasible;
                    }
                    flower.borrow_mut().feasibility = transit.feasibility();
                    flower.borrow_mut().children = TabChildren::Transition(transit);
                }
                None => feasibility = Feasibility::Feasible,
            }
        }
        // TODO: Set entire tree of feasibility
        tab.borrow_mut().feasibility = feasibility;
    }

    fn expand_static(
        &mut self,
        tab: &Rc<RefCell<TableauNode2>>,
        mut forks: VecDeque<Fork>,
        isroot: bool,
    ) {
        let reflexive = self.framecond.reflexive() || !isroot && self.framecond.euclidean();
        self.expand_linear(&mut tab.borrow_mut(), reflexive);
        if tab.borrow().is_closed() {
            return;
        }
        self.store_disjs(&tab.borrow(), &mut forks);
        self.resolve_forks(&tab, &mut forks);
        if tab.borrow().is_closed() {
            return;
        }
        self.apply_forks(tab, forks, isroot);
    }

    fn expand_linear(&self, tab: &mut TableauNode2, reflexive: bool) {
        let mut i = 0;
        while let Some(label) = tab.formulae.get(i).cloned() {
            let mut new_formulae = PropLinear.expand(&label.formula);
            if reflexive {
                new_formulae.extend(TLinear.expand(&label.formula));
            }
            for new_formula in new_formulae {
                tab.add_check_dup_contra(Label {
                    formula: new_formula,
                    conflictset: label.conflictset.clone(),
                });
                if tab.is_closed() {
                    return;
                }
            }
            i += 1;
        }
    }

    fn store_disjs(&mut self, tab: &TableauNode2, forks: &mut VecDeque<Fork>) {
        for label in &tab.formulae {
            let branches = PropFork.expand(&label.formula);
            if branches.is_empty() {
                continue;
            }
            forks.push_back(self.forks.create_fork(
                ForkType::Disjunction,
                branches,
                &label.conflictset,
            ));
        }
    }

    fn resolve_forks(&mut self, tab: &Rc<RefCell<TableauNode2>>, forks: &mut VecDeque<Fork>) {
        let mut unresolved = VecDeque::new();
        while let Some(mut fork) = forks.pop_front() {
            let mut conflictset = vec![];
            let mut i = 0;
            'outer: while let Some(branch) = fork.branches.get_mut(i) {
                let mut j = 0;
                'inner: while let Some(label) = branch.labels.get_mut(j) {
                    match tab.borrow().check_dup_contra(&label.formula) {
                        DupContra::Ok => {}
                        DupContra::Bottom => {
                            fork.branches.swap_remove(i);
                            continue 'outer;
                        }
                        DupContra::Dup(conflicts) => {
                            conflictset.extend(conflicts);
                            conflictset.extend(label.conflictset.clone());
                            branch.labels.swap_remove(j);
                            continue 'inner;
                        }
                        DupContra::Contra(conflicts) => {
                            conflictset.extend(conflicts);
                            conflictset.extend(label.conflictset.clone());
                            fork.branches.swap_remove(i);
                            continue 'outer;
                        }
                    }
                    j += 1;
                }
                if branch.labels.is_empty() {
                    break 'outer;
                }
                i += 1;
            }
            conflictset.sort();
            conflictset.dedup();
            if fork.branches.is_empty() {
                let confs = tab.borrow_mut().add_check_dup_contra(Label {
                    formula: Formula::bottom(),
                    conflictset,
                });
                return;
            } else if fork.branches.len() == 1 {
                let branch = fork.branches.pop().expect("Checked in if statement above");
                tab.borrow_mut().choices.push((fork.id, branch.id));
                for label in branch.labels {
                    let confs = tab.borrow_mut().add_check_dup_contra(Label {
                        formula: label.formula,
                        conflictset: conflictset.clone(),
                    });
                    if tab.borrow().is_closed() {
                        return;
                    }
                }
                forks.append(&mut unresolved);
            } else if let Some(_) = fork.branches.iter().find(|b| b.labels.is_empty()) {
                let branch = fork.branches.pop().expect("Checked by if let find");
                tab.borrow_mut().choices.push((fork.id, branch.id));
                for label in branch.labels {
                    let confs = tab.borrow_mut().add_check_dup_contra(Label {
                        formula: label.formula,
                        conflictset: conflictset.clone(),
                    });
                    if tab.borrow().is_closed() {
                        return;
                    }
                }
                forks.append(&mut unresolved);
            } else {
                unresolved.push_back(fork);
            }
        }
        forks.append(&mut unresolved);
    }

    fn apply_forks(
        &mut self,
        tab: &Rc<RefCell<TableauNode2>>,
        mut forks: VecDeque<Fork>,
        isroot: bool,
    ) {
        loop {
            match &tab.borrow().children {
                TabChildren::Leaf => {}
                TabChildren::Transition(..) => return,
                TabChildren::Fork { .. } => break,
            };
            if let Some(fork) = forks.pop_front() {
                let mut branches = vec![];
                for branch in fork.branches {
                    let child = Rc::new(RefCell::new(TableauNode2::from_formulae(
                        branch.labels,
                        Some(tab),
                    )));
                    child.borrow_mut().choices.push((fork.id, branch.id));
                    branches.push(TabBranch {
                        id: branch.id,
                        node: child,
                    });
                }
                tab.borrow_mut().children = TabChildren::Fork {
                    id: fork.id,
                    branches,
                };
            } else {
                return;
            }
        }
        let mut feasibility = Feasibility::Contradiction;
        if let TabChildren::Fork { branches, .. } = &tab.borrow().children {
            for branch in branches.iter() {
                if branch.node.borrow().is_closed() {
                    continue;
                }
                self.expand_static(&branch.node, forks.clone(), isroot);
                feasibility = feasibility.better(&branch.node.borrow().feasibility);
            }
        }
        tab.borrow_mut().feasibility = Feasibility::Contradiction;
    }

    fn transit(&mut self, fruit: &Rc<RefCell<TableauNode2>>) -> Option<Transit> {
        let mut labels = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            labels.push(label.clone());
            true
        });
        let modals = Modals::new(
            labels.iter(),
            self.framecond.serial(),
            self.framecond.spotlit(),
        );
        if modals.ge.is_empty() {
            return None;
        }
        let mut transit = match self.framecond {
            FrameCondition::K | FrameCondition::D | FrameCondition::K45 | FrameCondition::D45 => {
                Transit::KOr45(TransitKOr45::from_modals(modals, self))
            }
            FrameCondition::K5 | FrameCondition::D5 => {
                Transit::K5(Transit5::from_modals(modals, self))
            }
            FrameCondition::KB5 | FrameCondition::S5 => {
                match TransitB5::from_modals(modals, fruit, self) {
                    Ok(transit) => Transit::B5(transit),
                    Err(transit) => Transit::KOr45(transit),
                }
            }
        };
        if transit.is_closed() {
            return Some(transit);
        }
        transit.recurse(self);
        if transit.is_closed() {
            return Some(transit);
        }
        transit.solve();
        Some(transit)
    }
}

impl ForkStore {
    fn create_fork(
        &mut self,
        forktype: ForkType,
        branches: Vec<Vec<Rc<Formula>>>,
        conflictset: &Vec<Conflict>,
    ) -> Fork {
        let mut fork = Fork {
            id: self.forks.len(),
            fktype: forktype,
            branches: vec![],
        };
        for (i, branch) in branches.into_iter().enumerate() {
            let mut fork_branch = Branch {
                id: i,
                labels: vec![],
            };
            for formula in branch {
                let label = Label {
                    formula,
                    conflictset: conflictset.clone(),
                };
                fork_branch.labels.push(label);
            }
            fork.branches.push(fork_branch);
        }
        self.forks.push(fork.clone());
        fork
    }
}

impl Transit {
    fn recurse(&mut self, calc: &mut GradedKCalc) {
        match self {
            Transit::KOr45(transit) => calc.transition_rec(&transit.paraws.tab),
            Transit::B5(_) | Transit::K5(_) => {}
        }
    }

    const fn is_closed(&self) -> bool {
        self.feasibility().is_bad()
    }

    pub(crate) const fn feasibility(&self) -> Feasibility {
        match self {
            Transit::KOr45(transit) => transit.feasibility,
            Transit::B5(transit) => transit.feasibility,
            Transit::K5(transit) => transit.feasibility,
        }
    }
}

impl Modals {
    fn new<'a>(labels: impl Iterator<Item = &'a Label>, serial: bool, spotlit: bool) -> Modals {
        let mut this = Modals {
            bx: vec![],
            ge: vec![],
            le: vec![],
        };
        for label in labels {
            this.store(&label);
        }
        if spotlit {
            let dummy = Formula::bottom();
            for f in this
                .ge
                .iter_mut()
                .chain(this.le.iter_mut())
                .map(|(_, l)| l)
                .chain(this.bx.iter_mut())
            {
                f.formula = Depth1F::from(mem::replace(&mut f.formula, dummy.clone())).into();
            }
        }
        if serial && this.ge.is_empty() && (!this.le.is_empty() || !this.bx.is_empty()) {
            this.ge.push((
                1,
                Label {
                    formula: Formula::top(),
                    conflictset: vec![],
                },
            ));
        }
        this
    }

    fn to_forks_constraints(
        &self,
        forkstore: &mut ForkStore,
    ) -> (Option<RangeInclusive<usize>>, Vec<Constraint>) {
        let labels = self.ge.iter().map(|(c, lab)| (c, true, lab));
        let labels = labels.chain(self.le.iter().map(|(c, lab)| (c, false, lab)));
        let mut forks = VecDeque::with_capacity(self.ge.len() + self.le.len());
        let mut constraints = Vec::with_capacity(self.ge.len() + self.le.len());
        for (&value, sense, lab) in labels {
            let fork = forkstore.create_fork(
                ForkType::ParallelWorlds,
                vec![vec![lab.formula.not()], vec![lab.formula.clone()]],
                &lab.conflictset,
            );
            constraints.push(Constraint {
                forkid: fork.id,
                sense,
                value,
                formula: lab.formula.clone(),
            });
            forks.push_back(fork);
        }
        let range = forks.front().zip(forks.back()).map(|(a, b)| a.id..=b.id);
        (range, constraints)
    }

    fn store(&mut self, label: &Label) {
        match label.formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Not(_)
            | Formula::And(..)
            | Formula::Or(..)
            | Formula::Imply(..)
            | Formula::Iff(..) => {}
            Formula::Box(phi) => self.bx.push(Label {
                formula: phi.clone(),
                conflictset: label.conflictset.clone(),
            }),
            Formula::Diamond(phi) => self.ge.push((
                1,
                Label {
                    formula: phi.clone(),
                    conflictset: label.conflictset.clone(),
                },
            )),
            Formula::DiamondGe(count, phi) => self.ge.push((
                *count,
                Label {
                    formula: phi.clone(),
                    conflictset: label.conflictset.clone(),
                },
            )),
            Formula::DiamondLe(count, phi) => self.le.push((
                *count,
                Label {
                    formula: phi.clone(),
                    conflictset: label.conflictset.clone(),
                },
            )),
        }
    }

    fn submodals(&self) -> Vec<Label> {
        let mut submodalfs = vec![];
        let mut out = vec![];
        for label in self
            .ge
            .iter()
            .chain(self.le.iter())
            .map(|(_, l)| l)
            .chain(self.bx.iter())
        {
            label.formula.store_modals(&mut submodalfs);
            for f in submodalfs.drain(..) {
                out.push(Label {
                    formula: f,
                    conflictset: label.conflictset.clone(),
                });
            }
        }
        out
    }

    pub(crate) fn display_constraints(
        &self,
        f: &mut fmt::Formatter<'_>,
        cns: &Vec<Constraint>,
    ) -> fmt::Result {
        for cn in cns {
            if cn.sense {
                writeln!(f, "(≥{}): φ{} := {}", cn.value, cn.forkid, cn.formula)?;
            } else {
                writeln!(f, "(≤{}): φ{} := {}", cn.value, cn.forkid, cn.formula)?;
            }
        }
        for phi in self.bx.iter() {
            writeln!(f, "□: {}", phi.formula)?;
        }
        Ok(())
    }
}

impl ParallelWorlds {
    fn from_modals(modals: &Modals, calc: &mut GradedKCalc) -> (Self, Vec<Constraint>) {
        let (forkranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let this = Self::from_forks(modals.bx.clone(), forkranges.into_iter(), calc);
        (this, constraints)
    }

    fn from_forks<'a>(
        formulae: Vec<Label>,
        forkids: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(formulae, None)));
        let forkid_ranges = Vec::from_iter(forkids);
        let forks = VecDeque::from_iter(
            forkid_ranges
                .iter()
                .flat_map(|r| calc.forks.forks[r.clone()].iter().cloned()),
        );
        calc.expand_static(&tab, forks, false);
        Self {
            tab,
            forkid_ranges,
            choices: vec![],
        }
    }

    pub(crate) fn set_choices(&mut self) {
        let mut fruits = vec![];
        let mut choices = vec![];
        TableauNode2::get_fruits(&self.tab, &mut fruits);
        for fruit in fruits {
            if fruit.borrow().is_closed() {
                continue;
            }
            let mut subchoices = vec![];
            fruit
                .borrow()
                .get_choices(&mut subchoices, &self.forkid_ranges);
            choices.push(subchoices);
        }
        choices.dedup(); // ?
        self.choices = choices;
    }
}

impl TransitKOr45 {
    fn from_modals(modals: Modals, calc: &mut GradedKCalc) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(&modals, calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            paraws,
            constraints,
            modals,
            solution: vec![],
        }
    }
}

impl Transit5 {
    fn from_modals(modals: Modals, calc: &mut GradedKCalc) -> Self {
        let submodals = modals.submodals();
        let mut settings = vec![true; submodals.len()];
        let mut paracliques = vec![];
        let (spotranges, spotconstraints) = modals.to_forks_constraints(&mut calc.forks);
        let mut feasibility = Feasibility::Contradiction;
        // OPT: Get initial spotlight para worlds and break if contradiction
        loop {
            let paraclique = ParaClique::new(
                submodals.iter(),
                &settings,
                modals.bx.iter().cloned(),
                spotranges.iter().cloned(),
                calc,
            );
            feasibility = feasibility.better(&paraclique.feasibility);
            paracliques.push(paraclique);
            if !Self::next_setting(&mut settings) {
                break;
            }
        }
        Self {
            modals,
            submodals,
            spotconstraints,
            paracliques,
            feasibility,
        }
    }

    fn next_setting(settings: &mut Vec<bool>) -> bool {
        for st in settings {
            *st = !*st;
            if !*st {
                return true;
            }
        }
        return false;
    }
}

impl ParaClique {
    fn new<'a>(
        submodals: impl Iterator<Item = &'a Label>,
        signs: &Vec<bool>,
        modalboxes: impl Iterator<Item = Label>,
        ranges: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let settings: Vec<_> = submodals
            .zip(signs.iter())
            .map(|(label, sign)| Label {
                formula: if *sign {
                    label.formula.clone()
                } else {
                    label.formula.not()
                },
                conflictset: label.conflictset.clone(),
            })
            .collect();
        let cliquemodals = Modals::new(settings.iter(), false, false);
        let (cliquews, cliqueconstraints) = ParallelWorlds::from_modals(&cliquemodals, calc);
        let mut spotformulae = settings;
        spotformulae.extend(cliquemodals.bx.iter().cloned());
        spotformulae.extend(modalboxes);
        let spotranges = ranges.chain(cliquews.forkid_ranges.iter().cloned());
        let spotws = ParallelWorlds::from_forks(spotformulae, spotranges, calc);
        let cliquefeas = spotws.tab.borrow().feasibility;
        ParaClique {
            settings: signs.clone(),
            feasibility: cliquefeas,
            spotws,
            cliquews,
            cliqueconstraints,
            spotsolution: vec![],
            cliquesolution: vec![],
        }
    }
}

impl TransitB5 {
    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2>>,
        calc: &mut GradedKCalc,
    ) -> Result<Self, TransitKOr45> {
        let (paraws, constraints) = ParallelWorlds::from_modals(&modals, calc);
        let feasibility = paraws.tab.borrow().feasibility;
        if paraws.tab.borrow().is_closed() {
            return Err(TransitKOr45 {
                paraws,
                constraints,
                modals,
                solution: vec![],
                feasibility,
            });
        }
        Ok(Self {
            reflexion: Self::get_reflexion(&modals, &paraws, leaf, calc),
            paraws,
            constraints,
            modals,
            solution: vec![],
            rfxsolution: 0,
            feasibility,
        })
    }

    fn get_reflexion(
        modals: &Modals,
        paraws: &ParallelWorlds,
        leaf: &Rc<RefCell<TableauNode2>>,
        calc: &mut GradedKCalc,
    ) -> ParallelWorlds {
        let mut formulae = modals.bx.clone();
        leaf.borrow().traverse_anc_formulae(&mut |l| {
            formulae.push(l.clone());
            true
        });
        ParallelWorlds::from_forks(formulae, paraws.forkid_ranges.iter().cloned(), calc)
    }
}

impl Formula {
    fn store_modals(self: &Rc<Formula>, out: &mut Vec<Rc<Formula>>) {
        match self.as_ref() {
            Formula::Bottom | Formula::Top | Formula::PropVar(_, _) => {}
            Formula::Not(phi) => phi.store_modals(out),
            Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::DiamondGe(_, _)
            | Formula::DiamondLe(_, _) => out.push(self.clone()),
            Formula::And(phi0, phi1)
            | Formula::Or(phi0, phi1)
            | Formula::Imply(phi0, phi1)
            | Formula::Iff(phi0, phi1) => {
                phi0.store_modals(out);
                phi1.store_modals(out);
            }
        }
    }
}

impl PropLinear {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::DiamondGe(..)
            | Formula::DiamondLe(..)
            | Formula::Or(_, _)
            | Formula::Imply(_, _)
            | Formula::Iff(_, _) => vec![],
            Formula::And(phi1, phi2) => vec![phi1.clone(), phi2.clone()],
            Formula::Not(phi) => match phi.as_ref() {
                Formula::Bottom
                | Formula::PropVar(..)
                | Formula::And(_, _)
                | Formula::Iff(_, _) => vec![],
                Formula::Top => vec![Formula::bottom()],
                Formula::Not(psi) => vec![psi.clone()],
                Formula::Box(psi) => vec![psi.not().diamond()],
                Formula::Diamond(psi) => vec![psi.not().box_()],
                Formula::DiamondGe(count, psi) => vec![psi.dmle(count - 1)],
                Formula::DiamondLe(count, psi) => vec![psi.dmge(count + 1)],
                Formula::Or(psi1, psi2) => vec![psi1.not(), psi2.not()],
                Formula::Imply(psi1, psi2) => vec![psi1.clone(), psi2.not()],
            },
        }
    }
}

impl PropFork {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::DiamondGe(..)
            | Formula::DiamondLe(..)
            | Formula::And(_, _) => vec![],
            Formula::Not(phi) => match phi.as_ref() {
                Formula::Bottom
                | Formula::Top
                | Formula::PropVar(..)
                | Formula::Not(_)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(..)
                | Formula::DiamondLe(..)
                | Formula::Or(_, _)
                | Formula::Imply(_, _) => vec![],
                Formula::And(psi1, psi2) => vec![vec![psi1.not()], vec![psi2.not()]],
                Formula::Iff(psi1, psi2) => vec![
                    vec![psi1.clone(), psi2.not()],
                    vec![psi1.not(), psi2.clone()],
                ],
            },
            Formula::Or(phi1, phi2) => vec![vec![phi1.clone()], vec![phi2.clone()]],
            Formula::Imply(phi1, phi2) => vec![vec![phi1.not()], vec![phi2.clone()]],
            Formula::Iff(phi1, phi2) => vec![
                vec![phi1.clone(), phi2.clone()],
                vec![phi1.not(), phi2.not()],
            ],
        }
    }
}

impl TLinear {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![phi.clone()],
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Diamond(_)
            | Formula::DiamondGe(..)
            | Formula::DiamondLe(..)
            | Formula::Not(_)
            | Formula::Or(_, _)
            | Formula::And(..)
            | Formula::Imply(_, _)
            | Formula::Iff(_, _) => vec![],
        }
    }
}

impl Feasibility {
    pub(crate) const fn symbol(&self) -> &'static str {
        match self {
            Feasibility::Feasible => "✓",
            Feasibility::Contradiction => "⊥",
            Feasibility::NoSolution => "∅",
            Feasibility::Infeasible => "⨉",
        }
    }

    pub(crate) const fn is_bad(&self) -> bool {
        match self {
            Feasibility::Contradiction | Feasibility::Infeasible | Feasibility::NoSolution => true,
            Feasibility::Feasible => false,
        }
    }

    pub(crate) const fn better(&self, other: &Self) -> Self {
        match (self, other) {
            (_, Feasibility::Feasible) | (Feasibility::Feasible, _) => Feasibility::Feasible,
            (_, Feasibility::Infeasible) | (Feasibility::Infeasible, _) => Feasibility::Infeasible,
            (_, Feasibility::NoSolution) | (Feasibility::NoSolution, _) => Feasibility::NoSolution,
            _ => Feasibility::Contradiction,
        }
    }
}
