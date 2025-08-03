use std::{cell::RefCell, collections::VecDeque, fmt, mem, ops::RangeInclusive, rc::Rc};

use good_lp::Variable;

use crate::{
    flatformula::FlatFormula,
    formula::Formula,
    rules3::{Feasibility, ForkStore, ForkType, GradedKCalc},
    tableau2::{LabeledFormula, TabChildren, TableauNode2},
};

#[derive(Debug)]
pub(crate) struct Modals {
    pub(crate) bx: Vec<LabeledFormula>,
    pub(crate) ge: Vec<(u32, LabeledFormula)>,
    pub(crate) le: Vec<(u32, LabeledFormula)>,
}

#[derive(Debug)]
pub(crate) struct Constraint {
    pub(crate) forkid: usize,
    pub(crate) sense: bool,
    pub(crate) value: u32,
    pub(crate) formula: Rc<Formula>,
}

pub(crate) struct ParallelWorlds<T> {
    pub(crate) tab: Rc<RefCell<TableauNode2<T>>>,
    pub(crate) forkid_ranges: Vec<RangeInclusive<usize>>,
    pub(crate) choices: Vec<Vec<(usize, usize)>>,
}

pub(crate) struct TransitKOr45 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitT {
    pub(crate) reflexion: bool,
    pub(crate) feasibility: Feasibility,
    // TODO: Change constraints to include boxed formulae
    pub(crate) box_subformulae: Vec<Rc<Formula>>,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) ranges: Vec<RangeInclusive<usize>>,
    pub(crate) vars: Vec<Variable>,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitB5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) reflexion: ParallelWorlds<Self>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) solution: Vec<u32>,
    pub(crate) rfxsolution: usize,
}

pub(crate) struct Transit5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) modals: Modals,
    pub(crate) submodals: Vec<LabeledFormula>,
    pub(crate) spotconstraints: Vec<Constraint>,
    pub(crate) paracliques: Vec<ParaClique<Self>>,
}

pub(crate) struct ParaClique<T> {
    pub(crate) feasibility: Feasibility,
    pub(crate) settings: Vec<bool>,
    pub(crate) spotws: ParallelWorlds<T>,
    pub(crate) cliquews: ParallelWorlds<T>,
    pub(crate) cliqueconstraints: Vec<Constraint>,
    pub(crate) spotsolution: Vec<u32>,
    pub(crate) cliquesolution: Vec<u32>,
}

pub(crate) struct Transit4 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) ranges: Vec<RangeInclusive<usize>>,
    pub(crate) vars: Vec<Variable>,
    pub(crate) solution: Vec<u32>,
}

impl Modals {
    pub(crate) fn new<'a>(
        labels: impl Iterator<Item = &'a LabeledFormula>,
        serial: bool,
        spotlit: bool,
    ) -> Modals {
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
                f.formula = FlatFormula::from(mem::replace(&mut f.formula, dummy.clone())).into();
            }
        }
        if serial && this.ge.is_empty() && (!this.le.is_empty() || !this.bx.is_empty()) {
            this.ge.push((
                1,
                LabeledFormula {
                    formula: Formula::top(),
                    conflictset: vec![],
                    lemma: false,
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

    fn store(&mut self, label: &LabeledFormula) {
        match label.formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Not(_)
            | Formula::And(..)
            | Formula::Or(..)
            | Formula::Imply(..)
            | Formula::Iff(..) => {}
            Formula::Box(phi) => self.bx.push(LabeledFormula {
                formula: phi.clone(),
                conflictset: label.conflictset.clone(),
                lemma: false,
            }),
            Formula::Diamond(phi) => self.ge.push((
                1,
                LabeledFormula {
                    formula: phi.clone(),
                    conflictset: label.conflictset.clone(),
                    lemma: false,
                },
            )),
            Formula::DiamondGe(count, phi) => self.ge.push((
                *count,
                LabeledFormula {
                    formula: phi.clone(),
                    conflictset: label.conflictset.clone(),
                    lemma: false,
                },
            )),
            Formula::DiamondLe(count, phi) => self.le.push((
                *count,
                LabeledFormula {
                    formula: phi.clone(),
                    conflictset: label.conflictset.clone(),
                    lemma: false,
                },
            )),
        }
    }

    fn submodals(&self) -> Vec<LabeledFormula> {
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
                out.push(LabeledFormula {
                    formula: f,
                    conflictset: label.conflictset.clone(),
                    lemma: false,
                });
            }
        }
        out
    }
}

impl Formula {
    pub(crate) fn store_modals(self: &Rc<Formula>, out: &mut Vec<Rc<Formula>>) {
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

impl Constraint {
    pub(crate) fn display<'a>(
        f: &mut fmt::Formatter<'_>,
        bxs: impl Iterator<Item = &'a Rc<Formula>>,
        cns: &Vec<Self>,
    ) -> fmt::Result {
        for cn in cns {
            if cn.sense {
                writeln!(f, "(≥{}): φ{} := {}", cn.value, cn.forkid, cn.formula)?;
            } else {
                writeln!(f, "(≤{}): φ{} := {}", cn.value, cn.forkid, cn.formula)?;
            }
        }
        for phi in bxs {
            writeln!(f, "□: {phi}")?;
        }
        Ok(())
    }
}

impl<T: BaseTransit> ParallelWorlds<T> {
    fn from_modals(modals: &Modals, calc: &mut GradedKCalc) -> (Self, Vec<Constraint>) {
        let (forkranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let this = Self::from_forks(modals.bx.clone(), forkranges.into_iter(), calc);
        (this, constraints)
    }

    fn from_forks<'a>(
        formulae: Vec<LabeledFormula>,
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

    pub(crate) fn set_choices(&mut self, dedup: bool) {
        if !self.choices.is_empty() {
            return;
        }
        let mut fruits = vec![];
        TableauNode2::get_fruits(&self.tab, &mut fruits);
        for fruit in fruits {
            if fruit.borrow().is_closed() {
                continue;
            }
            let mut subchoices = vec![];
            fruit
                .borrow()
                .get_choices(&mut subchoices, &self.forkid_ranges);
            self.choices.push(subchoices);
        }
        if dedup {
            self.choices.dedup(); // ?
        }
    }
}
pub(crate) trait Transit: BaseTransit {
    fn solve(&mut self);
}

pub(crate) trait BaseTransit: Sized {
    fn feasibility(&self) -> Feasibility;

    fn is_closed(&self) -> bool {
        self.feasibility().is_bad()
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self;

    fn recurse(&mut self, calc: &mut GradedKCalc);

    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result;
}

impl BaseTransit for TransitKOr45 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, calc: &mut GradedKCalc) {
        calc.first_transition(&self.paraws.tab)
    }

    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        Constraint::display(f, self.modals.bx.iter().map(|lab| &lab.formula), &self.constraints)?;
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

    fn from_modals(
        modals: Modals,
        _leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
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

impl BaseTransit for TransitT {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, calc: &mut GradedKCalc) {
        if self.feasibility.is_bad() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        let mut feasibility = Feasibility::Infeasible;
        for flower in flowers {
            if let Some(subtransit) = self.reflect(&flower, calc) {
                if let Feasibility::Feasible = subtransit.feasibility {
                    feasibility = Feasibility::Feasible;
                }
                flower.borrow_mut().feasibility = subtransit.feasibility;
                flower.borrow_mut().children = TabChildren::Transition(subtransit);
            } else {
                feasibility = Feasibility::Feasible;
            }
        }
        // TODO: Set entire tree of feasibility
        self.paraws.tab.borrow_mut().feasibility = feasibility;
        self.feasibility = feasibility;
    }

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
        Constraint::display(f, self.box_subformulae.iter(), &self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        todo!();
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

    fn from_modals(
        modals: Modals,
        _leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (ranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let paraws = ParallelWorlds::from_forks(modals.bx.clone(), ranges.iter().cloned(), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            reflexion: true,
            feasibility,
            box_subformulae: modals.bx.into_iter().map(|lab| lab.formula).collect(),
            paraws,
            constraints,
            ranges: ranges.map_or(vec![], |r| vec![r]),
            vars: vec![],
            solution: vec![],
        }
    }
}

impl TransitT {
    fn from_reflection(
        modals: Modals,
        mut labels: Vec<LabeledFormula>,
        srcranges: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (forkids, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let ranges: Vec<_> = srcranges.chain(forkids.into_iter()).collect();
        for lab in labels.iter_mut() {
            lab.lemma = true;
        }
        let paraws = ParallelWorlds::<Self>::from_forks(labels, ranges.iter().cloned(), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            reflexion: true,
            feasibility,
            paraws,
            constraints,
            box_subformulae: modals.bx.into_iter().map(|lab| lab.formula).collect(),
            vars: vec![],
            solution: vec![],
            ranges,
        }
    }

    pub(crate) fn reflect(
        &self,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Option<Self> {
        let mut labels = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            labels.push(label.clone());
            true
        });
        let modals = Modals::new(labels.iter().filter(|lab| !lab.lemma), false, false);
        if modals.ge.is_empty() && modals.le.is_empty() {
            return Self::transition(labels.iter(), self.ranges.clone(), calc);
        }
        let mut transit = Self::from_reflection(modals, labels, self.ranges.iter().cloned(), calc);
        if transit.is_closed() {
            return Some(transit);
        }
        transit.recurse(calc);
        if transit.is_closed() {
            return Some(transit);
        }
        transit.solve();
        Some(transit)
    }

    fn transition<'a>(
        labels: impl Iterator<Item = &'a LabeledFormula>,
        ranges: Vec<RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) -> Option<Self> {
        if ranges.is_empty() {
            return None;
        }
        let formulae: Vec<_> = labels
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
        let box_subformulae = formulae.iter().map(|lab| lab.formula.clone()).collect();
        let paraws = ParallelWorlds::<Self>::from_forks(formulae, ranges.iter().cloned(), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        let mut subtransit = Self {
            reflexion: false,
            feasibility,
            paraws,
            constraints: vec![],
            box_subformulae,
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
        subtransit.solve();
        Some(subtransit)
    }
}

impl BaseTransit for TransitB5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, _calc: &mut GradedKCalc) {}

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(&modals, calc);
        let feasibility = paraws.tab.borrow().feasibility;
        let reflexion = if paraws.tab.borrow().is_closed() {
            ParallelWorlds::from_forks(vec![], [].into_iter(), calc)
        } else {
            Self::get_reflexion(&modals, &paraws, leaf, calc)
        };
        Self {
            feasibility,
            paraws,
            constraints,
            modals,
            solution: vec![],
            rfxsolution: 0,
            reflexion,
        }
    }

    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        Constraint::display(f, self.modals.bx.iter().map(|lab| &lab.formula), &self.constraints)?;
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
        match self.feasibility {
            Feasibility::Contradiction | Feasibility::Infeasible | Feasibility::NoSolution => {
                writeln!(f, "No solution")?
            }
            Feasibility::Feasible => {
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
        }
        writeln!(f)
    }
}

impl TransitB5 {
    fn get_reflexion(
        modals: &Modals,
        paraws: &ParallelWorlds<Self>,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> ParallelWorlds<Self> {
        let mut formulae = modals.bx.clone();
        leaf.borrow().traverse_anc_formulae(&mut |l| {
            formulae.push(l.clone());
            true
        });
        ParallelWorlds::from_forks(formulae, paraws.forkid_ranges.iter().cloned(), calc)
    }
}

impl BaseTransit for Transit5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, _calc: &mut GradedKCalc) {}

    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        Constraint::display(f, self.modals.bx.iter().map(|lab| &lab.formula), &self.spotconstraints)?;
        writeln!(f)?;
        writeln!(f, "Second Transition Modals:")?;
        for (i, submodal) in self.submodals.iter().enumerate() {
            writeln!(f, "ψ{i} := {}", submodal.formula)?;
        }
        writeln!(f)?;
        for (i, paracliq) in self.paracliques.iter().enumerate() {
            writeln!(f, "Clique {i}:")?;
            TableauNode2::display_root(&paracliq.spotws.tab, f, curri, roots)?;
            writeln!(f)?;
            TableauNode2::display_root(&paracliq.cliquews.tab, f, curri, roots)?;
            for (i, choice) in paracliq.spotws.choices.iter().enumerate() {
                write!(f, "u{i}: ")?;
                for (forkid, branchid) in choice {
                    write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
                }
                writeln!(f)?;
            }
            for (i, choice) in paracliq.cliquews.choices.iter().enumerate() {
                write!(f, "w{i}: ")?;
                for (forkid, branchid) in choice {
                    write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
                }
                writeln!(f)?;
            }
            if paracliq.spotsolution.is_empty() {
                writeln!(f, "No solution")?;
            } else {
                write!(f, "Solution: ")?;
                for (i, val) in paracliq.spotsolution.iter().enumerate() {
                    write!(f, "{val}*u{i} ")?;
                }
                writeln!(f)?;
                for (i, val) in paracliq.cliquesolution.iter().enumerate() {
                    write!(f, "{val}*w{i} ")?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }

    fn from_modals(
        modals: Modals,
        _leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
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
}

impl<T: Transit> ParaClique<T> {
    fn new<'a>(
        submodals: impl Iterator<Item = &'a LabeledFormula>,
        signs: &Vec<bool>,
        modalboxes: impl Iterator<Item = LabeledFormula>,
        ranges: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let settings: Vec<_> = submodals
            .zip(signs.iter())
            .map(|(label, sign)| LabeledFormula {
                formula: if *sign {
                    label.formula.clone()
                } else {
                    label.formula.not()
                },
                conflictset: label.conflictset.clone(),
                lemma: false,
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

impl Transit5 {
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

impl BaseTransit for Transit4 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, calc: &mut GradedKCalc) {
        if self.feasibility.is_bad() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        let mut feasibility = Feasibility::Infeasible;
        for flower in flowers {
            let subtransit = self.diffract(&flower, calc);
            match subtransit {
                Some(subtransit) => {
                    if let Feasibility::Feasible = subtransit.feasibility {
                        feasibility = Feasibility::Feasible;
                    }
                    flower.borrow_mut().feasibility = subtransit.feasibility;
                    flower.borrow_mut().children = TabChildren::Transition(subtransit);
                }
                None => feasibility = Feasibility::Feasible,
            }
        }
        // TODO: Set entire tree of feasibility
        self.feasibility = feasibility;
        self.paraws.tab.borrow_mut().feasibility = feasibility;
    }

    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        Constraint::display(f, self.modals.bx.iter().map(|lab| &lab.formula), &self.constraints)?;
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

    fn from_modals(
        modals: Modals,
        _leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(&modals, calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            ranges: paraws.forkid_ranges.clone(),
            paraws,
            constraints,
            modals,
            vars: vec![],
            solution: vec![],
        }
    }
}

impl Transit4 {
    fn from_diffraction(
        modals: Modals,
        srcranges: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (forkids, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let ranges: Vec<_> = srcranges.chain(forkids.into_iter()).collect();
        let paraws = ParallelWorlds::from_forks(modals.bx.clone(), ranges.iter().cloned(), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            paraws,
            constraints,
            modals,
            vars: vec![],
            solution: vec![],
            ranges,
        }
    }

    pub(crate) fn diffract(
        &self,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Option<Transit4> {
        let mut labels = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            labels.push(label.clone());
            true
        });
        labels.extend(self.modals.bx.iter().cloned());
        let modals = Modals::new(labels.iter(), calc.framecond.ray(), false);
        // sleep(Duration::from_secs(3));
        if modals.ge.is_empty() {
            return None;
        }
        let mut subtransit = Self::from_diffraction(modals, self.ranges.iter().cloned(), calc);
        if subtransit.feasibility.is_bad() {
            return Some(subtransit);
        }
        subtransit.recurse(calc);
        if subtransit.feasibility.is_bad() {
            return Some(subtransit);
        }
        subtransit.check();
        Some(subtransit)
    }
}
