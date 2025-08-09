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

#[derive(Clone, Debug)]
pub(crate) struct Grading {
    pub(crate) forkid: usize,
    pub(crate) sense: bool,
    pub(crate) value: u32,
    pub(crate) formula: Rc<Formula>,
}

pub(crate) struct Constraints {
    pub(crate) gradings: Vec<Grading>,
    pub(crate) boxsubforms: Vec<LabeledFormula>,
}

pub(crate) struct ParallelWorlds<T> {
    pub(crate) tab: Rc<RefCell<TableauNode2<T>>>,
    pub(crate) forkids: Vec<RangeInclusive<usize>>,
    pub(crate) choices: Vec<Vec<(usize, usize)>>,
}

pub(crate) struct TransitKOr45 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitT {
    pub(crate) reflexion: bool,
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) ranges: Vec<RangeInclusive<usize>>,
    pub(crate) vars: Vec<Variable>,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitB5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) reflexion: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
    pub(crate) rfxsolution: usize,
}

pub(crate) struct Transit5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) submodals: Vec<LabeledFormula>,
    pub(crate) spotconstraints: Constraints,
    pub(crate) paracliques: Vec<ParaClique<Self>>,
}

pub(crate) struct ParaClique<T> {
    pub(crate) feasibility: Feasibility,
    pub(crate) settings: Vec<bool>,
    pub(crate) spotws: ParallelWorlds<T>,
    pub(crate) cliquews: ParallelWorlds<T>,
    pub(crate) cliqueconstraints: Constraints,
    pub(crate) spotsolution: Vec<u32>,
    pub(crate) cliquesolution: Vec<u32>,
}

pub(crate) struct Transit4 {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
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
        self,
        forkstore: &mut ForkStore,
    ) -> (Option<RangeInclusive<usize>>, Constraints) {
        let mut forks = VecDeque::with_capacity(self.ge.len() + self.le.len());
        let mut constraints = Vec::with_capacity(self.ge.len() + self.le.len());
        let labels = self.ge.into_iter().map(|(c, lab)| (c, true, lab));
        let labels = labels.chain(self.le.into_iter().map(|(c, lab)| (c, false, lab)));
        for (value, sense, lab) in labels {
            let fork = forkstore.create_fork(
                ForkType::ParallelWorlds,
                vec![vec![lab.formula.not()], vec![lab.formula.clone()]],
                &lab.conflictset,
            );
            constraints.push(Grading {
                forkid: fork.id,
                sense,
                value,
                formula: lab.formula,
            });
            forks.push_back(fork);
        }
        let range = forks.front().zip(forks.back()).map(|(a, b)| a.id..=b.id);
        (
            range,
            Constraints {
                gradings: constraints,
                boxsubforms: self.bx,
            },
        )
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

impl fmt::Display for Constraints {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for cn in &self.gradings {
            if cn.sense {
                writeln!(f, "(≥{}): φ{} := {}", cn.value, cn.forkid, cn.formula)?;
            } else {
                writeln!(f, "(≤{}): φ{} := {}", cn.value, cn.forkid, cn.formula)?;
            }
        }
        for phi in &self.boxsubforms {
            writeln!(f, "□: {}", phi.formula)?;
        }
        Ok(())
    }
}

impl<T: BaseTransit> ParallelWorlds<T> {
    fn from_modals(
        modals: Modals,
        parent: Option<&Rc<RefCell<TableauNode2<T>>>>,
        calc: &mut GradedKCalc,
    ) -> (Self, Constraints) {
        let box_subform = modals.bx.clone();
        let (forkranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let this = Self::from_forks(box_subform, forkranges.into_iter().collect(), parent, calc);
        (this, constraints)
    }

    fn from_forks(
        formulae: Vec<LabeledFormula>,
        forkids: Vec<RangeInclusive<usize>>,
        parent: Option<&Rc<RefCell<TableauNode2<T>>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(formulae, parent)));
        let forks = VecDeque::from_iter(
            forkids
                .iter()
                .flat_map(|r| calc.forks.forks[r.clone()].iter().cloned()),
        );
        calc.expand_static(&tab, forks, false);
        Self {
            tab,
            forkids,
            choices: vec![],
        }
    }

    fn add_forks(
        &mut self,
        forkids: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut GradedKCalc,
    ) {
        let mut forks = VecDeque::new();
        for range in forkids {
            forks.extend(calc.forks.forks[range.clone()].iter().cloned());
            self.forkids.push(range);
        }
        calc.expand_static(&self.tab, forks, false);
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
            fruit.borrow().get_choices(&mut subchoices, &self.forkids);
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

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut GradedKCalc) -> Option<Self>;

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
        calc.transition(&self.paraws.tab)
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut GradedKCalc) -> Option<Self> {
        calc.first_transit(fruit)
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

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            paraws,
            constraints,
            solution: vec![],
        }
    }
}

impl BaseTransit for TransitT {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut GradedKCalc) -> Option<Self> {
        Self::reflect(fruit, vec![], None.into_iter(), calc)
    }

    fn recurse(&mut self, calc: &mut GradedKCalc) {
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
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        for lab in &modals.bx {
            println!("B: {} L: {}", lab.formula, lab.lemma);
        }
        let bxsubforms = modals.bx.clone();
        let (ranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let ranges = ranges.map_or(vec![], |r| vec![r]);
        println!("R: {:?}", ranges);
        let paraws = ParallelWorlds::from_forks(bxsubforms, ranges.clone(), Some(leaf), calc);
        for lab in &paraws.tab.borrow().formulae {
            println!("M: {} L: {}", lab.formula, lab.lemma);
        }
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            reflexion: true,
            feasibility,
            paraws,
            constraints,
            ranges,
            vars: vec![],
            solution: vec![],
        }
    }
}

impl TransitT {
    fn from_reflection(
        modals: Modals,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        mut labels: Vec<LabeledFormula>,
        mut ranges: Vec<RangeInclusive<usize>>,
        src_constraints: impl Iterator<Item = Grading>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (forkids, mut constraints) = modals.to_forks_constraints(&mut calc.forks);
        for lab in labels.iter_mut() {
            lab.lemma = true;
        }
        for lab in &labels {
            println!("L: {} L: {}", lab.formula, lab.lemma);
        }
        println!("R: {:?}", forkids);
        let paraws = ParallelWorlds::<Self>::from_forks(
            labels,
            forkids.clone().map_or(vec![], |f| vec![f]),
            Some(fruit),
            calc,
        );
        ranges.extend(forkids.into_iter());
        constraints.gradings.extend(src_constraints);
        let feasibility = paraws.tab.borrow().feasibility;
        for lab in &paraws.tab.borrow().formulae {
            println!("F: {} L: {}", lab.formula, lab.lemma);
        }
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

    pub(crate) fn reflect(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        ranges: Vec<RangeInclusive<usize>>,
        constraints: impl Iterator<Item = Grading>,
        calc: &mut GradedKCalc,
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
        calc: &mut GradedKCalc,
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

impl BaseTransit for TransitB5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, _calc: &mut GradedKCalc) {}

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut GradedKCalc) -> Option<Self> {
        calc.first_transit(fruit)
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let boxsubforms = modals.bx.clone();
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        let reflexion = if paraws.tab.borrow().is_closed() {
            ParallelWorlds::from_forks(vec![], vec![], Some(leaf), calc)
        } else {
            Self::get_reflexion(boxsubforms, &paraws, leaf, calc)
        };
        Self {
            feasibility,
            paraws,
            constraints,
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
        match self.feasibility {
            Feasibility::Contradiction | Feasibility::NoSolution => {
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
        mut boxsubforms: Vec<LabeledFormula>,
        paraws: &ParallelWorlds<Self>,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> ParallelWorlds<Self> {
        leaf.borrow().traverse_anc_formulae(&mut |l| {
            boxsubforms.push(l.clone());
            true
        });
        ParallelWorlds::from_forks(boxsubforms, paraws.forkids.clone(), Some(leaf), calc)
    }
}

impl BaseTransit for Transit5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn recurse(&mut self, _calc: &mut GradedKCalc) {}

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut GradedKCalc) -> Option<Self> {
        calc.first_transit(fruit)
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
        writeln!(f, "{}", self.spotconstraints)?;
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
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let boxsubforms = modals.bx.clone();
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
                boxsubforms.iter().cloned(),
                Vec::from_iter(spotranges.iter().cloned()),
                leaf,
                calc,
            );
            feasibility = feasibility.better(&paraclique.feasibility);
            paracliques.push(paraclique);
            if !Self::next_setting(&mut settings) {
                break;
            }
        }
        Self {
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
        mut spotranges: Vec<RangeInclusive<usize>>,
        leaf: &Rc<RefCell<TableauNode2<T>>>,
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
        let mut spotformulae = settings;
        spotformulae.extend(cliquemodals.bx.iter().cloned());
        spotformulae.extend(modalboxes);
        let (cliquews, cliqueconstraints) =
            ParallelWorlds::from_modals(cliquemodals, Some(leaf), calc);
        spotranges.extend_from_slice(&cliquews.forkids);
        let spotws = ParallelWorlds::from_forks(spotformulae, spotranges, Some(leaf), calc);
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

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut GradedKCalc) -> Option<Self> {
        calc.first_transit(fruit)
    }

    fn recurse(&mut self, calc: &mut GradedKCalc) {
        if self.feasibility.is_bad() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        for flower in flowers {
            let subtransit = self.diffract(&flower, calc);
            if let Some(subtransit) = subtransit {
                flower.borrow_mut().feasibility = subtransit.feasibility;
                flower.borrow_mut().children = TabChildren::Transition(subtransit);
            }
        }
        self.feasibility = TableauNode2::set_feasibility_rec(&self.paraws.tab);
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

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (paraws, constraints) = ParallelWorlds::from_modals(modals, Some(leaf), calc);
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            ranges: paraws.forkids.clone(),
            paraws,
            constraints,
            vars: vec![],
            solution: vec![],
        }
    }
}

impl Transit4 {
    fn from_diffraction(
        modals: Modals,
        mut ranges: Vec<RangeInclusive<usize>>,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let (forkids, constraints) = modals.to_forks_constraints(&mut calc.forks);
        ranges.extend(forkids.into_iter());
        let paraws = ParallelWorlds::from_forks(
            constraints.boxsubforms.clone(),
            ranges.clone(),
            Some(fruit),
            calc,
        );
        let feasibility = paraws.tab.borrow().feasibility;
        Self {
            feasibility,
            paraws,
            constraints,
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
        labels.extend(self.constraints.boxsubforms.iter().cloned());
        let modals = Modals::new(labels.iter(), calc.framecond.ray(), false);
        // sleep(Duration::from_secs(3));
        if modals.ge.is_empty() {
            return None;
        }
        let mut subtransit = Self::from_diffraction(modals, self.ranges.clone(), fruit, calc);
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
