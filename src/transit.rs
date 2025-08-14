use std::{cell::RefCell, collections::VecDeque, fmt, mem, ops::RangeInclusive, rc::Rc};

use good_lp::Variable;

use crate::{
    flatformula::FlatFormula,
    formula::Formula,
    rules3::{Calculus, Feasibility, ForkStore, ForkType},
    tableau2::{Conflict, LabeledFormula, TabChildren, TableauNode2},
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

#[derive(Clone)]
pub(crate) struct Constraints {
    pub(crate) gradings: Vec<Grading>,
    pub(crate) boxsubforms: Vec<LabeledFormula>,
}

pub(crate) struct ParallelWorlds<T> {
    pub(crate) tab: Rc<RefCell<TableauNode2<T>>>,
    pub(crate) forkids: Vec<RangeInclusive<usize>>,
    pub(crate) choices: Vec<Vec<(usize, usize)>>,
}

pub(crate) trait BaseTransit: Sized {
    fn feasibility(&self) -> Feasibility;

    fn is_closed(&self) -> bool {
        self.feasibility().is_bad()
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self>;
}

pub(crate) trait SolveTransit: BaseTransit {
    fn solve(&mut self);

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Self;

    fn recurse(&mut self, calc: &mut Calculus);
}

pub(crate) trait DisplayTransit: Sized {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result;
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

pub(crate) struct TransitB {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitTB {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: Vec<ParallelWorlds<TransitTB>>,
    pub(crate) constraints: Vec<Vec<Grading>>,
    pub(crate) solutions: Vec<TBSolution>,
}

// pub(crate) struct MetaTransitTB {
//     pub(crate) feasibility: Feasibility,
//     pub(crate) paraws: Vec<ParallelWorlds<TransitTB>>,
//     pub(crate) constraints: Vec<Vec<Grading>>,
//     pub(crate) solutions: Vec<>,
// }

pub(crate) enum TBSolution {
    None,
    Forward(Vec<u32>),
    Backward(Vec<(Vec<u32>, usize, Option<usize>)>),
}

pub(crate) fn general_transit<T: BaseTransit + SolveTransit>(
    calc: &mut Calculus,
    fruit: &Rc<RefCell<TableauNode2<T>>>,
) -> Option<T> {
    let mut labels = vec![];
    fruit.borrow().traverse_anc_formulae(&mut |label| {
        labels.push(label.clone());
        true
    });
    let modals = Modals::new(
        labels.iter(),
        calc.framecond.ray(),
        calc.framecond.spotlit(),
    );
    if modals.ge.is_empty() {
        return None;
    }
    let mut transit = T::from_modals(modals, fruit, calc);
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
            let _ = this.store(&label);
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

    pub(crate) fn to_forks_constraints(
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

    fn store<'a>(&'a mut self, formula: &LabeledFormula) -> Option<&'a LabeledFormula> {
        match formula.formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Not(_)
            | Formula::And(..)
            | Formula::Or(..)
            | Formula::Imply(..)
            | Formula::Iff(..) => None,
            Formula::Box(phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                };
                self.bx.push(subform);
                self.bx.last()
            }
            Formula::Diamond(phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                };
                self.ge.push((1, subform));
                self.ge.last().map(|(_, f)| f)
            }
            Formula::DiamondGe(count, phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                };
                self.ge.push((*count, subform));
                self.ge.last().map(|(_, f)| f)
            }
            Formula::DiamondLe(count, phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                };
                self.le.push((*count, subform));
                self.le.last().map(|(_, f)| f)
            }
        }
    }

    pub(crate) fn submodals(&self) -> Vec<LabeledFormula> {
        let mut out = vec![];
        for label in self
            .ge
            .iter()
            .chain(self.le.iter())
            .map(|(_, l)| l)
            .chain(self.bx.iter())
        {
            label.store_modals(&mut |f| out.push(f));
        }
        out
    }

    pub(crate) fn to_deep_forks_constraints(
        self,
        forkstore: &mut ForkStore,
    ) -> (Vec<Vec<RangeInclusive<usize>>>, Vec<Constraints>) {
        let mut deep_modals = vec![self];
        while let Some(modals) = deep_modals.last()
            && !modals.ge.is_empty()
            && !modals.le.is_empty()
        {
            let mut newformulae = vec![];
            for formula in modals
                .ge
                .iter()
                .chain(modals.le.iter())
                .map(|(_, l)| l)
                .chain(modals.bx.iter())
            {
                formula.store_modals(&mut |f| newformulae.push(f));
            }
            deep_modals.push(Modals::new(newformulae.iter(), false, false));
        }
        let mut forks: Vec<Vec<_>> = vec![];
        let mut constraintsets = vec![];
        for modals in deep_modals {
            let (fork, constraints) = modals.to_forks_constraints(forkstore);
            forks.push(fork.into_iter().collect());
            constraintsets.push(constraints);
        }
        for i in (0..forks.len() - 1).rev() {
            let (left_slice, right_slice) = forks.split_at_mut(i + 1);
            let left = &mut left_slice[i];
            let right = &right_slice[0];
            left.extend(right.iter().cloned());
        }
        for i in (0..forks.len() - 1).rev() {
            let (left_slice, right_slice) = constraintsets.split_at_mut(i + 1);
            let left = &mut left_slice[i];
            let right = &right_slice[0];
            left.boxsubforms.extend(right.boxsubforms.iter().cloned());
            left.gradings.extend(right.gradings.iter().cloned());
        }
        (forks, constraintsets)
    }
}

impl LabeledFormula {
    pub(crate) fn store_modals(&self, store: &mut impl FnMut(Self)) {
        self.formula.store_modals(&mut |f| {
            store(LabeledFormula {
                formula: f.clone(),
                conflictset: self.conflictset.clone(),
                lemma: false,
            })
        });
    }
}

impl Formula {
    fn store_modals(self: &Rc<Formula>, store: &mut impl FnMut(&Rc<Formula>)) {
        match self.as_ref() {
            Formula::Bottom | Formula::Top | Formula::PropVar(_, _) => {}
            Formula::Not(phi) => phi.store_modals(store),
            Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::DiamondGe(_, _)
            | Formula::DiamondLe(_, _) => store(self),
            Formula::And(phi0, phi1)
            | Formula::Or(phi0, phi1)
            | Formula::Imply(phi0, phi1)
            | Formula::Iff(phi0, phi1) => {
                phi0.store_modals(store);
                phi1.store_modals(store);
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
    pub(crate) fn from_modals(
        modals: Modals,
        parent: Option<&Rc<RefCell<TableauNode2<T>>>>,
        calc: &mut Calculus,
    ) -> (Self, Constraints) {
        let (forkranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let this = Self::from_forks(
            constraints.boxsubforms.clone(),
            forkranges.into_iter().collect(),
            parent,
            calc,
        );
        (this, constraints)
    }

    pub(crate) fn from_forks(
        formulae: Vec<LabeledFormula>,
        forkids: Vec<RangeInclusive<usize>>,
        parent: Option<&Rc<RefCell<TableauNode2<T>>>>,
        calc: &mut Calculus,
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
        calc: &mut Calculus,
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

impl BaseTransit for TransitTB {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<TransitTB>>>, calc: &mut Calculus) -> Option<Self> {
        let mut formulae = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |label| {
            formulae.push(label.clone());
            true
        });
        let modals = Modals::new(formulae.iter(), false, false);
        if modals.ge.is_empty() {
            return None;
        }
        let (forks, constraintsets) = modals.to_deep_forks_constraints(&mut calc.forks);
        for formula in &mut formulae {
            formula.lemma = true;
        }
        let reflexion = ParallelWorlds::from_forks(formulae, forks[0].clone(), Some(fruit), calc);
        let feasibility = reflexion.tab.borrow().feasibility;
        let (gradings, boxsubforms): (Vec<_>, Vec<_>) = constraintsets
            .into_iter()
            .map(|cns| (cns.gradings, cns.boxsubforms))
            .unzip();
        let mut this = TransitTB {
            feasibility,
            paraws: vec![reflexion],
            constraints: gradings,
            solutions: vec![],
        };
        for (fks, formulae) in forks.into_iter().zip(boxsubforms) {
            let paraws = ParallelWorlds::from_forks(formulae, fks, None, calc);
            if paraws.tab.borrow().is_closed() {
                this.paraws.push(paraws);
                return Some(this);
            }
            this.paraws.push(paraws);
        }
        this.solve();
        Some(this)
    }
}

impl BaseTransit for TransitB5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        general_transit(calc, fruit)
    }
}

impl BaseTransit for Transit5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        general_transit(calc, fruit)
    }
}

impl<T: BaseTransit> ParaClique<T> {
    pub(crate) fn new<'a>(
        submodals: impl Iterator<Item = &'a LabeledFormula>,
        signs: &Vec<bool>,
        modalboxes: impl Iterator<Item = LabeledFormula>,
        mut spotranges: Vec<RangeInclusive<usize>>,
        leaf: &Rc<RefCell<TableauNode2<T>>>,
        calc: &mut Calculus,
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

impl BaseTransit for Transit4 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        general_transit(calc, fruit)
    }
}

impl Transit4 {
    fn from_diffraction(
        modals: Modals,
        mut ranges: Vec<RangeInclusive<usize>>,
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
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
        calc: &mut Calculus,
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
