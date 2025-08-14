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
