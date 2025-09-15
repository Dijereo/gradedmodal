use std::{cell::RefCell, collections::VecDeque, fmt, mem, ops::RangeInclusive, rc::Rc};

use crate::{
    flatformula::FlatFormula,
    formula::Formula,
    model::GraphInner,
    rules::{Calculus, Feasibility, ForkStore, ForkType},
    tableau::{DisplayTableau, LabeledFormula, TableauNode2},
    timeout::{MayTimeout, TimeoutHandler},
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

    fn transit(
        fruit: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>>;
}

pub(crate) trait SolveTransit: BaseTransit {
    fn solve(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<()>;

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self>;

    fn recurse(&mut self, calc: &mut Calculus, toh: &impl TimeoutHandler) -> MayTimeout<()>;
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

pub(crate) trait ModelTransit: Sized {
    fn to_graph_inner(this: DisplayTableau<Self>) -> GraphInner;
}

pub(crate) fn general_transit<T: BaseTransit + SolveTransit>(
    calc: &mut Calculus,
    fruit: &Rc<RefCell<TableauNode2<T>>>,
    toh: &impl TimeoutHandler,
) -> MayTimeout<Option<T>> {
    let mut labels = vec![];
    fruit.borrow().traverse_anc_formulae(&mut |label| {
        labels.push(label.clone());
        true
    });
    let modals = Modals::new(
        labels.iter(),
        calc.framecond.ray(),
        calc.framecond.spotlit(),
        toh,
    )?;
    if modals.ge.is_empty() {
        return Ok(None);
    }
    let mut transit = T::from_modals(modals, fruit, calc, toh)?;
    if transit.is_closed() {
        return Ok(Some(transit));
    }
    transit.recurse(calc, toh)?;
    if transit.is_closed() {
        return Ok(Some(transit));
    }
    transit.solve(toh)?;
    Ok(Some(transit))
}

impl Modals {
    pub(crate) fn new<'a>(
        labels: impl Iterator<Item = &'a LabeledFormula>,
        serial: bool,
        spotlit: bool,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Modals> {
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
                f.formula =
                    FlatFormula::from_rcf(mem::replace(&mut f.formula, dummy.clone()), toh)?.into();
            }
        }
        if serial && this.ge.is_empty() && (!this.le.is_empty() || !this.bx.is_empty()) {
            this.ge.push((
                1,
                LabeledFormula {
                    formula: Formula::top(),
                    conflictset: vec![],
                    lemma: false,
                    expanded: false,
                },
            ));
        }
        Ok(this)
    }

    pub(crate) fn iter_all<'a>(&'a self) -> impl Iterator<Item = &'a LabeledFormula> {
        self.ge
            .iter()
            .chain(self.le.iter())
            .map(|(_, f)| f)
            .chain(self.bx.iter())
    }

    pub(crate) fn to_forks_constraints(
        self,
        forkstore: &mut ForkStore,
    ) -> (Option<RangeInclusive<usize>>, Constraints) {
        let mut startid = None;
        let mut endid = 0;
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
            if let None = startid {
                startid = Some(fork.id);
            }
            endid = fork.id;
        }
        let range = startid.map(|s| s..=endid);
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
                    expanded: false,
                };
                self.bx.push(subform);
                self.bx.last()
            }
            Formula::Diamond(phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                    expanded: false,
                };
                self.ge.push((1, subform));
                self.ge.last().map(|(_, f)| f)
            }
            Formula::DiamondGe(count, phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                    expanded: false,
                };
                self.ge.push((*count, subform));
                self.ge.last().map(|(_, f)| f)
            }
            Formula::DiamondLe(count, phi) => {
                let subform = LabeledFormula {
                    formula: phi.clone(),
                    conflictset: formula.conflictset.clone(),
                    lemma: false,
                    expanded: false,
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

    pub(crate) fn to_box_forks_constraints(
        self,
        forkstore: &mut ForkStore,
    ) -> (
        Option<RangeInclusive<usize>>,
        Constraints,
        Vec<(Rc<Formula>, usize)>,
    ) {
        let (forks, constraints) = self.to_forks_constraints(forkstore);
        let mut boxforks = vec![];
        let mut startid = None;
        let mut endid = 0;
        for formula in &constraints.boxsubforms {
            let fork = forkstore.create_fork(
                ForkType::ParallelWorlds,
                vec![vec![formula.formula.not()], vec![formula.formula.clone()]],
                &formula.conflictset,
            );
            if let None = startid {
                startid = Some(fork.id);
            }
            endid = fork.id;
            boxforks.push((formula.formula.clone(), fork.id));
        }
        let range = match (forks, startid) {
            (None, None) => None,
            (None, Some(startid)) => Some(startid..=endid),
            (r @ Some(_), None) => r,
            (Some(r), Some(_)) => Some(*r.start()..=endid),
        };
        (range, constraints, boxforks)
    }

    pub(crate) fn to_deep_forks_constraints(
        self,
        forkstore: &mut ForkStore,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<(
        Vec<RangeInclusive<usize>>,
        Constraints,
        Vec<(Rc<Formula>, usize)>,
    )> {
        let mut newformulae = vec![];
        for formula in self.iter_all() {
            formula.store_modals(&mut |f| newformulae.push(f));
        }
        let (forks, mut constraints) = self.to_forks_constraints(forkstore);
        let mut forks: Vec<_> = forks.into_iter().collect();
        let mut modals = Modals::new(newformulae.iter(), false, false, toh)?;
        let mut boxforks = vec![];
        while !modals.ge.is_empty() || !modals.le.is_empty() {
            newformulae.clear();
            for formula in modals.iter_all() {
                formula.store_modals(&mut |f| newformulae.push(f));
            }
            let (fks, cns, bxfks) = modals.to_box_forks_constraints(forkstore);
            forks.extend(fks);
            constraints.gradings.extend(cns.gradings);
            boxforks.extend(bxfks);
            modals = Modals::new(newformulae.iter(), false, false, toh)?;
        }
        Ok((forks, constraints, boxforks))
    }

    pub(crate) fn to_existing_forks(
        self,
        src_range: &Vec<RangeInclusive<usize>>,
        src_constraints: Constraints,
        forkstore: &ForkStore,
    ) -> (Vec<RangeInclusive<usize>>, Constraints) {
        let mut constraints = Vec::with_capacity(self.ge.len() + self.le.len());
        let mut forkids = Vec::with_capacity(self.ge.len() + self.le.len());
        let formulae = self.ge.into_iter().map(|(c, formula)| (c, true, formula));
        let formulae = formulae.chain(self.le.into_iter().map(|(c, formula)| (c, false, formula)));
        let src_gradings = VecDeque::from(src_constraints.gradings);
        let src_boxsubforms = VecDeque::from(src_constraints.boxsubforms);
        for (value, sense, formula) in formulae {
            let mut fork = None;
            for i in 0..src_gradings.len() {
                if formula
                    .formula
                    .directly_equivalent(&src_gradings[i].formula)
                {
                    fork = Some(&forkstore.forks[src_gradings[i].forkid]);
                    break;
                }
            }
            let fork = fork.expect("Fork should be found");
            constraints.push(Grading {
                forkid: fork.id,
                sense,
                value,
                formula: formula.formula,
            });
            forkids.push(fork.id..=fork.id);
        }
        (
            forkids,
            Constraints {
                gradings: constraints,
                boxsubforms: self.bx,
            },
        )
    }

    // pub(crate) fn to_deep_existing_forks(
    //     self,
    //     constraints: Constraints,
    //     forkstore: &ForkStore,
    // ) -> (Vec<RangeInclusive<usize>>, Constraints) {
    //     if self.ge.is_empty() && self.le.is_empty() {
    //         let (forks, constraints) = self.to_forks_constraints(forkstore);
    //         return (forks.into_iter().collect(), constraints);
    //     }
    //     let mut newformulae = vec![];
    //     for formula in self.iter_all() {
    //         formula.store_modals(&mut |f| newformulae.push(f));
    //     }
    //     let (forks, mut constraints) = self.to_forks_constraints(forkstore);
    //     let mut forks: Vec<_> = forks.into_iter().collect();
    //     let mut modals = Modals::new(newformulae.iter(), false, false);
    //     while !modals.ge.is_empty() || !modals.le.is_empty() {
    //         newformulae.clear();
    //         for formula in modals.iter_all() {
    //             formula.store_modals(&mut |f| newformulae.push(f));
    //         }
    //         let (fks, cns) = modals.to_box_forks_constraints(forkstore);
    //         forks.extend(fks);
    //         constraints.gradings.extend(cns.gradings);
    //         constraints.boxsubforms.extend(cns.boxsubforms);
    //         modals = Modals::new(newformulae.iter(), false, false);
    //     }
    //     (forks, constraints)
    // }
}

impl LabeledFormula {
    pub(crate) fn store_modals(&self, store: &mut impl FnMut(Self)) {
        self.formula.store_modals(&mut |f| {
            store(LabeledFormula {
                formula: f.clone(),
                conflictset: self.conflictset.clone(),
                lemma: false,
                expanded: false,
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
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<(Self, Constraints)> {
        let (forkranges, constraints) = modals.to_forks_constraints(&mut calc.forks);
        let this = Self::from_forks(
            constraints.boxsubforms.clone(),
            forkranges.into_iter().collect(),
            parent,
            calc,
            toh,
        )?;
        Ok((this, constraints))
    }

    pub(crate) fn from_forks(
        formulae: Vec<LabeledFormula>,
        forkids: Vec<RangeInclusive<usize>>,
        parent: Option<&Rc<RefCell<TableauNode2<T>>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self> {
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(formulae, parent)));
        let forks = VecDeque::from_iter(
            forkids
                .iter()
                .flat_map(|r| calc.forks.forks[r.clone()].iter().cloned()),
        );
        calc.expand_static(&tab, forks, false, toh)?;
        Ok(Self {
            tab,
            forkids,
            choices: vec![],
        })
    }

    fn add_forks(
        &mut self,
        forkids: impl Iterator<Item = RangeInclusive<usize>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<()> {
        let mut forks = VecDeque::new();
        for range in forkids {
            forks.extend(calc.forks.forks[range.clone()].iter().cloned());
            self.forkids.push(range);
        }
        calc.expand_static(&self.tab, forks, false, toh)?;
        Ok(())
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
