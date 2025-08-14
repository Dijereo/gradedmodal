use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{
    flatformula::FlatFormula,
    formula::Formula,
    frame::FrameCondition,
    tableau2::{Conflict, DupContra, LabeledFormula, TabBranch, TabChildren, TableauNode2},
    transit::{BaseTransit, Modals, SolveTransit},
};

#[derive(Clone, Copy)]
pub(crate) enum Feasibility {
    Feasible,
    NoSolution,
    Contradiction,
}

struct PropLinear;
struct PropFork;
struct TLinear;

#[derive(Clone, Debug)]
pub(crate) enum ForkType {
    Disjunction,
    ParallelWorlds,
}

#[derive(Clone, Debug)]
struct Branch {
    id: usize,
    labels: Vec<LabeledFormula>,
}

#[derive(Clone, Debug)]
pub(crate) struct Fork {
    pub(crate) id: usize,
    pub(crate) fktype: ForkType,
    pub(crate) branches: Vec<Branch>,
}

pub(crate) struct Calculus {
    pub(crate) framecond: FrameCondition,
    pub(crate) forks: ForkStore,
}

pub(crate) struct ForkStore {
    pub(crate) forks: Vec<Fork>,
}

impl Calculus {
    pub(crate) fn sat<T: BaseTransit>(
        framecond: FrameCondition,
        mut formula: Rc<Formula>,
    ) -> Rc<RefCell<TableauNode2<T>>> {
        if framecond.luminal() {
            formula = FlatFormula::from(formula.clone()).into();
        }
        let formula = LabeledFormula {
            formula,
            conflictset: vec![],
            lemma: false,
        };
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(vec![formula], None)));
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
        calc.transition(&tab);
        tab
    }

    pub(crate) fn transition<T: BaseTransit>(&mut self, tab: &Rc<RefCell<TableauNode2<T>>>) {
        if tab.borrow().is_closed() {
            return;
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(tab, &mut flowers);
        for flower in flowers {
            if let Some(transit) = T::transit(&flower, self) {
                flower.borrow_mut().feasibility = transit.feasibility();
                flower.borrow_mut().children = TabChildren::Transition(transit);
            }
        }
        TableauNode2::set_feasibility_rec(tab);
    }

    pub(crate) fn expand_static<T: BaseTransit>(
        &mut self,
        tab: &Rc<RefCell<TableauNode2<T>>>,
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

    fn expand_linear<T: BaseTransit>(&self, tab: &mut TableauNode2<T>, reflexive: bool) {
        let mut i = 0;
        while let Some(label) = tab.formulae.get(i).cloned() {
            let mut new_formulae = PropLinear.expand(&label.formula);
            if reflexive {
                new_formulae.extend(TLinear.expand(&label.formula));
            }
            for new_formula in new_formulae {
                tab.add_check_dup_contra(LabeledFormula {
                    formula: new_formula,
                    conflictset: label.conflictset.clone(),
                    lemma: label.lemma,
                });
                if tab.is_closed() {
                    return;
                }
            }
            i += 1;
        }
    }

    fn store_disjs<T: BaseTransit>(&mut self, tab: &TableauNode2<T>, forks: &mut VecDeque<Fork>) {
        for label in &tab.formulae {
            if label.lemma {
                continue;
            }
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

    fn resolve_forks<T: BaseTransit>(
        &mut self,
        tab: &Rc<RefCell<TableauNode2<T>>>,
        forks: &mut VecDeque<Fork>,
    ) {
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
                let confs = tab.borrow_mut().add_check_dup_contra(LabeledFormula {
                    formula: Formula::bottom(),
                    conflictset,
                    lemma: false,
                });
                return;
            } else if fork.branches.len() == 1 {
                let branch = fork.branches.pop().expect("Checked in if statement above");
                tab.borrow_mut().choices.push((fork.id, branch.id));
                for label in branch.labels {
                    let confs = tab.borrow_mut().add_check_dup_contra(LabeledFormula {
                        formula: label.formula,
                        conflictset: conflictset.clone(),
                        lemma: label.lemma,
                    });
                    if tab.borrow().is_closed() {
                        return;
                    }
                }
                forks.append(&mut unresolved);
            } else if let Some(branch) = fork.branches.iter().find(|b| b.labels.is_empty()) {
                tab.borrow_mut().choices.push((fork.id, branch.id));
                for label in &branch.labels {
                    let confs = tab.borrow_mut().add_check_dup_contra(LabeledFormula {
                        formula: label.formula.clone(),
                        conflictset: conflictset.clone(),
                        lemma: label.lemma,
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

    fn apply_forks<T: BaseTransit>(
        &mut self,
        tab: &Rc<RefCell<TableauNode2<T>>>,
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
        tab.borrow_mut().feasibility = feasibility;
    }
}

impl ForkStore {
    pub(crate) fn create_fork(
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
                let label = LabeledFormula {
                    formula,
                    conflictset: conflictset.clone(),
                    lemma: false,
                };
                fork_branch.labels.push(label);
            }
            fork.branches.push(fork_branch);
        }
        self.forks.push(fork.clone());
        fork
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
            Formula::DiamondLe(0, phi) => vec![phi.not()],
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
            // Feasibility::Uknown => "?",
            // Feasibility::Infeasible => "⨉",
            // ✅❌🚫
        }
    }

    pub(crate) const fn is_bad(&self) -> bool {
        match self {
            Feasibility::Contradiction | Feasibility::NoSolution => true,
            Feasibility::Feasible => false,
        }
    }

    pub(crate) const fn better(&self, other: &Self) -> Self {
        match (self, other) {
            (_, Feasibility::Feasible) | (Feasibility::Feasible, _) => Feasibility::Feasible,
            (_, Feasibility::NoSolution) | (Feasibility::NoSolution, _) => Feasibility::NoSolution,
            _ => Feasibility::Contradiction,
            // (_, Feasibility::Uknown) | (Feasibility::Uknown, _) => Feasibility::Uknown,
        }
    }
}
