use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{
    formula::Formula,
    tableau2::{Conflict, DupContra, Label, TableauNode2},
};

struct PropLinear;
struct PropFork;

#[derive(Clone, Debug)]
enum ForkType {
    Conjunction,
    ParallelWorlds,
}

#[derive(Clone, Debug)]
struct Fork {
    forkid: usize,
    fktype: ForkType,
    branches: Vec<Vec<Label>>,
}

struct GradedKCalc {
    // leaves_backlog: VecDeque<Rc<RefCell<TableauNode2>>>,
    forks: Vec<Fork>,
}

struct GradedTransit {
    boxed: Vec<Rc<Formula>>,
    diamge: Vec<(u32, Rc<Formula>)>,
    diamle: Vec<(u32, Rc<Formula>)>,
}

impl GradedKCalc {
    fn sat(formulae: Vec<Rc<Formula>>) -> TableauNode2 {
        let labels = formulae
            .into_iter()
            .map(|f| Label {
                formula: f,
                conflictset: vec![],
            })
            .collect();
        let mut calc = Self { forks: vec![] };
        let mut tab = TableauNode2::from_formulae(labels, None);
        if !tab.is_closed {
            calc.init(&mut tab);
            calc.apply(&mut tab);
        }
        tab
    }

    fn init(&self, tab: &mut TableauNode2) {
        // todo!()
    }

    fn apply(&mut self, tab: &mut TableauNode2) {
        if tab.is_closed {
            return;
        }
        self.expand_static(tab);
        // while let Some(leaf) =  {
        //     let transit = GradedTransit::create(&leaf);
        //     todo!()
        // }
        // if tab.is_closed {
        //     return;
        // }
        todo!()
        // self.apply_trans(world);
    }

    fn expand_static(&mut self, tab: &mut TableauNode2) {
        self.expand_linear(tab);
        if tab.is_closed {
            return;
        }
        let mut forks = self.add_forks(tab);
        self.resolve_forks(tab, &mut forks);
        if tab.is_closed {
            return;
        }
        self.apply_forks(tab, forks)
    }

    fn expand_linear(&self, tab: &mut TableauNode2) {
        let mut i = 0;
        while let Some(label) = tab.formulae.get(i).cloned() {
            for new_formula in PropLinear.expand(&label.formula) {
                tab.add_check_dup_contra(Label {
                    formula: new_formula,
                    conflictset: label.conflictset.clone(),
                });
                if tab.is_closed {
                    return;
                }
            }
            i += 1;
        }
    }

    fn add_forks(&mut self, tab: &TableauNode2) -> VecDeque<Fork> {
        let mut fork_queue = VecDeque::new();
        for label in &tab.formulae {
            let branches = PropFork.expand(&label.formula);
            if branches.is_empty() {
                continue;
            }
            let fork = Fork {
                forkid: self.forks.len(),
                fktype: ForkType::Conjunction,
                branches: branches
                    .into_iter()
                    .map(|b| {
                        b.into_iter()
                            .map(|f| Label {
                                formula: f,
                                conflictset: label.conflictset.clone(),
                            })
                            .collect()
                    })
                    .collect(),
            };
            fork_queue.push_back(fork.clone());
            self.forks.push(fork);
        }
        fork_queue
    }

    fn resolve_forks(&self, tab: &mut TableauNode2, forks: &mut VecDeque<Fork>) {
        let mut unresolved = VecDeque::new();
        while let Some(mut fork) = forks.pop_front() {
            let mut i = 0;
            'outer: while let Some(branch) = fork.branches.get_mut(i) {
                let mut j = 0;
                'inner: while let Some(label) = branch.get_mut(j) {
                    match tab.check_dup_contra(&label.formula) {
                        DupContra::Ok => {}
                        DupContra::Bottom => {
                            fork.branches.swap_remove(i);
                            continue 'outer;
                        }
                        DupContra::Dup(conflicts) => {
                            branch.swap_remove(j);
                            continue 'inner;
                        }
                        DupContra::Contra(conflicts) => {
                            fork.branches.swap_remove(i);
                            continue 'outer;
                        }
                    }
                    j += 1;
                }
                if branch.is_empty() {
                    break 'outer;
                }
                i += 1;
            }
            if fork.branches.is_empty() {
                let confs = tab.add_check_dup_contra(Label {
                    formula: Formula::bottom(),
                    conflictset: todo!(),
                });
                return;
            } else if fork.branches.len() == 1 {
                let branch = fork.branches.pop().expect("Checked in if statement above");
                for label in branch {
                    let confs = tab.add_check_dup_contra(label);
                }
                forks.append(&mut unresolved);
            } else {
                unresolved.push_back(fork);
            }
        }
        forks.append(&mut unresolved);
    }

    fn apply_forks(&self, tab: &mut TableauNode2, forks: VecDeque<Fork>) {
        todo!()
    }
}

impl GradedTransit {
    fn create(leaf: &Rc<RefCell<TableauNode2>>) -> Self {
        let mut transit = Self {
            boxed: vec![],
            diamge: vec![],
            diamle: vec![],
        };
        leaf.borrow().traverse_anc_formulae(&mut |label| {
            transit.store_formula(&label.formula);
            true
        });
        if transit.diamge.is_empty() {
            return transit;
        }

        todo!()
    }

    fn store_formula(&mut self, formula: &Rc<Formula>) {
        todo!()
        // May Need to check for applicability in some cases
    }

    fn setup_world(&self, signs: &Vec<bool>) {
        todo!()
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
            Formula::Not(phi) => match phi.as_ref() {
                Formula::Bottom
                | Formula::Top
                | Formula::PropVar(..)
                | Formula::And(_, _)
                | Formula::Iff(_, _) => vec![],
                Formula::Not(psi) => vec![psi.clone()],
                Formula::Box(psi) => vec![psi.not().diamond()],
                Formula::Diamond(psi) => vec![psi.not().box_()],
                Formula::DiamondGe(count, psi) => vec![psi.dmle(count - 1)],
                Formula::DiamondLe(count, psi) => vec![psi.dmge(count + 1)],
                Formula::Or(psi1, psi2) => vec![psi1.not(), psi2.not()],
                Formula::Imply(psi1, psi2) => vec![psi1.clone(), psi2.not()],
            },
            Formula::And(phi1, phi2) => {
                vec![phi1.clone(), phi2.clone()]
            }
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
