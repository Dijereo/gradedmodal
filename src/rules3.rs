use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{
    formula::Formula,
    tableau2::{Conflict, DupContra, Label, TabChild, TableauNode2},
};

struct PropLinear;
struct PropFork;

#[derive(Clone, Debug)]
enum ForkType {
    Conjunction,
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
    // leaves_backlog: VecDeque<Rc<RefCell<TableauNode2>>>,
    forks: Vec<Fork>,
}

struct GradedTransit {
    boxed: Vec<Rc<Formula>>,
    diamge: Vec<(u32, Rc<Formula>)>,
    diamle: Vec<(u32, Rc<Formula>)>,
}

impl GradedKCalc {
    pub(crate) fn sat(formulae: Vec<Rc<Formula>>) -> Rc<RefCell<TableauNode2>> {
        let labels = formulae
            .into_iter()
            .map(|f| Label {
                formula: f,
                conflictset: vec![],
            })
            .collect();
        let mut calc = Self { forks: vec![] };
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(labels, None)));
        if !tab.borrow().is_closed {
            calc.init(&mut tab.borrow_mut());
            calc.apply(&tab);
        }
        tab
    }

    fn init(&self, tab: &mut TableauNode2) {
        // todo!()
    }

    fn apply(&mut self, tab: &Rc<RefCell<TableauNode2>>) {
        if tab.borrow().is_closed {
            return;
        }
        self.expand_static(tab, VecDeque::new());
        let mut open_leaves = Vec::new();
        TableauNode2::get_open_leaves(tab, &mut open_leaves);
        for leaf in open_leaves {
            let transit = GradedTransit::create(&leaf);
        }
        todo!()

        // if tab.is_closed {
        //     return;
        // }
        // todo!()
        // self.apply_trans(world);
    }

    fn expand_paral_worlds(&mut self, tab: &Rc<RefCell<TableauNode2>>, mut forks: VecDeque<Fork>) {
        if !tab.borrow().is_closed {
            todo!()
        }
        self.expand_static(tab, VecDeque::new());
        let mut open_leaves = Vec::new();
        TableauNode2::get_open_leaves(tab, &mut open_leaves);
        for leaf in open_leaves {
            let transit = GradedTransit::create(&leaf);
        }
        // tab
        todo!()
    }

    fn expand_static(&mut self, tab: &Rc<RefCell<TableauNode2>>, mut forks: VecDeque<Fork>) {
        self.expand_linear(&mut tab.borrow_mut());
        if tab.borrow().is_closed {
            return;
        }
        self.add_forks(&tab.borrow(), &mut forks);
        self.resolve_forks(&mut tab.borrow_mut(), &mut forks);
        if tab.borrow().is_closed {
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

    fn add_forks(&mut self, tab: &TableauNode2, forks: &mut VecDeque<Fork>) {
        for label in &tab.formulae {
            let branches = PropFork.expand(&label.formula);
            if branches.is_empty() {
                continue;
            }
            let fork = Fork {
                id: self.forks.len(),
                fktype: ForkType::Conjunction,
                branches: branches
                    .into_iter()
                    .enumerate()
                    .map(|(i, b)| Branch {
                        id: i,
                        labels: b
                            .into_iter()
                            .map(|f| Label {
                                formula: f,
                                conflictset: label.conflictset.clone(),
                            })
                            .collect(),
                    })
                    .collect(),
            };
            forks.push_back(fork.clone());
            self.forks.push(fork);
        }
    }

    fn resolve_forks(&self, tab: &mut TableauNode2, forks: &mut VecDeque<Fork>) {
        let mut unresolved = VecDeque::new();
        while let Some(mut fork) = forks.pop_front() {
            let mut conflictset = vec![];
            let mut i = 0;
            'outer: while let Some(branch) = fork.branches.get_mut(i) {
                let mut j = 0;
                'inner: while let Some(label) = branch.labels.get_mut(j) {
                    match tab.check_dup_contra(&label.formula) {
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
                let confs = tab.add_check_dup_contra(Label {
                    formula: Formula::bottom(),
                    conflictset,
                });
                return;
            } else if fork.branches.len() == 1 {
                let branch = fork.branches.pop().expect("Checked in if statement above");
                for label in branch.labels {
                    let confs = tab.add_check_dup_contra(Label {
                        formula: label.formula,
                        conflictset: conflictset.clone(),
                    });
                    if tab.is_closed {
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

    fn apply_forks(&mut self, tab: &Rc<RefCell<TableauNode2>>, mut forks: VecDeque<Fork>) {
        while tab.borrow().children.is_empty() {
            if let Some(fork) = forks.pop_front() {
                for branch in fork.branches {
                    tab.borrow_mut().forkid = Some(fork.id);
                    let child = Rc::new(RefCell::new(TableauNode2::from_formulae(
                        branch.labels,
                        Some(tab),
                    )));
                    tab.borrow_mut().children.push(TabChild {
                        branchid: branch.id,
                        node: child,
                    });
                }
            } else {
                return;
            }
        }
        let mut all_closed = true;
        for child in &tab.borrow().children {
            if child.node.borrow().is_closed {
                continue;
            }
            self.expand_static(&child.node, forks.clone());
            if !child.node.borrow().is_closed {
                all_closed = false;
            }
        }
        if all_closed {
            tab.borrow_mut().is_closed = true;
        }
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
        match formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(..)
            | Formula::Not(_)
            | Formula::And(..)
            | Formula::Or(..)
            | Formula::Imply(..)
            | Formula::Iff(..) => {}
            Formula::Box(formula) => self.boxed.push(formula.clone()),
            Formula::Diamond(formula) => self.diamge.push((1, formula.clone())),
            Formula::DiamondGe(count, formula) => self.diamge.push((*count, formula.clone())),
            Formula::DiamondLe(count, formula) => self.diamle.push((*count, formula.clone())),
        }
    }

    fn setup_paral_worlds(self) {
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(
            self.boxed
                .into_iter()
                .map(|f| Label {
                    formula: f,
                    conflictset: vec![],
                })
                .collect(),
            None,
        )));
        if tab.borrow().is_closed {
            return;
        }
        let forks = VecDeque::from_iter(
            self.diamge
                .iter()
                .chain(self.diamle.iter())
                .enumerate()
                .map(|(id, (c, f))| Fork {
                    id,
                    fktype: ForkType::ParallelWorlds,
                    branches: vec![Branch {
                        id: 0,
                        labels: vec![Label {
                            formula: f.clone(),
                            conflictset: vec![],
                        }],
                    }],
                }),
        );
        let mut calc = GradedKCalc {
            forks: forks.iter().cloned().collect(),
        };
        calc.expand_paral_worlds(&tab, forks);
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
