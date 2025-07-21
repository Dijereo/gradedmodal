use std::{cell::RefCell, collections::VecDeque, fmt, rc::Rc};

use crate::{
    formula::Formula,
    ilp::check_feasibility,
    tableau2::{Conflict, DupContra, Label, TabBranch, TabChildren, TableauNode2},
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

pub(crate) struct GradedTransit {
    pub(crate) boxed: Vec<Rc<Formula>>,
    pub(crate) diamge: Vec<(u32, Rc<Formula>)>,
    pub(crate) diamle: Vec<(u32, Rc<Formula>)>,
    pub(crate) para_worlds: Vec<(Rc<RefCell<TableauNode2>>, Vec<(usize, usize)>)>,
    pub(crate) solution: Option<Vec<u32>>,
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
            calc.init(&tab);
            calc.apply(&tab, VecDeque::new());
        }
        tab
    }

    fn init(&self, tab: &Rc<RefCell<TableauNode2>>) {
        // todo!()
    }

    fn apply(&mut self, tab: &Rc<RefCell<TableauNode2>>, forks: VecDeque<Fork>) -> bool {
        if tab.borrow().is_closed {
            return false;
        }
        self.expand_static(tab, forks);
        if tab.borrow().is_closed {
            return false;
        }
        let mut open_leaves = Vec::new();
        let mut feasible = false;
        TableauNode2::get_open_leaves(tab, &mut open_leaves, false);
        for leaf in open_leaves {
            if let Some((trans_tab, transit)) = GradedTransit::create(&leaf) {
                leaf.borrow_mut().is_closed |= trans_tab.borrow().is_closed;
                feasible |= transit.solution.is_some();
                leaf.borrow_mut().children = TabChildren::Transition(trans_tab, transit);
            }
        }
        feasible
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
        self.apply_forks(tab, forks);
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

    fn resolve_forks(&mut self, tab: &mut TableauNode2, forks: &mut VecDeque<Fork>) {
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
                tab.choices.push((fork.id, branch.id));
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
        loop {
            match &tab.borrow().children {
                TabChildren::Transition(..) => return,
                TabChildren::Fork { branches, .. } if !branches.is_empty() => break,
                _ => {}
            };
            if let Some(fork) = forks.pop_front() {
                for branch in fork.branches {
                    if let TabChildren::Fork { id: forkid, .. } = &mut tab.borrow_mut().children {
                        *forkid = Some(fork.id);
                    }
                    let child = Rc::new(RefCell::new(TableauNode2::from_formulae(
                        branch.labels,
                        Some(tab),
                    )));
                    child.borrow_mut().choices.push((fork.id, branch.id));
                    if let TabChildren::Fork { branches, .. } = &mut tab.borrow_mut().children {
                        branches.push(TabBranch {
                            branchid: branch.id,
                            node: child,
                        });
                    }
                }
            } else {
                return;
            }
        }
        let mut all_closed = true;
        if let TabChildren::Fork { branches, .. } = &tab.borrow().children {
            for branch in branches.iter() {
                if branch.node.borrow().is_closed {
                    continue;
                }
                self.expand_static(&branch.node, forks.clone());
                if !branch.node.borrow().is_closed {
                    all_closed = false;
                }
            }
        }
        if all_closed {
            tab.borrow_mut().is_closed = true;
        }
    }
}

impl GradedTransit {
    fn create(leaf: &Rc<RefCell<TableauNode2>>) -> Option<(Rc<RefCell<TableauNode2>>, Self)> {
        let mut transit = Self {
            boxed: vec![],
            diamge: vec![],
            diamle: vec![],
            para_worlds: vec![],
            solution: None,
        };
        leaf.borrow().traverse_anc_formulae(&mut |label| {
            transit.store_formula(&label.formula);
            true
        });
        if transit.diamge.is_empty() {
            return None;
        }
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(
            transit
                .boxed
                .iter()
                .map(|f| Label {
                    formula: f.clone(),
                    conflictset: vec![],
                })
                .collect(),
            None,
        )));
        if tab.borrow().is_closed {
            return Some((tab, transit));
        }
        let forks = transit.get_forks();
        let mut calc = GradedKCalc {
            forks: forks.iter().cloned().collect(),
        };
        let feasible = calc.apply(&tab, forks);
        if tab.borrow().is_closed || !feasible {
            return Some((tab, transit));
        }
        let mut seeds = vec![];
        TableauNode2::get_open_leaves(&tab, &mut seeds, true);
        for seed in seeds {
            if seed.borrow().is_closed {
                continue;
            }
            let mut choices = vec![];
            TableauNode2::get_choices(&seed, &mut choices);
            // println!("Choice {:?}", choices);
            choices.sort_unstable();
            choices.truncate(transit.diamge.len() + transit.diamle.len());
            transit.para_worlds.push((seed, choices));
        }
        transit
            .para_worlds
            .sort_unstable_by(|(_, ch1), (_, ch2)| ch1.cmp(ch2));
        transit
            .para_worlds
            .dedup_by(|(_, ch1), (_, ch2)| ch1 == ch2);
        transit.solution = check_feasibility(&transit);
        Some((tab, transit))
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

    fn get_forks(&self) -> VecDeque<Fork> {
        VecDeque::from_iter(
            self.diamge
                .iter()
                .chain(self.diamle.iter())
                .enumerate()
                .map(|(id, (c, f))| Fork {
                    id,
                    fktype: ForkType::ParallelWorlds,
                    branches: vec![
                        Branch {
                            id: 0,
                            labels: vec![Label {
                                formula: f.clone(),
                                conflictset: vec![],
                            }],
                        },
                        Branch {
                            id: 1,
                            labels: vec![Label {
                                formula: f.not(),
                                conflictset: vec![],
                            }],
                        },
                    ],
                }),
        )
    }
}

impl fmt::Display for GradedTransit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (c, phi)) in self.diamge.iter().enumerate() {
            writeln!(f, "({i}) ≥{c}: {phi}")?;
        }
        for ((c, phi), i) in self.diamle.iter().zip(self.diamge.len()..) {
            writeln!(f, "({i}) ≤{c}: {phi}")?;
        }
        for phi in self.boxed.iter() {
            writeln!(f, "□: {phi}")?;
        }
        for (i, (_leaf, choice)) in self.para_worlds.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}{forkid} ", if *branchid == 0 { "" } else { "¬" })?;
            }
            writeln!(f)?;
        }
        match &self.solution {
            Some(values) => {
                for (i, val) in values.iter().enumerate() {
                    write!(f, "{val} w{i} ")?;
                }
                writeln!(f)
            }
            None => writeln!(f, "No solution"),
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
