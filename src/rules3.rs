use std::{cell::RefCell, collections::VecDeque, fmt, ops::RangeInclusive, rc::Rc};

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
    // leaves_backlog: VecDeque<Rc<RefCell<TableauNode2>>>,
    framecond: FrameCondition,
    forks: Vec<Fork>,
}

pub(crate) enum Feasibility {
    Contradiction,
    Unfeasible,
    Feasible,
    NoSolution,
}

pub(crate) struct Modals {
    pub(crate) bx: Vec<Label>,
    pub(crate) ge: Vec<(u32, Label)>,
    pub(crate) le: Vec<(u32, Label)>,
}

pub(crate) struct Constraint {
    pub(crate) forkid: usize,
    pub(crate) sense: bool,
    pub(crate) value: u32,
}

pub(crate) struct ParallelWorlds {
    pub(crate) tab: Rc<RefCell<TableauNode2>>,
    pub(crate) forkid_ranges: Vec<RangeInclusive<usize>>,
    pub(crate) choices: Vec<Vec<(usize, usize)>>,
}

pub(crate) enum Transit {
    KOr45(TransitKOr45),
    B5(TransitB5),
}

pub(crate) struct TransitKOr45 {
    pub(crate) paraws: ParallelWorlds,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) solution: Vec<u32>,
}

pub(crate) struct TransitB5 {
    pub(crate) paraws: ParallelWorlds,
    pub(crate) reflexion: ParallelWorlds,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) modals: Modals,
    pub(crate) solution: Vec<u32>,
    pub(crate) rfxsolution: usize,
}

// pub(crate) struct Graded5Transit {
//     pub(crate) framecond: FrameCondition,
//     pub(crate) boxed: Vec<Rc<Formula>>,
//     pub(crate) diamge: Vec<(u32, Rc<Formula>)>,
//     pub(crate) diamle: Vec<(u32, Rc<Formula>)>,
//     pub(crate) submodals: Vec<Rc<Formula>>,
//     pub(crate) clique_settings: Vec<bool>,
//     pub(crate) spottab: Option<Rc<RefCell<TableauNode2>>>,
//     pub(crate) cliquetab: Option<Rc<RefCell<TableauNode2>>>,
//     pub(crate) outcome: Feasibility,
//     pub(crate) solution: Option<(Vec<u32>, Vec<u32>)>,
// }

impl GradedKCalc {
    pub(crate) fn sat(
        mut formulae: Vec<Rc<Formula>>,
        framecond: FrameCondition,
    ) -> (Rc<RefCell<TableauNode2>>, Self) {
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
        let mut calc = Self {
            framecond,
            forks: vec![],
        };
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(labels, None)));
        if tab.borrow().is_closed {
            return (tab, calc);
        }
        // calc.apply(&tab, );
        calc.expand_static(&tab, VecDeque::new());
        if tab.borrow().is_closed {
            return (tab, calc);
        }
        let feasible = calc.transition_rec(&tab);
        // TODO: tab.borrow_mut().is_closed |= !feasible;
        (tab, calc)
    }

    fn transition_rec(&mut self, tab: &Rc<RefCell<TableauNode2>>) -> Feasibility {
        if tab.borrow().is_closed {
            return Feasibility::Contradiction;
        }
        let mut open_leaves = Vec::new();
        TableauNode2::get_open_leaves(tab, &mut open_leaves, false);
        let mut feasibility = Feasibility::Unfeasible;
        for leaf in open_leaves {
            match self.transit(&leaf) {
                Some((Feasibility::Feasible, transit)) => {
                    feasibility = Feasibility::Feasible;
                    leaf.borrow_mut().children =
                        TabChildren::Transition(Feasibility::Feasible, transit);
                }
                Some((feas, transit)) => {
                    leaf.borrow_mut().children = TabChildren::Transition(feas, transit);
                }
                None => feasibility = Feasibility::Feasible,
            }
        }
        feasibility
    }

    fn expand_static(&mut self, tab: &Rc<RefCell<TableauNode2>>, mut forks: VecDeque<Fork>) {
        self.expand_linear(&mut tab.borrow_mut());
        if tab.borrow().is_closed {
            return;
        }
        self.store_disjs(&tab.borrow(), &mut forks);
        self.resolve_forks(&tab, &mut forks);
        if tab.borrow().is_closed {
            return;
        }
        self.apply_forks(tab, forks);
    }

    fn expand_linear(&self, tab: &mut TableauNode2) {
        let mut i = 0;
        while let Some(label) = tab.formulae.get(i).cloned() {
            let mut new_formulae = PropLinear.expand(&label.formula);
            if self.framecond.reflexive() {
                new_formulae.extend(TLinear.expand(&label.formula));
            }
            for new_formula in new_formulae {
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

    fn store_disjs(&mut self, tab: &TableauNode2, forks: &mut VecDeque<Fork>) {
        for label in &tab.formulae {
            let branches = PropFork.expand(&label.formula);
            if branches.is_empty() {
                continue;
            }
            forks.push_back(self.create_fork(ForkType::Disjunction, branches, &label.conflictset));
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
                    if tab.borrow().is_closed {
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
                    if tab.borrow().is_closed {
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
                        branchid: branch.id,
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

    fn transit(&mut self, leaf: &Rc<RefCell<TableauNode2>>) -> Option<(Feasibility, Transit)> {
        let modals = Modals::new(leaf, self.framecond.serial());
        if modals.ge.is_empty() {
            return None;
        }
        let mut transit = match self.framecond {
            FrameCondition::K | FrameCondition::D | FrameCondition::K45 | FrameCondition::D45 => {
                Transit::KOr45(TransitKOr45::from_modals(modals, self))
            }
            FrameCondition::KB5 | FrameCondition::S5 => {
                match TransitB5::from_modals(modals, leaf, self) {
                    Ok(transit) => Transit::B5(transit),
                    Err(transit) => Transit::KOr45(transit),
                }
            }
        };
        if transit.is_closed() {
            return Some((Feasibility::Contradiction, transit));
        }
        let feas = transit.recurse(self);
        match feas {
            Feasibility::Contradiction | Feasibility::Unfeasible | Feasibility::NoSolution => {
                return Some((feas, transit));
            }
            Feasibility::Feasible => {}
        }
        Some(transit.solve())
    }
}

impl Transit {
    fn recurse(&mut self, calc: &mut GradedKCalc) -> Feasibility {
        match self {
            Transit::KOr45(transit) => calc.transition_rec(&transit.paraws.tab),
            Transit::B5(_) => Feasibility::Feasible,
        }
    }

    fn is_closed(&self) -> bool {
        match self {
            Transit::KOr45(transit) => transit.paraws.tab.borrow().is_closed,
            Transit::B5(transit) => transit.paraws.tab.borrow().is_closed,
        }
    }
}

impl Modals {
    fn new(leaf: &Rc<RefCell<TableauNode2>>, serial: bool) -> Modals {
        let mut this = Modals {
            bx: vec![],
            ge: vec![],
            le: vec![],
        };
        leaf.borrow().traverse_anc_formulae(&mut |label| {
            this.store(&label);
            true
        });
        if serial && this.ge.is_empty() && !this.is_empty() {
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

    fn is_empty(&self) -> bool {
        self.bx.is_empty() && self.ge.is_empty() && self.le.is_empty()
    }

    pub(crate) fn display_modals(
        &self,
        f: &mut fmt::Formatter<'_>,
        cns: &Vec<Constraint>,
        calc: &GradedKCalc,
    ) -> fmt::Result {
        for cn in cns {
            if cn.sense {
                writeln!(
                    f,
                    "(≥{}): φ{} := {}",
                    cn.value, cn.forkid, calc.forks[cn.forkid].branches[1].labels[0].formula
                )?;
            } else {
                writeln!(
                    f,
                    "(≤{}): φ{} := {}",
                    cn.value, cn.forkid, calc.forks[cn.forkid].branches[1].labels[0].formula
                )?;
            }
        }
        for phi in self.bx.iter() {
            writeln!(f, "□: {}", phi.formula)?;
        }
        Ok(())
    }
}

impl ParallelWorlds {
    fn new<'a>(
        determ: Vec<Label>,
        nondeterm: impl Iterator<Item = &'a Label>,
        calc: &mut GradedKCalc,
    ) -> Self {
        let forks = VecDeque::from_iter(nondeterm.map(|f| {
            calc.create_fork(
                ForkType::ParallelWorlds,
                vec![vec![f.formula.not()], vec![f.formula.clone()]],
                &f.conflictset,
            )
        }));
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(determ, None)));
        let minforkid = forks
            .front()
            .expect("Parallel World should have at least one fork")
            .id;
        let maxforkid = forks
            .back()
            .expect("Parallel World should have at least one fork")
            .id;
        calc.expand_static(&tab, forks);
        Self {
            tab,
            forkid_ranges: vec![minforkid..=maxforkid],
            choices: vec![],
        }
    }

    pub(crate) fn set_choices(&mut self) {
        let mut seeds = vec![];
        let mut choices = vec![];
        TableauNode2::get_open_leaves(&self.tab, &mut seeds, true);
        for seed in seeds {
            if seed.borrow().is_closed {
                continue;
            }
            let mut subchoices = vec![];
            TableauNode2::get_choices(&seed, &mut subchoices, &self.forkid_ranges);
            choices.push(subchoices);
        }
        choices.dedup();
        self.choices = choices;
    }
}

impl TransitKOr45 {
    fn from_modals(modals: Modals, calc: &mut GradedKCalc) -> Self {
        let paraws = ParallelWorlds::new(
            modals.bx.clone(),
            modals.ge.iter().chain(modals.le.iter()).map(|(_, l)| l),
            calc,
        );
        let mut constraints = vec![];
        for ((value, sense), forkid) in modals
            .ge
            .iter()
            .map(|(c, _)| (*c, true))
            .chain(modals.le.iter().map(|(c, _)| (*c, false)))
            .zip(paraws.forkid_ranges[0].clone())
        {
            constraints.push(Constraint {
                forkid,
                sense,
                value,
            });
        }
        Self {
            paraws,
            constraints,
            modals,
            solution: vec![],
        }
    }
}

impl TransitB5 {
    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2>>,
        calc: &mut GradedKCalc,
    ) -> Result<Self, TransitKOr45> {
        let paraws = ParallelWorlds::new(
            modals.bx.clone(),
            modals.ge.iter().chain(modals.le.iter()).map(|(_, l)| l),
            calc,
        );
        if paraws.tab.borrow().is_closed {
            return Err(TransitKOr45 {
                paraws,
                constraints: vec![],
                modals,
                solution: vec![],
            });
        }
        let mut constraints = vec![];
        for ((value, sense), forkid) in modals
            .ge
            .iter()
            .map(|(c, _)| (*c, true))
            .chain(modals.le.iter().map(|(c, _)| (*c, false)))
            .zip(paraws.forkid_ranges[0].clone())
        {
            constraints.push(Constraint {
                forkid,
                sense,
                value,
            });
        }
        Ok(Self {
            reflexion: Self::get_reflexion(&modals, &paraws, leaf, calc),
            paraws,
            constraints,
            modals,
            solution: vec![],
            rfxsolution: 0,
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
        let forks = VecDeque::from_iter(
            paraws
                .forkid_ranges
                .iter()
                .flat_map(|r| calc.forks[r.clone()].iter())
                .cloned(),
        );
        let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(formulae, None)));
        calc.expand_static(&tab, forks);
        ParallelWorlds {
            tab,
            forkid_ranges: paraws.forkid_ranges.clone(),
            choices: vec![],
        }
    }
}

// impl Graded5Transit {
//     fn create(leaf: &Rc<RefCell<TableauNode2>>, framecond: FrameCondition) -> Graded5Transit {
//         let mut transit = Self::init(leaf, framecond);
//         if transit.diamge.is_empty() {
//             return transit;
//         }
//         let tab = Rc::new(RefCell::new(TableauNode2::from_formulae(
//             transit
//                 .boxed
//                 .iter()
//                 .map(|f| Label {
//                     formula: f.clone(),
//                     conflictset: vec![],
//                 })
//                 .collect(),
//             None,
//         )));
//         if tab.borrow().is_closed {
//             transit.outcome = Feasibility::Contradiction;
//             transit.root = Some(tab);
//             return transit;
//         }
//         let forks = transit.get_spot_forks();
//         let mut calc = GradedKCalc {
//             framecond: transit.framecond,
//             forks: forks.iter().cloned().collect(),
//         };
//         let feasible = calc.apply(&tab, forks);
//         if tab.borrow().is_closed {
//             transit.root = Some(tab);
//             transit.outcome = Feasibility::Contradiction;
//             return transit;
//         } else if !feasible {
//             transit.root = Some(tab);
//             transit.outcome = Feasibility::Unfeasible;
//             return transit;
//         }
//         let mut seeds = vec![];
//         TableauNode2::get_open_leaves(&tab, &mut seeds, true);
//         transit.root = Some(tab);
//         for seed in seeds {
//             if seed.borrow().is_closed {
//                 continue;
//             }
//             let mut choices = vec![];
//             TableauNode2::get_choices(&seed, &mut choices);
//             // println!("Choice {:?}", choices);
//             choices.sort_unstable();
//             choices.truncate(transit.diamge.len() + transit.diamle.len());
//             if transit.framecond.cliqued() {
//                 transit.reflexion.reflect(&seed, &choices);
//             }
//             transit.para_worlds.push(choices);
//         }
//         transit.para_worlds.sort_unstable_by(Vec::cmp);
//         transit.para_worlds.dedup_by(|ch1, ch2| ch1 == ch2);
//         transit.reflexion.para_worlds.sort_unstable_by(Vec::cmp);
//         transit
//             .reflexion
//             .para_worlds
//             .dedup_by(|ch1, ch2| ch1 == ch2);
//         check_feasibility(&mut transit);
//         transit
//     }

//     fn init(leaf: &Rc<RefCell<TableauNode2>>, framecond: FrameCondition) -> Graded5Transit {
//         let mut transit = Self {
//             framecond,
//             boxed: vec![],
//             diamge: vec![],
//             diamle: vec![],
//             submodals: vec![],
//             clique_settings: vec![],
//             spottab: None,
//             cliquetab: None,
//             outcome: Feasibility::NoTransition,
//             solution: None,
//         };
//         leaf.borrow().traverse_anc_formulae(&mut |label| {
//             transit.store_formula(&label.formula);
//             true
//         });
//         let dummy = Formula::bottom();
//         for f in transit
//             .diamge
//             .iter_mut()
//             .chain(transit.diamle.iter_mut())
//             .map(|(_, f)| f)
//             .chain(transit.boxed.iter_mut())
//         {
//             *f = Depth1F::from(mem::replace(f, dummy.clone())).into();
//             f.store_modals(&mut transit.submodals);
//         }
//         transit.clique_settings = vec![true; transit.submodals.len()];
//         if transit.framecond.serial() && transit.diamge.is_empty() && !transit.is_empty() {
//             transit.diamge.push((1, Formula::top()));
//         }
//         transit
//     }

//     fn store_formula(&mut self, formula: &Rc<Formula>) {
//         match formula.as_ref() {
//             Formula::Bottom
//             | Formula::Top
//             | Formula::PropVar(..)
//             | Formula::Not(_)
//             | Formula::And(..)
//             | Formula::Or(..)
//             | Formula::Imply(..)
//             | Formula::Iff(..) => {}
//             Formula::Box(phi) => self.boxed.push(phi.clone()),
//             Formula::Diamond(phi) => self.diamge.push((1, phi.clone())),
//             Formula::DiamondGe(count, phi) => self.diamge.push((*count, phi.clone())),
//             Formula::DiamondLe(count, phi) => self.diamle.push((*count, phi.clone())),
//         }
//     }

//     fn get_spot_forks(&self) -> VecDeque<Fork> {
//         VecDeque::from_iter(
//             self.diamge
//                 .iter()
//                 .chain(self.diamle.iter())
//                 .enumerate()
//                 .map(|(id, (c, f))| Fork {
//                     id,
//                     fktype: ForkType::ParallelWorlds,
//                     branches: vec![
//                         Branch {
//                             id: 0,
//                             labels: vec![Label {
//                                 formula: f.not(),
//                                 conflictset: vec![],
//                             }],
//                         },
//                         Branch {
//                             id: 1,
//                             labels: vec![Label {
//                                 formula: f.clone(),
//                                 conflictset: vec![],
//                             }],
//                         },
//                     ],
//                 }),
//         )
//     }

//     fn get_clique_forks(&self) -> VecDeque<Fork> {
//         VecDeque::from_iter(
//             self.submodals
//                 .iter()
//                 .enumerate()
//                 .map(|(id, (c, f))| Fork {
//                     id,
//                     fktype: ForkType::ParallelWorlds,
//                     branches: vec![
//                         Branch {
//                             id: 0,
//                             labels: vec![Label {
//                                 formula: f.not(),
//                                 conflictset: vec![],
//                             }],
//                         },
//                         Branch {
//                             id: 1,
//                             labels: vec![Label {
//                                 formula: f.clone(),
//                                 conflictset: vec![],
//                             }],
//                         },
//                     ],
//                 }),
//         )
//     }

//     pub(crate) fn is_empty(&self) -> bool {
//         self.boxed.is_empty() && self.diamge.is_empty() && self.diamle.is_empty()
//     }

//     pub(crate) fn display_modals(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         for (i, (c, phi)) in self.diamge.iter().enumerate() {
//             writeln!(f, "(≥{c}): φ{i} := {phi}")?;
//         }
//         for ((c, phi), i) in self.diamle.iter().zip(self.diamge.len()..) {
//             writeln!(f, "(≤{c}): φ{i} := {phi}")?;
//         }
//         for phi in self.boxed.iter() {
//             writeln!(f, "□: {phi}")?;
//         }
//         Ok(())
//     }
// }

impl Formula {
    fn store_modals(self: &Rc<Formula>, out: &mut Vec<Rc<Formula>>) {
        match self.as_ref() {
            Formula::Bottom | Formula::Top | Formula::PropVar(_, _) => {}
            Formula::Not(phi) => phi.store_modals(out),
            Formula::Box(phi)
            | Formula::Diamond(phi)
            | Formula::DiamondGe(_, phi)
            | Formula::DiamondLe(_, phi) => out.push(phi.clone()),
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
