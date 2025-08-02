use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{
    formula::Formula,
    tableau2::{DupContra, Label, TabBranch, TabChildren, TableauNode2},
};

#[derive(Clone, Debug)]
struct Fork {
    id: usize,
    branches: Vec<Branch>,
}

#[derive(Clone, Debug)]
struct Branch {
    id: usize,
    labels: Vec<Label>,
}

fn expand_static(
    tab: &Rc<RefCell<TableauNode2>>,
    fork_list: &mut Vec<Fork>,
    mut fork_queue: VecDeque<Fork>,
    reflexive: bool,
) {
    expand_linear(&mut tab.borrow_mut(), reflexive);
    if tab.borrow().is_closed() {
        return;
    }
    add_forks(&tab.borrow(), &mut fork_queue, fork_list);
    resolve_forks(&mut tab.borrow_mut(), &mut fork_queue);
    if tab.borrow().is_closed() {
        return;
    }
    apply_forks(tab, fork_list, fork_queue, reflexive);
}

fn expand_linear(tab: &mut TableauNode2, reflexive: bool) {
    let mut i = 0;
    while let Some(label) = tab.formulae.get(i).cloned() {
        let mut new_formulae = expand_prop_lin(&label.formula);
        if reflexive {
            new_formulae.extend(expand_t(&label.formula));
        }
        for new_formula in new_formulae {
            tab.add_check_dup_contra(Label {
                formula: new_formula,
                conflictset: label.conflictset.clone(),
            });
            if tab.is_closed() {
                return;
            }
        }
        i += 1;
    }
}

fn apply_forks(
    tab: &Rc<RefCell<TableauNode2>>,
    fork_list: &mut Vec<Fork>,
    mut forks: VecDeque<Fork>,
    reflexive: bool,
) {
    loop {
        match &tab.borrow().children {
            TabChildren::Transition(..) => return,
            TabChildren::Fork { branches, .. } if !branches.is_empty() => break,
            _ => {}
        };
        if let Some(fork) = forks.pop_front() {
            for branch in fork.branches {
                if let TabChildren::Fork { id: forkid, .. } = &mut tab.borrow_mut().children {
                    *forkid = fork.id;
                }
                let child = Rc::new(RefCell::new(TableauNode2::from_formulae(
                    branch.labels,
                    Some(tab),
                )));
                child.borrow_mut().choices.push((fork.id, branch.id));
                if let TabChildren::Fork { branches, .. } = &mut tab.borrow_mut().children {
                    branches.push(TabBranch {
                        id: branch.id,
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
            if branch.node.borrow().is_closed() {
                continue;
            }
            expand_static(&branch.node, fork_list, forks.clone(), reflexive);
            if !branch.node.borrow().is_closed() {
                all_closed = false;
            }
        }
    }
    if all_closed {
        // tab.borrow_mut().is_closed = true;
    }
}

fn add_forks(tab: &TableauNode2, fork_queue: &mut VecDeque<Fork>, fork_list: &mut Vec<Fork>) {
    for label in &tab.formulae {
        let branches = expand_prop_fork(&label.formula);
        if branches.is_empty() {
            continue;
        }
        let fork = Fork {
            id: fork_list.len(),
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
        fork_queue.push_back(fork.clone());
        fork_list.push(fork);
    }
}

fn resolve_forks(tab: &mut TableauNode2, forks: &mut VecDeque<Fork>) {
    let mut unresolved = VecDeque::new();
    while let Some(mut fork) = forks.pop_front() {
        let mut conflictset = vec![];
        let mut i = 0;
        'outer: while let Some(branch) = fork.branches.get_mut(i) {
            let mut j = 0;
            'inner: while let Some(label) = branch.labels.get_mut(j) {
                // OPT: only check leaf? except when adding new batch of forks after forking?
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
                if tab.is_closed() {
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

fn expand_prop_lin(formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
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
    }
}

fn expand_prop_fork(formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
    match formula.as_ref() {
        Formula::Bottom
        | Formula::Top
        | Formula::PropVar(..)
        | Formula::Box(_)
        | Formula::Diamond(_)
        | Formula::DiamondGe(..)
        | Formula::DiamondLe(..)
        | Formula::And(_, _) => vec![],
        Formula::Or(phi1, phi2) => vec![vec![phi1.clone()], vec![phi2.clone()]],
        Formula::Imply(phi1, phi2) => vec![vec![phi1.not()], vec![phi2.clone()]],
        Formula::Iff(phi1, phi2) => vec![
            vec![phi1.clone(), phi2.clone()],
            vec![phi1.not(), phi2.not()],
        ],
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
    }
}

fn expand_t(formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
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
