use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    rc::{Rc, Weak},
};

use crate::{
    formula::Formula,
    rules3::{Feasibility, GradedTransit},
};

pub(crate) enum TabChildren {
    Fork {
        id: Option<usize>,
        branches: Vec<TabBranch>,
    },
    Transition(GradedTransit),
}

pub(crate) struct TabBranch {
    pub(crate) branchid: usize,
    pub(crate) node: Rc<RefCell<TableauNode2>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Conflict {
    forkid: usize,
    branchid: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct Label {
    pub(crate) formula: Rc<Formula>,
    pub(crate) conflictset: Vec<Conflict>,
}

pub(crate) struct TableauNode2 {
    pub(crate) is_closed: bool,
    pub(crate) formulae: Vec<Label>,
    pub(crate) choices: Vec<(usize, usize)>,
    pub(crate) children: TabChildren,
    pub(crate) parent: Weak<RefCell<TableauNode2>>,
}

impl TableauNode2 {
    pub(crate) fn from_formulae(
        labels: Vec<Label>,
        parent: Option<&Rc<RefCell<TableauNode2>>>,
    ) -> Self {
        let mut tab = Self {
            is_closed: false,
            formulae: vec![],
            choices: vec![],
            children: TabChildren::Fork {
                id: None,
                branches: vec![],
            },
            parent: parent.map_or(Weak::new(), Rc::downgrade),
        };
        for label in labels {
            tab.add_check_dup_contra(label);
        }
        if tab.formulae.is_empty() {
            tab.formulae.push(Label {
                formula: Rc::new(Formula::Top),
                conflictset: vec![],
            });
        }
        tab
    }

    pub(crate) fn traverse_anc_formulae(&self, map_while: &mut impl FnMut(&Label) -> bool) {
        for label in &self.formulae {
            if !map_while(label) {
                return;
            }
        }
        if let Some(parent) = &self.parent.upgrade() {
            parent.borrow().traverse_anc_formulae(map_while);
        }
    }

    fn check_dup(&self, new_formula: &Rc<Formula>) -> Option<Vec<Conflict>> {
        let mut dup = None;
        self.traverse_anc_formulae(&mut |label| {
            if new_formula == &label.formula {
                dup = Some(label.conflictset.clone());
                false
            } else {
                true
            }
        });
        dup
    }

    fn check_contra(&self, new_formula: &Rc<Formula>) -> Option<Vec<Conflict>> {
        let mut conflicts = None;
        self.traverse_anc_formulae(&mut |label| {
            if new_formula.is_negation(&label.formula) {
                conflicts = Some(label.conflictset.clone());
                false
            } else {
                true
            }
        });
        conflicts
    }

    pub(crate) fn check_dup_contra(&mut self, formula: &Rc<Formula>) -> DupContra {
        if let Some(confs) = self.check_dup(formula) {
            DupContra::Dup(confs)
        } else if formula.is_bottom() {
            DupContra::Bottom
        } else if let Some(confs) = self.check_contra(formula) {
            DupContra::Contra(confs)
        } else {
            DupContra::Ok
        }
    }

    pub(crate) fn add_check_dup_contra(&mut self, new_label: Label) -> DupContra {
        if let Some(confs) = self.check_dup(&new_label.formula) {
            DupContra::Dup(confs)
        } else if new_label.formula.is_bottom() {
            self.formulae.push(new_label);
            self.is_closed = true;
            DupContra::Bottom
        } else if let Some(confs) = self.check_contra(&new_label.formula) {
            let mut confs2 = confs.clone();
            confs2.extend(new_label.conflictset.clone());
            self.formulae.push(new_label);
            self.formulae.push(Label {
                formula: Formula::bottom(),
                conflictset: confs2,
            });
            self.is_closed = true;
            DupContra::Contra(confs)
        } else {
            self.formulae.push(new_label);
            DupContra::Ok
        }
    }

    pub(crate) fn get_open_leaves(
        tab: &Rc<RefCell<TableauNode2>>,
        leaves: &mut Vec<Rc<RefCell<TableauNode2>>>,
        include_seeds: bool,
    ) {
        if tab.borrow().is_closed {
            return;
        }
        match &tab.borrow().children {
            TabChildren::Fork { branches, .. } => {
                if branches.len() == 0 {
                    leaves.push(tab.clone());
                }
                for child in branches {
                    Self::get_open_leaves(&child.node, leaves, include_seeds);
                }
            }
            TabChildren::Transition(transit) if include_seeds => match transit.outcome {
                Feasibility::NoTransition | Feasibility::Feasible => leaves.push(tab.clone()),
                Feasibility::Contradiction | Feasibility::NoSolution | Feasibility::Unfeasible => {}
            },
            TabChildren::Transition(_) => {}
        }
    }

    pub(crate) fn get_choices(tab: &Rc<RefCell<TableauNode2>>, choices: &mut Vec<(usize, usize)>) {
        if let Some(parent) = tab.borrow().parent.upgrade() {
            Self::get_choices(&parent, choices);
        }
        choices.extend_from_slice(&tab.borrow().choices)
    }

    fn get_depths_rec(&self, out: &mut VecDeque<usize>, depth: usize) {
        out.push_back(depth);
        match &self.children {
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    branch.node.borrow().get_depths_rec(out, depth + 1);
                }
            }
            TabChildren::Transition(..) => out.push_back(depth + 1),
        }
    }

    pub(crate) fn display_root(
        this: &Rc<RefCell<Self>>,
        f: &mut fmt::Formatter<'_>,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<Self>>)>,
    ) -> fmt::Result {
        let thisref = this.borrow();
        let mut next_depths = VecDeque::new();
        thisref.get_depths_rec(&mut next_depths, 1);
        Self::display_rec(
            this,
            f,
            next_depths
                .pop_front()
                .expect("Should have at least one element"),
            &mut next_depths,
            curri,
            roots,
        )
    }

    fn display_rec(
        this: &Rc<RefCell<Self>>,
        f: &mut fmt::Formatter<'_>,
        depth: usize,
        next_depths: &mut VecDeque<usize>,
        curri: &mut usize,
        seeds: &mut VecDeque<(usize, Rc<RefCell<Self>>)>,
    ) -> fmt::Result {
        let thisref = this.borrow();
        let next_depth = next_depths.pop_front();
        if depth > 1 {
            write!(f, "{:indent$}|-+ ", "", indent = depth * 2 - 4)?
        } else if depth == 1 {
            write!(f, "+ ")?
        }
        if let Some(label) = thisref.formulae.first() {
            writeln!(f, "{}", label.formula)?
        }
        for label in &thisref.formulae[1..] {
            if let Some(next_depth) = next_depth {
                if next_depth > 1 {
                    writeln!(
                        f,
                        "{:indent$}|{:width$}{}",
                        "",
                        "",
                        label.formula,
                        indent = next_depth * 2 - 4,
                        width = 2 * (depth + 1 - next_depth) + 1
                    )?
                } else if depth > 0 {
                    writeln!(f, "|{:width$}{}", "", label.formula, width = 2 * depth - 1)?
                } else {
                    writeln!(f, "{}", label.formula)?
                }
            } else {
                writeln!(f, "{:indent$}{}", "", label.formula, indent = 2 * depth)?
            }
        }
        match &thisref.children {
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    Self::display_rec(&branch.node, f, depth + 1, next_depths, curri, seeds)?
                }
            }
            TabChildren::Transition(transit) => {
                Self::display_transition(f, transit, depth + 1, next_depths, *curri)?;
                seeds.push_back((*curri, this.clone()));
                *curri += 1;
            }
        }
        Ok(())
    }

    fn display_transition(
        f: &mut fmt::Formatter<'_>,
        transit: &GradedTransit,
        depth: usize,
        next_depths: &mut VecDeque<usize>,
        rooti: usize,
    ) -> fmt::Result {
        let _next_depth = next_depths.pop_front();
        let specchar = match transit.outcome {
            Feasibility::NoTransition if transit.is_empty() => return Ok(()),
            Feasibility::NoTransition | Feasibility::Feasible => "✓",
            Feasibility::Contradiction => "⊥",
            Feasibility::NoSolution => "∅",
            Feasibility::Unfeasible => "⨉",
        };
        if depth > 1 {
            writeln!(
                f,
                "{:indent$}|-> {rooti} {specchar}",
                "",
                indent = depth * 2 - 4
            )
        } else {
            writeln!(f, "{rooti} {specchar}")
        }
    }

    fn display_transit(
        transit: &GradedTransit,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<Self>>)>,
    ) -> fmt::Result {
        if transit.is_empty() {
            return Ok(());
        }
        writeln!(f)?;
        writeln!(f, "{rooti}:")?;
        transit.display_modals(f)?;
        match transit.outcome {
            Feasibility::NoTransition => {
                writeln!(f, "No transition needed")?;
                return writeln!(f);
            }
            Feasibility::Feasible => writeln!(f, "Feasible")?,
            Feasibility::Contradiction => writeln!(f, "Contradiction")?,
            Feasibility::NoSolution => writeln!(f, "No Solution")?,
            Feasibility::Unfeasible => writeln!(f, "Unfeasible")?,
        }
        writeln!(f)?;
        let root = if let Some(root) = &transit.root {
            root
        } else {
            return Ok(());
        };
        TableauNode2::display_root(root, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in transit.para_worlds.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        match &transit.solution {
            Some(values) => {
                write!(f, "Solution: ")?;
                for (i, val) in values.iter().enumerate() {
                    write!(f, "{val}*w{i} ")?;
                }
                writeln!(f)?;
            }
            None => writeln!(f, "No solution")?,
        }
        writeln!(f)
    }
}

pub(crate) struct DisplayTableau(pub(crate) Rc<RefCell<TableauNode2>>);

impl fmt::Display for DisplayTableau {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 1;
        let mut seeds = VecDeque::new();
        if self.0.borrow().is_closed {
            writeln!(f, "0: ⊥")?;
        } else {
            writeln!(f, "0:")?;
        }
        TableauNode2::display_root(&self.0, f, &mut i, &mut seeds)?;
        writeln!(f)?;
        writeln!(f)?;
        while let Some((seedi, seed)) = seeds.pop_front() {
            if let TabChildren::Transition(transit) = &seed.borrow().children {
                TableauNode2::display_transit(transit, f, seedi, &mut i, &mut seeds)?;
            }
        }
        Ok(())
    }
}

pub(crate) enum DupContra {
    Ok,
    Bottom,
    Dup(Vec<Conflict>),
    Contra(Vec<Conflict>),
}
