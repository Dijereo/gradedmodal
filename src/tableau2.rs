use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    ops::RangeInclusive,
    rc::{Rc, Weak},
};

use crate::{
    formula::Formula,
    rules3::{Feasibility, GradedKCalc, Transit, TransitB5, TransitKOr45},
};

pub(crate) enum TabChildren {
    Leaf,
    Fork { id: usize, branches: Vec<TabBranch> },
    Transition(Feasibility, Transit),
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
    pub(crate) fn from_formulae(labels: Vec<Label>, parent: Option<&Rc<RefCell<Self>>>) -> Self {
        let mut tab = Self {
            is_closed: false,
            formulae: vec![],
            choices: vec![],
            children: TabChildren::Leaf,
            parent: parent.map_or(Weak::new(), Rc::downgrade),
        };
        for label in labels {
            tab.add_check_dup_contra(label);
        }
        if tab.formulae.is_empty() {
            tab.formulae.push(Label {
                formula: Formula::top(),
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
            if let TabChildren::Fork { .. } = parent.borrow().children {
                parent.borrow().traverse_anc_formulae(map_while);
            }
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
            println!("Check contra: {new_formula} vs {}", label.formula);
            if new_formula.is_negation(&label.formula) {
                conflicts = Some(label.conflictset.clone());
                println!("Contra");
                false
            } else {
                println!("Ok");
                true
            }
        });
        conflicts
    }

    pub(crate) fn check_dup_contra(&self, formula: &Rc<Formula>) -> DupContra {
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
        println!("New Formula: {}", new_label.formula);
        if let Some(confs) = self.check_dup(&new_label.formula) {
            println!("Dup");
            DupContra::Dup(confs)
        } else if new_label.formula.is_bottom() {
            self.formulae.push(new_label);
            self.is_closed = true;
            println!("Bottom");
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
            println!("Contra");
            DupContra::Contra(confs)
        } else {
            self.formulae.push(new_label);
            println!("Ok");
            DupContra::Ok
        }
    }

    pub(crate) fn get_open_leaves(
        tab: &Rc<RefCell<Self>>,
        leaves: &mut Vec<Rc<RefCell<Self>>>,
        include_seeds: bool,
    ) {
        if tab.borrow().is_closed {
            return;
        }
        match &tab.borrow().children {
            TabChildren::Leaf => {
                leaves.push(tab.clone());
            }
            TabChildren::Fork { branches, .. } => {
                for child in branches {
                    Self::get_open_leaves(&child.node, leaves, include_seeds);
                }
            }
            TabChildren::Transition(feasibility, _) if include_seeds => match feasibility {
                Feasibility::Feasible => leaves.push(tab.clone()),
                Feasibility::Contradiction | Feasibility::NoSolution | Feasibility::Unfeasible => {}
            },
            TabChildren::Transition(..) => {}
        }
    }

    pub(crate) fn get_choices(
        tab: &Rc<RefCell<Self>>,
        choices: &mut Vec<(usize, usize)>,
        forkid_ranges: &Vec<RangeInclusive<usize>>,
    ) {
        // OPT: bin search + remove
        if let Some(parent) = tab.borrow().parent.upgrade() {
            Self::get_choices(&parent, choices, forkid_ranges);
        }
        choices.extend(
            tab.borrow()
                .choices
                .iter()
                .filter(|(fid, _)| forkid_ranges.iter().any(|r| r.contains(fid))),
        )
    }

    fn get_depths_rec(&self, out: &mut VecDeque<usize>, depth: usize) {
        out.push_back(depth);
        match &self.children {
            TabChildren::Leaf => {}
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
            TabChildren::Leaf => {}
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    Self::display_rec(&branch.node, f, depth + 1, next_depths, curri, seeds)?
                }
            }
            TabChildren::Transition(feasibility, _) => {
                Self::display_transition(f, feasibility, depth + 1, next_depths, *curri)?;
                seeds.push_back((*curri, this.clone()));
                *curri += 1;
            }
        }
        Ok(())
    }

    fn display_transition(
        f: &mut fmt::Formatter<'_>,
        feasiblity: &Feasibility,
        depth: usize,
        next_depths: &mut VecDeque<usize>,
        rooti: usize,
    ) -> fmt::Result {
        let _next_depth = next_depths.pop_front();
        let specchar = match feasiblity {
            Feasibility::Feasible => "✓",
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
}

pub(crate) struct DisplayTableau(pub(crate) Rc<RefCell<TableauNode2>>, pub(crate) GradedKCalc);

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
            if let TabChildren::Transition(feasibility, transit) = &seed.borrow().children {
                transit.display_transit(f, feasibility, &self.1, seedi, &mut i, &mut seeds)?;
            }
        }
        Ok(())
    }
}

impl Transit {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        feasiblity: &Feasibility,
        calc: &GradedKCalc,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2>>)>,
    ) -> fmt::Result {
        match self {
            Transit::KOr45(transit) => {
                transit.display_transit(f, feasiblity, calc, rooti, curri, roots)
            }
            Transit::B5(transit) => {
                transit.display_transit(f, feasiblity, calc, rooti, curri, roots)
            }
        }
    }
}

impl TransitKOr45 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        feasiblity: &Feasibility,
        calc: &GradedKCalc,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}:")?;
        self.modals.display_modals(f, &self.constraints, calc)?;
        match feasiblity {
            Feasibility::Feasible => writeln!(f, "Feasible")?,
            Feasibility::Contradiction => writeln!(f, "Contradiction")?,
            Feasibility::NoSolution => writeln!(f, "No Solution")?,
            Feasibility::Unfeasible => writeln!(f, "Unfeasible")?,
        }
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        match feasiblity {
            Feasibility::Contradiction | Feasibility::Unfeasible | Feasibility::NoSolution => {
                writeln!(f, "No solution")?
            }
            Feasibility::Feasible => {
                write!(f, "Solution: ")?;
                for (i, val) in self.solution.iter().enumerate() {
                    write!(f, "{val}*w{i} ")?;
                }
                writeln!(f)?;
            }
        }
        writeln!(f)
    }
}

impl TransitB5 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        feasiblity: &Feasibility,
        calc: &GradedKCalc,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}:")?;
        self.modals.display_modals(f, &self.constraints, calc)?;
        match feasiblity {
            Feasibility::Feasible => writeln!(f, "Feasible")?,
            Feasibility::Contradiction => writeln!(f, "Contradiction")?,
            Feasibility::NoSolution => writeln!(f, "No Solution")?,
            Feasibility::Unfeasible => writeln!(f, "Unfeasible")?,
        }
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.reflexion.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        for (i, choice) in self.reflexion.choices.iter().enumerate() {
            write!(f, "u{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        match feasiblity {
            Feasibility::Contradiction | Feasibility::Unfeasible | Feasibility::NoSolution => {
                writeln!(f, "No solution")?
            }
            Feasibility::Feasible => {
                write!(f, "Solution: ")?;
                for (i, val) in self.solution.iter().enumerate() {
                    if i == self.rfxsolution {
                        write!(f, "{val}*w{i}+u ")?;
                    } else {
                        write!(f, "{val}*w{i} ")?;
                    }
                }
                writeln!(f)?;
            }
        }
        writeln!(f)
    }
}

pub(crate) enum DupContra {
    Ok,
    Bottom,
    Dup(Vec<Conflict>),
    Contra(Vec<Conflict>),
}
