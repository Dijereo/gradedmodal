use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    rc::{Rc, Weak},
};

use crate::formula::Formula;

struct Branch {
    id: usize,
    node: Rc<RefCell<TableauNode2>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Conflict {
    fork: usize,
    branch: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct Label {
    pub(crate) formula: Rc<Formula>,
    pub(crate) conflictset: Vec<Conflict>,
}

pub(crate) struct TableauNode2 {
    pub(crate) is_closed: bool,
    pub(crate) formulae: Vec<Label>,
    pub(crate) forkid: Option<usize>,
    pub(crate) children: Vec<Branch>,
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
            children: vec![],
            parent: parent.map_or(Weak::new(), Rc::downgrade),
            forkid: None,
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

    fn get_depths_rec(&self, out: &mut VecDeque<usize>, depth: usize) {
        out.push_back(depth);
        for child in &self.children {
            child.node.borrow().get_depths_rec(out, depth + 1);
        }
    }

    fn display_rec(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: usize,
        next_depths: &mut VecDeque<usize>,
    ) -> fmt::Result {
        let next_depth = next_depths.pop_front();
        if depth > 1 {
            write!(f, "{:indent$}|-+ ", "", indent = depth * 2 - 4)?
        } else if depth == 1 {
            write!(f, "+ ")?
        }
        if let Some(label) = self.formulae.first() {
            writeln!(f, "{}", label.formula)?
        }
        for label in &self.formulae[1..] {
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
        for child in &self.children {
            child.node.borrow().display_rec(f, depth + 1, next_depths)?
        }
        Ok(())
    }
}

impl fmt::Display for TableauNode2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut next_depths = VecDeque::new();
        self.get_depths_rec(&mut next_depths, 1);
        next_depths.pop_front();
        self.display_rec(f, 1, &mut next_depths)
    }
}

pub(crate) enum DupContra {
    Ok,
    Bottom,
    Dup(Vec<Conflict>),
    Contra(Vec<Conflict>),
}
