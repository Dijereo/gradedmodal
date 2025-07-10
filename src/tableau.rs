use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    rc::{Rc, Weak},
};

use crate::{formula::Formula, rules::RuleCalc};

pub(crate) struct WorldTableau {
    pub(crate) is_closed: bool,
    pub(crate) root: Rc<RefCell<TableauNode>>,
    pub(crate) transitions: Vec<WorldTableau>,
}

pub(crate) struct TableauNode {
    pub(crate) is_closed: bool,
    pub(crate) formulae: Vec<Rc<Formula>>,
    pub(crate) children: Vec<Rc<RefCell<TableauNode>>>,
    pub(crate) parent: Option<Weak<RefCell<TableauNode>>>,
}

impl WorldTableau {
    pub(crate) fn create<R>(formulae: Vec<Rc<Formula>>, rules: &R) -> Self
    where
        R: RuleCalc,
    {
        let mut tab = Self::from_formulae(formulae);
        if !tab.is_closed {
            rules.apply(&mut tab);
        }
        tab
    }

    fn from_formulae(formulae: Vec<Rc<Formula>>) -> Self {
        let root = TableauNode::from_formulae(formulae, None);
        Self {
            is_closed: root.is_closed,
            root: Rc::new(RefCell::new(root)),
            transitions: vec![],
        }
    }

    fn display_rec(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        if self.is_closed {
            writeln!(f, "{depth}: ⊥")?;
        } else {
            writeln!(f, "{depth}:")?;
        }
        writeln!(f, "{}", self.root.borrow())?;
        for transition in &self.transitions {
            transition.display_rec(f, depth + 1)?;
        }
        Ok(())
    }
}

impl fmt::Display for WorldTableau {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_rec(f, 0)
    }
}

impl TableauNode {
    pub(crate) fn from_formulae(
        formulae: Vec<Rc<Formula>>,
        parent: Option<&Rc<RefCell<TableauNode>>>,
    ) -> Self {
        let mut tab = Self {
            is_closed: false,
            formulae: vec![],
            children: vec![],
            parent: parent.map(Rc::downgrade),
        };
        for new_formula in formulae {
            tab.add_check_dup_contra(new_formula);
        }
        if tab.formulae.is_empty() {
            tab.formulae.push(Rc::new(Formula::Top));
        }
        tab
    }

    pub(crate) fn traverse_anc_formulae(&self, map_while: &mut impl FnMut(&Rc<Formula>) -> bool) {
        for formula in &self.formulae {
            if !map_while(formula) {
                return;
            }
        }
        if let Some(parent) = &self.parent {
            if let Some(parent) = parent.upgrade() {
                parent.borrow().traverse_anc_formulae(map_while);
            }
        }
    }

    fn check_dup(&self, new_formula: &Rc<Formula>) -> bool {
        let mut dup = false;
        self.traverse_anc_formulae(&mut |formula| {
            if new_formula == formula {
                dup = true;
                false
            } else {
                true
            }
        });
        dup
    }

    fn check_contra(&self, new_formula: &Rc<Formula>) -> bool {
        let mut contra = false;
        self.traverse_anc_formulae(&mut |formula| {
            if new_formula.is_negation(formula) {
                contra = true;
                false
            } else {
                true
            }
        });
        contra
    }

    pub(crate) fn add_check_dup_contra(&mut self, new_formula: Rc<Formula>) {
        if !self.check_dup(&new_formula) {
            if new_formula.is_bottom() {
                self.formulae.push(new_formula);
                self.is_closed = true;
            } else if self.check_contra(&new_formula) {
                self.formulae.push(new_formula);
                self.formulae.push(Rc::new(Formula::Bottom));
                self.is_closed = true;
            } else {
                self.formulae.push(new_formula);
            }
        }
    }

    fn get_depths_rec(&self, out: &mut VecDeque<usize>, depth: usize) {
        out.push_back(depth);
        for child in &self.children {
            child.borrow().get_depths_rec(out, depth + 1);
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
        if let Some(formula) = self.formulae.first() {
            writeln!(f, "{formula}")?
        }
        for formula in &self.formulae[1..] {
            if let Some(next_depth) = next_depth {
                if next_depth > 1 {
                    writeln!(
                        f,
                        "{:indent$}|{:width$}{formula}",
                        "",
                        "",
                        indent = next_depth * 2 - 4,
                        width = 2 * (depth + 1 - next_depth) + 1
                    )?
                } else if depth > 0 {
                    writeln!(f, "|{:width$}{formula}", "", width = 2 * depth - 1)?
                } else {
                    writeln!(f, "{formula}")?
                }
            } else {
                writeln!(f, "{:indent$}{formula}", "", indent = 2 * depth)?
            }
        }
        for child in &self.children {
            child.borrow().display_rec(f, depth + 1, next_depths)?
        }
        Ok(())
    }
}

impl fmt::Display for TableauNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut next_depths = VecDeque::new();
        self.get_depths_rec(&mut next_depths, 1);
        next_depths.pop_front();
        self.display_rec(f, 1, &mut next_depths)
    }
}
