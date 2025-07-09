use std::{collections::VecDeque, fmt, rc::Rc};

use crate::{
    formula::Formula,
    rules::{RuleCalculus, SequenceRule, SplitRule, TransitionRule},
};

pub(crate) struct ModalTableau {
    is_closed: bool,
    world: WorldTableau,
    transitions: Vec<ModalTableau>,
}

struct WorldTableau {
    is_closed: bool,
    formulae: Vec<Rc<Formula>>,
    children: Vec<WorldTableau>,
}

impl ModalTableau {
    pub(crate) fn create(formulae: Vec<Rc<Formula>>, rules: &RuleCalculus) -> Self {
        let mut tab = Self::from_formulae(formulae);
        if !tab.is_closed {
            tab.world
                .process_static(rules.seq, rules.split, &mut vec![], VecDeque::new());
            tab.is_closed = tab.world.is_closed;
        }
        if !tab.is_closed {
            tab.process_transitions(rules);
        }
        tab
    }

    fn from_formulae(formulae: Vec<Rc<Formula>>) -> Self {
        let world = WorldTableau::from_formulae(formulae, &vec![]);
        Self {
            is_closed: world.is_closed,
            world,
            transitions: vec![],
        }
    }

    fn process_transitions(&mut self, rules: &RuleCalculus) {
        if self.world.is_closed {
            self.is_closed = true;
            return;
        }
        let mut trans_worlds = vec![];
        for rule in rules.trans {
            trans_worlds.extend(self.world.process_transitions(*rule, &mut vec![]));
        }
        for mut trans_world in trans_worlds {
            trans_world.process_static(rules.seq, rules.split, &mut vec![], VecDeque::new());
            let mut trans_tab = ModalTableau {
                is_closed: trans_world.is_closed,
                world: trans_world,
                transitions: vec![],
            };
            if !trans_tab.is_closed {
                trans_tab.process_transitions(rules);
            }
            self.transitions.push(trans_tab);
        }
        if !self.transitions.is_empty() && self.transitions.iter().all(|tw| tw.is_closed) {
            self.is_closed = true;
        }
    }

    fn display_rec(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        if self.is_closed {
            writeln!(f, "{depth}: ⊥")?;
        } else {
            writeln!(f, "{depth}:")?;
        }
        writeln!(f, "{}", self.world)?;
        for transition in &self.transitions {
            transition.display_rec(f, depth + 1)?;
        }
        Ok(())
    }
}

impl fmt::Display for ModalTableau {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_rec(f, 0)
    }
}

type Split = Vec<Vec<Rc<Formula>>>;

impl WorldTableau {
    fn from_formulae(formulae: Vec<Rc<Formula>>, ancestor_formulae: &[Vec<Rc<Formula>>]) -> Self {
        let mut tab = Self {
            is_closed: false,
            formulae: vec![],
            children: vec![],
        };
        for new_formula in formulae {
            tab.add_dedup_check_contra(new_formula, ancestor_formulae);
        }
        if tab.formulae.is_empty() {
            tab.formulae.push(Rc::new(Formula::Top));
        }
        tab
    }

    fn process_static(
        &mut self,
        seq_rules: &[&dyn SequenceRule],
        split_rules: &[&dyn SplitRule],
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
        mut splits: VecDeque<Split>,
    ) {
        self.process_sequence(seq_rules, ancestor_formulae);
        if !self.is_closed {
            splits.extend(self.get_new_splits(split_rules));
            self.process_splits(seq_rules, split_rules, ancestor_formulae, splits)
        }
    }

    fn process_sequence(
        &mut self,
        rules: &[&dyn SequenceRule],
        ancestor_formulae: &[Vec<Rc<Formula>>],
    ) {
        let mut i = 0;
        while let Some(formula) = self.formulae.get(i) {
            let mut new_formulae = vec![];
            for rule in rules {
                new_formulae.extend(rule.expand(&formula));
            }
            for new_formula in new_formulae {
                self.add_dedup_check_contra(new_formula, ancestor_formulae);
                if self.is_closed {
                    return;
                }
            }
            i += 1;
        }
    }

    fn get_new_splits(&self, rules: &[&dyn SplitRule]) -> Vec<Vec<Vec<Rc<Formula>>>> {
        let mut out = vec![];
        for formula in &self.formulae {
            for rule in rules {
                let split = rule.expand(formula);
                if !split.is_empty() {
                    out.push(split);
                }
            }
        }
        out
    }

    fn process_splits(
        &mut self,
        seq_rules: &[&dyn SequenceRule],
        split_rules: &[&dyn SplitRule],
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
        mut splits: VecDeque<Split>,
    ) {
        ancestor_formulae.push(self.formulae.clone());
        if self.children.is_empty() {
            if let Some(split) = splits.pop_front() {
                for branch in split {
                    let child = Self::from_formulae(branch, ancestor_formulae);
                    self.children.push(child);
                }
            } else {
                return;
            }
        }
        let mut all_closed = true;
        for child in &mut self.children {
            if !child.is_closed {
                child.process_static(seq_rules, split_rules, ancestor_formulae, splits.clone());
                if !child.is_closed {
                    all_closed = false;
                }
            }
        }
        if all_closed {
            self.is_closed = true;
        }
        ancestor_formulae.pop();
    }

    fn add_dedup_check_contra(
        &mut self,
        new_formula: Rc<Formula>,
        ancestor_formulae: &[Vec<Rc<Formula>>],
    ) {
        if new_formula.is_bottom() {
            self.is_closed = true;
        }
        let mut add_bottom = false;
        for ancestor in ancestor_formulae {
            for anc_formula in ancestor {
                if &new_formula == anc_formula {
                    return;
                }
                if new_formula.is_negation(anc_formula) {
                    self.is_closed = true;
                    add_bottom = true;
                }
            }
        }
        for self_formula in &self.formulae {
            if &new_formula == self_formula {
                return;
            }
            if new_formula.is_negation(self_formula) {
                self.is_closed = true;
                add_bottom = true;
            }
        }
        self.formulae.push(new_formula);
        if add_bottom {
            for ancestor in ancestor_formulae {
                for anc_formula in ancestor {
                    if anc_formula.is_bottom() {
                        return;
                    }
                }
            }
            for self_formula in &self.formulae {
                if self_formula.is_bottom() {
                    return;
                }
            }
            self.formulae.push(Rc::new(Formula::Bottom));
        }
    }

    fn process_transitions(
        &self,
        rule: &dyn TransitionRule,
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
    ) -> Vec<WorldTableau> {
        let mut trans_worlds = vec![];
        for formula in &self.formulae {
            let mut trans_formulae = rule.try_apply(formula);
            if trans_formulae.is_empty() {
                continue;
            }
            for anc_formulae in ancestor_formulae.iter() {
                for anc_formula in anc_formulae {
                    trans_formulae.extend(rule.transition(anc_formula));
                }
            }
            for other_formula in &self.formulae {
                trans_formulae.extend(rule.transition(other_formula));
            }
            let mut trans_world = WorldTableau::from_formulae(trans_formulae, &vec![]);
            if !trans_world.is_closed {
                let mut trans_anc_formulae = vec![trans_world.formulae.clone()];
                let mut closed = true;
                for child in &self.children {
                    if child.is_closed {
                        continue;
                    }
                    let trans_child = child.process_transition_rec(rule, &mut trans_anc_formulae);
                    closed &= trans_child.is_closed;
                    trans_world.children.push(trans_child);
                }
                if closed && !self.children.is_empty() {
                    trans_world.is_closed;
                }
            }
            trans_worlds.push(trans_world);
        }
        ancestor_formulae.push(self.formulae.clone());
        for child in &self.children {
            if !child.is_closed {
                trans_worlds.extend(child.process_transitions(rule, ancestor_formulae));
            }
        }
        ancestor_formulae.pop();
        trans_worlds
    }

    fn process_transition_rec(
        &self,
        rule: &dyn TransitionRule,
        trans_anc_formulae: &mut Vec<Vec<Rc<Formula>>>,
    ) -> Self {
        let mut trans_formulae = vec![];
        for formula in &self.formulae {
            trans_formulae.extend(rule.transition(formula));
        }
        if trans_formulae.is_empty() {
            trans_formulae.push(Rc::new(Formula::Top));
        }
        let mut trans_tab = Self::from_formulae(trans_formulae, trans_anc_formulae);
        if !trans_tab.is_closed {
            trans_anc_formulae.push(trans_tab.formulae.clone());
            let mut closed = true;
            for child in &self.children {
                if child.is_closed {
                    continue;
                }
                let trans_child = child.process_transition_rec(rule, trans_anc_formulae);
                closed &= trans_child.is_closed;
                trans_tab.children.push(trans_child);
            }
            if closed && !self.children.is_empty() {
                trans_tab.is_closed = true;
            }
            trans_anc_formulae.pop();
        }
        trans_tab
    }

    fn get_depths_rec(&self, out: &mut VecDeque<usize>, depth: usize) {
        out.push_back(depth);
        for child in &self.children {
            child.get_depths_rec(out, depth + 1);
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
            child.display_rec(f, depth + 1, next_depths)?
        }
        Ok(())
    }
}

impl fmt::Display for WorldTableau {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut next_depths = VecDeque::new();
        self.get_depths_rec(&mut next_depths, 1);
        next_depths.pop_front();
        self.display_rec(f, 1, &mut next_depths)
    }
}
