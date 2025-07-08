use std::{collections::VecDeque, fmt, mem::take, rc::Rc};

use crate::formula::Formula;

pub(crate) struct ModalTableau {
    is_open: Option<bool>,
    world: WorldTableau,
    transitions: Vec<ModalTableau>,
}

struct WorldTableau {
    is_open: Option<bool>,
    formulae: Vec<Rc<Formula>>,
    children: Vec<WorldTableau>,
}

trait Rule {
    fn expand_sequence(&self, formula: &Formula) -> Vec<Rc<Formula>>;
    fn expand_split(&self, formula: &Formula) -> Vec<Vec<Rc<Formula>>>;
}

trait TransitionRule {
    fn try_apply(&self, formula: &Formula) -> Vec<Rc<Formula>>;
    fn transition(&self, formula: &Formula) -> Vec<Rc<Formula>>;
}

impl ModalTableau {
    pub(crate) fn create(formulae: Vec<Rc<Formula>>) -> Self {
        let mut tab = Self::from_formulae(formulae);
        let propcalc = PropTabCalc;
        let rule = TRule;
        let out = tab
            .world
            .process_closed(&[&PropTabCalc, &TRule], &mut vec![], VecDeque::new());
        tab.process_transitions(&[KRule], &[&PropTabCalc, &TRule]);
        tab
    }

    fn from_formulae(formulae: Vec<Rc<Formula>>) -> Self {
        Self {
            is_open: None,
            world: WorldTableau::from_formulae(formulae, &vec![]),
            transitions: vec![],
        }
    }

    fn process_transitions(&mut self, trans_rules: &[impl TransitionRule], rules: &[&dyn Rule]) {
        let mut worlds = vec![];
        for rule in trans_rules {
            worlds.extend(self.world.process_transitions(rule, &mut vec![]));
        }
        for mut world in worlds {
            world.process_closed(rules, &mut vec![], VecDeque::new());
            let mut tab = ModalTableau {
                is_open: None,
                world,
                transitions: vec![],
            };
            tab.process_transitions(trans_rules, rules);
            self.transitions.push(tab);
        }
    }

    fn display_rec(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        writeln!(f, "{depth}:")?;
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
            is_open: None,
            formulae: vec![],
            children: vec![],
        };
        for new_formula in formulae {
            let _ = tab.add_check_contra(new_formula, ancestor_formulae);
        }
        tab
    }

    fn process_closed(
        &mut self,
        rules: &[&dyn Rule],
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
        mut splits: VecDeque<Split>,
    ) -> bool {
        if self.process_sequence_closed(rules, ancestor_formulae) {
            true
        } else {
            let new_splits = self.get_new_splits(rules);
            splits.extend(new_splits);
            self.process_split_closed(rules, ancestor_formulae, splits)
        }
    }

    fn process_sequence_closed(
        &mut self,
        rules: &[&dyn Rule],
        ancestor_formulae: &[Vec<Rc<Formula>>],
    ) -> bool {
        let mut i = 0;
        while let Some(formula) = self.formulae.get(i) {
            let mut new_formulae = vec![];
            for rule in rules {
                new_formulae.append(&mut rule.expand_sequence(&formula));
            }
            for new_formula in new_formulae {
                if self.add_check_contra(new_formula, ancestor_formulae) {
                    return true;
                }
            }
            i += 1;
        }
        false
    }

    fn get_new_splits(&mut self, rules: &[&dyn Rule]) -> Vec<Vec<Vec<Rc<Formula>>>> {
        let mut out = vec![];
        for formula in &self.formulae {
            for rule in rules {
                let split = rule.expand_split(formula);
                if !split.is_empty() {
                    out.push(split);
                }
            }
        }
        out
    }

    fn process_split_closed<'a>(
        &mut self,
        rules: &[&dyn Rule],
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
        mut splits: VecDeque<Split>,
    ) -> bool {
        if self.children.is_empty() {
            if let Some(split) = splits.pop_front() {
                let mut closed = true;
                for branch in split {
                    ancestor_formulae.push(self.formulae.clone());
                    let mut child = Self::from_formulae(branch.clone(), ancestor_formulae);
                    if child.is_open.is_none() {
                        closed &= child.process_closed(rules, ancestor_formulae, splits.clone());
                    }
                    self.children.push(child);
                    ancestor_formulae.pop();
                }
                closed
            } else {
                false
            }
        } else {
            let mut closed = true;
            for child in &mut self.children {
                ancestor_formulae.push(self.formulae.clone());
                if child.is_open.unwrap_or(true) {
                    closed &= child.process_closed(rules, ancestor_formulae, splits.clone());
                }
                ancestor_formulae.pop();
            }
            closed
        }
    }

    fn add_check_contra(
        &mut self,
        new_formula: Rc<Formula>,
        ancestor_formulae: &[Vec<Rc<Formula>>],
    ) -> bool {
        self.formulae.push(new_formula);
        let new_formula = self.formulae.last().expect("Just pushed");
        if let Formula::Bottom = new_formula.as_ref() {
            self.close();
            return true;
        }
        for ancestor in ancestor_formulae {
            for anc_formula in ancestor {
                if new_formula.is_negation(anc_formula) {
                    self.formulae.push(Rc::new(Formula::Bottom));
                    self.close();
                    return true;
                }
            }
        }
        for self_formula in &self.formulae {
            if new_formula.is_negation(self_formula) {
                self.formulae.push(Rc::new(Formula::Bottom));
                self.close();
                return true;
            }
        }
        false
    }

    fn close(&mut self) {
        self.is_open = Some(false);
        for child in &mut self.children {
            child.close();
        }
    }

    fn process_transitions(
        &self,
        rule: &impl TransitionRule,
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
    ) -> Vec<WorldTableau> {
        let mut trans_worlds = vec![];
        for formula in &self.formulae {
            let out = rule.try_apply(formula);
            if !out.is_empty() {
                let mut trans_formulae = out;
                for anc_formulae in ancestor_formulae.iter() {
                    for anc_formula in anc_formulae {
                        trans_formulae.extend(rule.transition(anc_formula));
                    }
                }
                for other_formula in &self.formulae {
                    trans_formulae.extend(rule.transition(other_formula));
                }
                let mut trans_world = WorldTableau::from_formulae(trans_formulae, &vec![]);
                if trans_world.is_open.unwrap_or(true) {
                    let mut trans_anc_formulae = vec![trans_world.formulae.clone()];
                    let mut closed = true;
                    for child in &self.children {
                        let trans_child =
                            child.process_transition_rec(rule, &mut trans_anc_formulae);
                        closed &= !trans_child.is_open.unwrap_or(true);
                        trans_world.children.push(trans_child);
                    }
                    if closed && !self.children.is_empty() {
                        trans_world.is_open = Some(false);
                    }
                }
                trans_worlds.push(trans_world);
            }
        }
        ancestor_formulae.push(self.formulae.clone());
        for child in &self.children {
            trans_worlds.extend(child.process_transitions(rule, ancestor_formulae));
        }
        ancestor_formulae.pop();
        trans_worlds
    }

    fn process_transition_rec(
        &self,
        rule: &impl TransitionRule,
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
    ) -> Self {
        let mut trans_formulae = vec![];
        for formula in &self.formulae {
            trans_formulae.extend(rule.transition(formula));
        }
        if trans_formulae.is_empty() {
            trans_formulae.push(Rc::new(Formula::Not(Rc::new(Formula::Bottom))));
        }
        let mut trans_tab = Self::from_formulae(trans_formulae, ancestor_formulae);
        if trans_tab.is_open.unwrap_or(true) {
            ancestor_formulae.push(trans_tab.formulae.clone());
            let mut closed = true;
            for child in &self.children {
                let trans_child = child.process_transition_rec(rule, ancestor_formulae);
                closed &= !trans_child.is_open.unwrap_or(true);
                trans_tab.children.push(trans_child);
            }
            if closed && !self.children.is_empty() {
                trans_tab.is_open = Some(false);
            }
            ancestor_formulae.pop();
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

struct PropTabCalc;

impl Rule for PropTabCalc {
    fn expand_sequence(&self, formula: &Formula) -> Vec<Rc<Formula>> {
        match formula {
            Formula::Bottom
            | Formula::PropVar(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::Or(_, _)
            | Formula::Imply(_, _)
            | Formula::Iff(_, _) => vec![],
            Formula::Not(phi) => Self::expand_seq_not(phi),
            Formula::And(phi1, phi2) => {
                vec![phi1.clone(), phi2.clone()]
            }
        }
    }

    fn expand_split(&self, formula: &Formula) -> Vec<Vec<Rc<Formula>>> {
        match formula {
            Formula::Bottom
            | Formula::PropVar(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::And(_, _) => vec![],
            Formula::Not(phi) => Self::expand_split_not(phi),
            Formula::Or(phi1, phi2) => {
                vec![vec![phi1.clone()], vec![phi2.clone()]]
            }
            Formula::Imply(phi1, phi2) => vec![
                vec![Rc::new(Formula::Not(phi1.clone()))],
                vec![phi2.clone()],
            ],
            Formula::Iff(phi1, phi2) => vec![
                vec![phi1.clone(), phi2.clone()],
                vec![
                    Rc::new(Formula::Not(phi1.clone())),
                    Rc::new(Formula::Not(phi2.clone())),
                ],
            ],
        }
    }
}

impl PropTabCalc {
    fn expand_seq_not(phi: &Formula) -> Vec<Rc<Formula>> {
        match phi {
            Formula::Bottom | Formula::PropVar(_) | Formula::And(_, _) | Formula::Iff(_, _) => {
                vec![]
            }
            Formula::Not(psi) => vec![psi.clone()],
            Formula::Box(psi) => vec![Rc::new(Formula::Diamond(Rc::new(Formula::Not(
                psi.clone(),
            ))))],
            Formula::Diamond(psi) => {
                vec![Rc::new(Formula::Box(Rc::new(Formula::Not(psi.clone()))))]
            }
            Formula::Or(psi1, psi2) => vec![Rc::new(Formula::And(
                Rc::new(Formula::Not(psi1.clone())),
                Rc::new(Formula::Not(psi2.clone())),
            ))],
            Formula::Imply(psi1, psi2) => vec![psi1.clone(), Rc::new(Formula::Not(psi2.clone()))],
        }
    }

    fn expand_split_not(phi: &Formula) -> Vec<Vec<Rc<Formula>>> {
        match phi {
            Formula::Bottom
            | Formula::PropVar(_)
            | Formula::Not(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::Or(_, _)
            | Formula::Imply(_, _) => vec![],
            Formula::And(psi1, psi2) => vec![
                vec![Rc::new(Formula::Not(psi1.clone()))],
                vec![Rc::new(Formula::Not(psi2.clone()))],
            ],
            Formula::Iff(psi1, psi2) => vec![
                vec![psi1.clone(), Rc::new(Formula::Not(psi2.clone()))],
                vec![Rc::new(Formula::Not(psi1.clone())), psi2.clone()],
            ],
        }
    }
}

struct TRule;

impl Rule for TRule {
    fn expand_sequence(&self, formula: &Formula) -> Vec<Rc<Formula>> {
        match formula {
            Formula::Box(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn expand_split(&self, formula: &Formula) -> Vec<Vec<Rc<Formula>>> {
        vec![]
    }
}

struct DRule;

impl Rule for DRule {
    fn expand_sequence(&self, formula: &Formula) -> Vec<Rc<Formula>> {
        match formula {
            Formula::Box(phi) => vec![Rc::new(Formula::Diamond(phi.clone()))],
            _ => vec![],
        }
    }

    fn expand_split(&self, formula: &Formula) -> Vec<Vec<Rc<Formula>>> {
        vec![]
    }
}

struct KRule;

impl TransitionRule for KRule {
    fn try_apply(&self, formula: &Formula) -> Vec<Rc<Formula>> {
        match formula {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition(&self, formula: &Formula) -> Vec<Rc<Formula>> {
        match formula {
            Formula::Box(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }
}
