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

impl ModalTableau {
    pub(crate) fn create(formulae: Vec<Rc<Formula>>) -> Self {
        let mut tab = Self::from_formulae(formulae);
        let out = tab
            .world
            .process_closed(&[PropTabCalc], &mut vec![], VecDeque::new());
        tab
    }

    fn from_formulae(formulae: Vec<Rc<Formula>>) -> Self {
        Self {
            is_open: None,
            world: WorldTableau::from_formulae(formulae, &vec![]),
            transitions: vec![],
        }
    }

    fn display_rec(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        writeln!(f, "{depth}>")?;
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

    fn process_closed<'a, 'b>(
        &mut self,
        rules: &[impl Rule],
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
        mut splits: VecDeque<Split>,
    ) -> bool {
        println!("a");
        if self.process_sequence_closed(rules, ancestor_formulae) {
            println!("b");
            true
        } else {
            println!("c");
            let new_splits = self.get_new_splits(rules);
            println!("d");
            splits.extend(new_splits);
            println!("e");
            self.process_split_closed(rules, ancestor_formulae, splits)
        }
    }

    fn process_sequence_closed(
        &mut self,
        rules: &[impl Rule],
        ancestor_formulae: &Vec<Vec<Rc<Formula>>>,
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

    fn get_new_splits(&mut self, rules: &[impl Rule]) -> Vec<Vec<Vec<Rc<Formula>>>> {
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
        rules: &[impl Rule],
        ancestor_formulae: &mut Vec<Vec<Rc<Formula>>>,
        mut splits: VecDeque<Split>,
    ) -> bool {
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

    fn display_rec(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        if indent > 0 {
            write!(f, "{:width$}", "", width = 2 * indent - 2)?;
            if self.is_open.unwrap_or(true) {
                write!(f, "+ ")?
            } else {
                write!(f, "- ")?
            }
        }
        if let Some(formula) = self.formulae.first() {
            writeln!(f, "{formula}")?
        }
        for formula in &self.formulae[1..] {
            writeln!(f, "{:width$}{formula}", "", width = indent * 2)?
        }
        for child in &self.children {
            child.display_rec(f, indent + 1)?
        }
        Ok(())
    }
}

impl fmt::Display for WorldTableau {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_rec(f, 0)
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
