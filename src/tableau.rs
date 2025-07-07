use std::{fmt, rc::Rc};

use crate::formula::Formula;

type Vec1<T> = (T, Vec<T>);
type Vec2<T> = (T, T, Vec<T>);

struct ModalTableau {
    is_open: Option<bool>,
    world: WorldTableau,
    transitions: Vec<ModalTableau>,
}

struct WorldTableau {
    is_open: Option<bool>,
    formulae: Vec<Formula>,
    children: Vec<WorldTableau>,
}

impl ModalTableau {
    fn from_formulae(formulae: Vec<Formula>) -> Self {
        Self {
            is_open: None,
            world: WorldTableau::from_formulae(formulae),
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

impl WorldTableau {
    fn from_formulae(formulae: Vec<Formula>) -> Self {
        Self {
            is_open: None,
            formulae,
            children: vec![],
        }
    }

    fn display_rec(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        if indent > 0 {
            write!(f, "{:width$}", "", width = 2 * indent - 2)?;
            write!(f, "+ ")?
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

impl PropTabCalc {
    fn expand_sequence(formula: &Formula) -> Vec<Rc<Formula>> {
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
    fn expand_split(formula: &Formula) -> Option<Vec2<Vec1<Rc<Formula>>>> {
        match formula {
            Formula::Bottom
            | Formula::PropVar(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::And(_, _) => None,
            Formula::Not(phi) => Self::expand_split_not(phi),
            Formula::Or(phi1, phi2) => {
                Some(((phi1.clone(), vec![]), (phi2.clone(), vec![]), vec![]))
            }
            Formula::Imply(phi1, phi2) => Some((
                (Rc::new(Formula::Not(phi1.clone())), vec![]),
                (phi2.clone(), vec![]),
                vec![],
            )),
            Formula::Iff(phi1, phi2) => Some((
                (phi1.clone(), vec![phi2.clone()]),
                (
                    Rc::new(Formula::Not(phi1.clone())),
                    vec![Rc::new(Formula::Not(phi2.clone()))],
                ),
                vec![],
            )),
        }
    }

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

    fn expand_split_not(phi: &Formula) -> Option<Vec2<Vec1<Rc<Formula>>>> {
        match phi {
            Formula::Bottom
            | Formula::PropVar(_)
            | Formula::Not(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::Or(_, _)
            | Formula::Imply(_, _) => None,
            Formula::And(psi1, psi2) => Some((
                (Rc::new(Formula::Not(psi1.clone())), vec![]),
                (Rc::new(Formula::Not(psi2.clone())), vec![]),
                vec![],
            )),
            Formula::Iff(psi1, psi2) => Some((
                (psi1.clone(), vec![Rc::new(Formula::Not(psi2.clone()))]),
                (Rc::new(Formula::Not(psi1.clone())), vec![psi2.clone()]),
                vec![],
            )),
        }
    }
}
