// use std::{
//     fmt::{self, write},
//     rc::Rc,
// };

// use crate::{
//     formula::Formula,
//     parser::{parse_eq, parse_fst, parse_snd},
//     token::Token,
// };

// #[derive(Clone, Debug)]
// struct NNFFormula {
//     depth: usize,
//     formula: FormulaInner,
// }

// #[derive(Clone, Debug)]
// enum FormulaInner {
//     Or(Or),
//     And(And),
//     Atom(Atom),
// }

// #[derive(Clone, Debug)]
// struct Or {
//     phi0: Conj,
//     phi1: Conj,
//     phis: Vec<Conj>,
// }

// #[derive(Clone, Debug)]
// enum Conj {
//     And(And),
//     Atom(Atom),
// }

// #[derive(Clone, Debug)]
// struct And {
//     phi0: Disj,
//     phi1: Disj,
//     phis: Vec<Disj>,
// }

// #[derive(Clone, Debug)]
// enum Disj {
//     Or(Rc<Or>),
//     Atom(Atom),
// }

// #[derive(Clone, Debug)]
// enum Atom {
//     Bottom,
//     Top,
//     Var(bool, char),
//     VarIdx(bool, char, usize),
//     Modal(Modal, Rc<NNFFormula>),
// }

// #[derive(Clone, Debug, PartialEq)]
// enum Modal {
//     Box,
//     Diamond,
//     DiamondGe(u32),
//     DiamondLe(u32),
// }

// impl From<Rc<Formula>> for NNFFormula {
//     fn from(value: Rc<Formula>) -> Self {
//         match *value.as_ref() {}
//     }
// }

// impl FormulaInner {
//     fn from_get_depth(value: &Rc<Formula>) -> (Self, usize) {
//         match value.as_ref() {
//             Formula::Bottom => (FormulaInner::Atom(Atom::Bottom), 0),
//             Formula::Top => (FormulaInner::Atom(Atom::Top), 0),
//             Formula::PropVar(p, Some(i)) => {
//                 (FormulaInner::Atom(Atom::VarIdx(true, *p, *i as usize)), 0)
//             }
//             Formula::PropVar(p, None) => (FormulaInner::Atom(Atom::Var(true, p)), 0),
//             Formula::Not(phi) => {}
//             Formula::Box(phi) => {
//                 let psi = Rc::new(NNFFormula::from(phi));
//                 Self {
//                     depth: psi.depth + 1,
//                     formula: FormulaInner::Atom(Atom::Modal(Modal::Box, psi)),
//                 }
//             }
//             Formula::Diamond(phi) => {
//                 let psi = Rc::new(NNFFormula::from(phi));
//                 Self {
//                     depth: psi.depth + 1,
//                     formula: FormulaInner::Atom(Atom::Modal(Modal::Diamond, psi)),
//                 }
//             }
//             Formula::DiamondGe(c, phi) => {
//                 let psi = Rc::new(NNFFormula::from(phi));
//                 Self {
//                     depth: psi.depth + 1,
//                     formula: FormulaInner::Atom(Atom::Modal(Modal::Diamond, psi)),
//                 }
//             }
//             Formula::DiamondLe(c, phi) => {
//                 let psi = Rc::new(NNFFormula::from(phi));
//                 Self {
//                     depth: psi.depth + 1,
//                     formula: FormulaInner::Atom(Atom::Modal(Modal::Diamond, psi)),
//                 }
//             }
//             Formula::And(phi0, phi1) => {
//                 let psi0 = FormulaInner::from(phi0);
//                 let psi1 = FormulaInner::from(phi1);
//                 match (psi0, psi1) {
//                     (FormulaInner::And(theta), FormulaInner::And(heta)) => {}
//                     (FormulaInner::Or(or), FormulaInner::Or(or)) => todo!(),
//                     (FormulaInner::Or(or), FormulaInner::And(and)) => todo!(),
//                     (FormulaInner::Or(or), FormulaInner::Atom(atom)) => todo!(),
//                     (FormulaInner::And(and), FormulaInner::Or(or)) => todo!(),
//                     (FormulaInner::And(and), FormulaInner::Atom(atom)) => todo!(),
//                     (FormulaInner::Atom(atom), FormulaInner::Or(or)) => todo!(),
//                     (FormulaInner::Atom(atom), FormulaInner::And(and)) => todo!(),
//                     (FormulaInner::Atom(atom), FormulaInner::Atom(atom)) => todo!(),
//                 }
//                 todo!()
//             }
//             Formula::Or(phi0, phi1) => todo!(),
//             Formula::Imply(phi0, phi1) => todo!(),
//             Formula::Iff(phi0, phi1) => todo!(),
//         }
//     }

//     fn from_not_get_depth(formula: &Rc<Formula>) -> (Self, usize) {
//         match formula.as_ref() {
//             Formula::Bottom => (FormulaInner::Atom(Atom::Top), 0),
//             Formula::Top => (FormulaInner::Atom(Atom::Bottom), 0),
//             Formula::PropVar(p, Some(i)) => {
//                 (FormulaInner::Atom(Atom::VarIdx(false, *p, *i as usize)), 0)
//             }
//             Formula::PropVar(p, None) => (FormulaInner::Atom(Atom::Var(false, *p)), 0),
//             Formula::Not(phi) => FormulaInner::from_get_depth(phi),
//             Formula::Box(phi) => {
//                 let (phi1, depth) = Self::from_not_get_depth(phi);
//                 let phi1 = Rc::new(NNFFormula {
//                     depth,
//                     formula: phi1,
//                 });
//                 (FormulaInner::Atom(Atom::Modal(Modal::Diamond, phi1)), depth)
//             }
//             Formula::Diamond(formula) => todo!(),
//             Formula::DiamondGe(_, formula) => todo!(),
//             Formula::DiamondLe(_, formula) => todo!(),
//             Formula::And(formula, formula1) => todo!(),
//             Formula::Or(formula, formula1) => todo!(),
//             Formula::Imply(formula, formula1) => todo!(),
//             Formula::Iff(formula, formula1) => todo!(),
//         }
//     }
// }

// impl fmt::Display for NNFFormula {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.formula)
//     }
// }

// impl fmt::Display for FormulaInner {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             FormulaInner::Or(disjunction) => write!(f, "{}", disjunction),
//             FormulaInner::And(conjunction) => write!(f, "{}", conjunction),
//             FormulaInner::Atom(atom) => write!(f, "{}", atom),
//         }
//     }
// }

// impl fmt::Display for Or {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{} ∨ {}", self.phi0, self.phi1)?;
//         for phi in &self.phis {
//             write!(f, " ∨ {}", phi)?;
//         }
//         Ok(())
//     }
// }

// impl fmt::Display for Conj {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Conj::And(and) => write!(f, "{and}"),
//             Conj::Atom(atom) => write!(f, "{atom}"),
//         }
//     }
// }

// impl fmt::Display for And {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{} ∧ {}", self.phi0, self.phi1)?;
//         for phi in &self.phis {
//             write!(f, " ∧ {}", phi)?;
//         }
//         Ok(())
//     }
// }

// impl fmt::Display for Disj {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Disj::Or(or) => write!(f, "{or}"),
//             Disj::Atom(atom) => write!(f, "{atom}"),
//         }
//     }
// }

// impl fmt::Display for Modal {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Modal::Box => write!(f, "□"),
//             Modal::Diamond => write!(f, "◇"),
//             Modal::DiamondGe(c) => write!(f, "◇≥{c}"),
//             Modal::DiamondLe(c) => write!(f, "◇≤{c}"),
//         }
//     }
// }

// impl fmt::Display for Atom {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Atom::Bottom => write!(f, "⊥"),
//             Atom::Top => write!(f, "⊤"),
//             Atom::Var(sign, p) => write!(f, "{}{p}", if *sign { "" } else { "¬" }),
//             Atom::VarIdx(sign, p, i) => write!(f, "{}{p}{i}", if *sign { "" } else { "¬" }),
//             Atom::Modal(modal, formula) => match (modal, &formula.formula) {
//                 (_, FormulaInner::Or(_)) | (_, FormulaInner::And(_)) => {
//                     write!(f, "{modal}({formula})")
//                 }
//                 (_, FormulaInner::Atom(Atom::Modal(..))) => write!(f, "{modal}{formula}"),
//                 (Modal::Box, _) | (Modal::Diamond, _) => {
//                     write!(f, "{modal} {formula}")
//                 }
//                 (Modal::DiamondGe(_), _) | (Modal::DiamondLe(_), _) => {
//                     write!(f, "{modal} {formula}")
//                 }
//             },
//         }
//     }
// }
