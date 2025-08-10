use std::{cmp::max, mem, rc::Rc};

use crate::formula::Formula;

#[derive(Clone, Debug)]
pub(crate) enum FlatFormula {
    Bool(bool),
    Var(bool, char),
    VarI(bool, char, usize),
    Disj(usize, Box<FlatFormula>, Box<FlatFormula>),
    Conj(usize, Box<FlatFormula>, Box<FlatFormula>),
    Dm(usize, Box<FlatFormula>),
    Bx(usize, Box<FlatFormula>),
    Ge(usize, u32, Box<FlatFormula>),
    Le(usize, u32, Box<FlatFormula>),
}

impl From<Rc<Formula>> for FlatFormula {
    fn from(value: Rc<Formula>) -> Self {
        let mut d1f = value.init_flat();
        d1f.flatten();
        d1f
    }
}

impl From<FlatFormula> for Rc<Formula> {
    fn from(value: FlatFormula) -> Self {
        match value {
            FlatFormula::Bool(false) => Formula::bottom(),
            FlatFormula::Bool(true) => Formula::top(),
            FlatFormula::Var(true, p) => Rc::new(Formula::PropVar(p, None)),
            FlatFormula::Var(false, p) => Rc::new(Formula::PropVar(p, None)).not(),
            FlatFormula::VarI(true, p, i) => Rc::new(Formula::PropVar(p, Some(i))),
            FlatFormula::VarI(false, p, i) => Rc::new(Formula::PropVar(p, Some(i))).not(),
            FlatFormula::Disj(_, phi0, phi1) => Rc::<Formula>::from(*phi0).or(&(*phi1).into()),
            FlatFormula::Conj(_, phi0, phi1) => Rc::<Formula>::from(*phi0).and(&(*phi1).into()),
            FlatFormula::Dm(_, phi) => Rc::<Formula>::from(*phi).diamond(),
            FlatFormula::Bx(_, phi) => Rc::<Formula>::from(*phi).box_(),
            FlatFormula::Ge(_, c, phi) => Rc::<Formula>::from(*phi).dmge(c),
            FlatFormula::Le(_, c, phi) => Rc::<Formula>::from(*phi).dmle(c),
        }
    }
}

impl FlatFormula {
    const fn depth(&self) -> usize {
        match self {
            FlatFormula::Var(..) | FlatFormula::VarI(..) | FlatFormula::Bool(_) => 0,
            FlatFormula::Disj(d, ..)
            | FlatFormula::Conj(d, ..)
            | FlatFormula::Dm(d, ..)
            | FlatFormula::Bx(d, ..)
            | FlatFormula::Ge(d, ..)
            | FlatFormula::Le(d, ..) => *d,
        }
    }

    fn reset_depth(&mut self) {
        match self {
            FlatFormula::Bool(_) | FlatFormula::Var(_, _) | FlatFormula::VarI(_, _, _) => {}
            FlatFormula::Disj(d, phi0, phi1) | FlatFormula::Conj(d, phi0, phi1) => {
                *d = max(phi0.depth(), phi1.depth());
            }
            FlatFormula::Dm(d, phi)
            | FlatFormula::Bx(d, phi)
            | FlatFormula::Ge(d, _, phi)
            | FlatFormula::Le(d, _, phi) => {
                *d = phi.depth() + 1;
            }
        }
    }

    fn not(self) -> Self {
        match self {
            FlatFormula::Bool(sign) => FlatFormula::Bool(!sign),
            FlatFormula::Var(sign, p) => FlatFormula::Var(!sign, p),
            FlatFormula::VarI(sign, p, i) => FlatFormula::VarI(!sign, p, i),
            FlatFormula::Disj(_, phi0, phi1) => phi0.not().conj(phi1.not()),
            FlatFormula::Conj(_, phi0, phi1) => phi0.not().disj(phi1.not()),
            FlatFormula::Dm(_, phi) => phi.not().bx(),
            FlatFormula::Bx(_, phi) => phi.not().dm(),
            FlatFormula::Ge(_, c, phi) => phi.le(c - 1),
            FlatFormula::Le(_, c, phi) => phi.ge(c + 1),
        }
    }

    fn conj(self, phi1: Self) -> Self {
        Self::Conj(
            max(self.depth(), phi1.depth()),
            Box::new(self),
            Box::new(phi1),
        )
    }

    fn disj(self, phi1: Self) -> Self {
        Self::Disj(
            max(self.depth(), phi1.depth()),
            Box::new(self),
            Box::new(phi1),
        )
    }

    fn bx(self) -> Self {
        FlatFormula::Bx(self.depth() + 1, Box::new(self))
    }

    fn dm(self) -> Self {
        FlatFormula::Dm(self.depth() + 1, Box::new(self))
    }

    fn ge(self, count: u32) -> Self {
        if count == 0 {
            FlatFormula::Bool(true)
        } else if count == 1 {
            FlatFormula::Dm(self.depth() + 1, Box::new(self))
        } else {
            FlatFormula::Ge(self.depth() + 1, count, Box::new(self))
        }
    }

    fn le(self, count: u32) -> Self {
        FlatFormula::Le(self.depth() + 1, count, Box::new(self))
    }

    fn flatten(&mut self) {
        match self {
            FlatFormula::Var(..) | FlatFormula::VarI(..) | FlatFormula::Bool(_) => {}
            FlatFormula::Disj(d, ..)
            | FlatFormula::Conj(d, ..)
            | FlatFormula::Dm(d, ..)
            | FlatFormula::Bx(d, ..)
            | FlatFormula::Ge(d, ..)
            | FlatFormula::Le(d, ..)
                if *d <= 1 => {}
            FlatFormula::Conj(d, phi0, phi1) | FlatFormula::Disj(d, phi0, phi1) => {
                phi0.flatten();
                phi1.flatten();
                *d = 1;
            }
            FlatFormula::Bx(_, phi) => {
                phi.flatten();
                *self = mem::replace(phi.as_mut(), FlatFormula::Bool(false)).flatten_bx();
            }
            FlatFormula::Dm(_, phi) => {
                phi.flatten();
                *self = mem::replace(phi.as_mut(), FlatFormula::Bool(false)).flatten_dm();
            }
            FlatFormula::Ge(_, c, phi) => {
                phi.flatten();
                *self = mem::replace(phi.as_mut(), FlatFormula::Bool(false)).flatten_ge(*c);
            }
            FlatFormula::Le(_, c, phi) => {
                phi.flatten();
                *self = mem::replace(phi.as_mut(), FlatFormula::Bool(false)).flatten_le(*c);
            }
        }
    }

    fn flatten_bx(mut self) -> FlatFormula {
        match self.flatten_rec() {
            CutStatus::Neither => self,
            CutStatus::Disj(nest) => {
                let mut phi = self.bx();
                phi.flatten();
                nest.disj(phi)
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.bx();
                phi.flatten();
                nest.conj(phi)
            }
            CutStatus::Both(nest, disj) => {
                let mut phi0 = self.bx();
                phi0.flatten();
                let mut phi1 = disj.bx();
                phi1.flatten();
                nest.conj(phi0).disj(phi1)
            }
        }
    }

    fn flatten_dm(mut self) -> FlatFormula {
        match self.flatten_rec() {
            CutStatus::Neither => self.conj(FlatFormula::Bool(true).dm()),
            CutStatus::Disj(nest) => {
                let mut phi = self.dm();
                phi.flatten();
                nest.conj(FlatFormula::Bool(true).dm()).disj(phi)
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.dm();
                phi.flatten();
                nest.conj(phi)
            }
            CutStatus::Both(nest, disj) => {
                let mut phi0 = self.dm();
                phi0.flatten();
                let mut phi1 = disj.dm();
                phi1.flatten();
                nest.conj(phi0).disj(phi1)
            }
        }
    }

    fn flatten_ge(mut self, c: u32) -> FlatFormula {
        match self.flatten_rec() {
            CutStatus::Neither => self.conj(FlatFormula::Bool(true).ge(c)),
            CutStatus::Disj(nest) => {
                let mut phi = self.ge(c);
                phi.flatten();
                nest.conj(FlatFormula::Bool(true).ge(c)).disj(phi)
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.ge(c);
                phi.flatten();
                nest.conj(phi)
            }
            CutStatus::Both(nest, disj) => {
                let mut phi0 = self.ge(c);
                phi0.flatten();
                let mut phi1 = disj.ge(c);
                phi1.flatten();
                nest.conj(phi0).disj(phi1)
            }
        }
    }

    fn flatten_le(mut self, c: u32) -> FlatFormula {
        match self.flatten_rec() {
            CutStatus::Neither => self.not().disj(FlatFormula::Bool(true).le(c)),
            CutStatus::Disj(nest) => {
                let mut phi = self.le(c);
                phi.flatten();
                nest.not().conj(phi).disj(FlatFormula::Bool(true).le(c))
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.le(c);
                phi.flatten();
                nest.not().disj(phi)
            }
            CutStatus::Both(nest, conj) => {
                let mut phi0 = self.le(c);
                phi0.flatten();
                let mut phi1 = conj.le(c);
                phi1.flatten();
                nest.not().conj(phi1).disj(phi0)
            }
        }
    }

    fn flatten_rec(&mut self) -> CutStatus {
        match self {
            FlatFormula::Var(_, _) | FlatFormula::VarI(_, _, _) | FlatFormula::Bool(_) => {
                unreachable!("Function should only be called on nested modals")
            }
            FlatFormula::Dm(..)
            | FlatFormula::Bx(..)
            | FlatFormula::Ge(..)
            | FlatFormula::Le(..) => CutStatus::Neither,
            FlatFormula::Disj(_, phi0, phi1) => {
                if phi1.depth() == 0 {
                    mem::swap(phi0.as_mut(), phi1.as_mut());
                }
                match phi1.flatten_rec() {
                    CutStatus::Neither => {
                        let phi1 = mem::replace(phi1.as_mut(), FlatFormula::Bool(false));
                        *self = mem::replace(phi0.as_mut(), FlatFormula::Bool(false));
                        CutStatus::Disj(phi1)
                    }
                    cutstatus @ CutStatus::Disj(..) => {
                        self.reset_depth();
                        cutstatus
                    }
                    CutStatus::Both(nest, disj) => {
                        let disj = disj.disj(phi0.as_ref().clone());
                        self.reset_depth();
                        CutStatus::Both(nest, disj)
                    }
                    CutStatus::Conj(nest) => {
                        let disj = phi0.as_ref().clone();
                        self.reset_depth();
                        CutStatus::Both(nest, disj)
                    }
                }
            }
            FlatFormula::Conj(_, phi0, phi1) => {
                if phi1.depth() == 0 {
                    mem::swap(phi0.as_mut(), phi1.as_mut());
                }
                match phi1.flatten_rec() {
                    CutStatus::Neither => {
                        let phi1 = mem::replace(phi1.as_mut(), FlatFormula::Bool(false));
                        *self = mem::replace(phi0.as_mut(), FlatFormula::Bool(false));
                        CutStatus::Conj(phi1)
                    }
                    cutstatus @ CutStatus::Conj(..) => {
                        self.reset_depth();
                        cutstatus
                    }
                    CutStatus::Both(nest, subdisj) => {
                        let disj = subdisj.conj(phi0.as_ref().clone());
                        self.reset_depth();
                        CutStatus::Both(nest, disj)
                    }
                    CutStatus::Disj(nest) => {
                        let mut disj = phi0.as_ref().clone();
                        self.reset_depth();
                        mem::swap(self, &mut disj);
                        CutStatus::Both(nest, disj)
                    }
                }
            }
        }
    }
}

impl Formula {
    fn init_flat(self: &Rc<Formula>) -> FlatFormula {
        match self.as_ref() {
            Formula::Bottom => FlatFormula::Bool(false),
            Formula::Top => FlatFormula::Bool(true),
            Formula::PropVar(p, Some(i)) => FlatFormula::VarI(true, *p, *i as usize),
            Formula::PropVar(p, None) => FlatFormula::Var(true, *p),
            Formula::Not(phi) => phi.init_neg_flat(),
            Formula::And(phi0, phi1) => phi0.init_flat().conj(phi1.init_flat()),
            Formula::Or(phi0, phi1) => phi0.init_flat().disj(phi1.init_flat()),
            Formula::Imply(phi0, phi1) => phi0.init_neg_flat().disj(phi1.init_flat()),
            Formula::Iff(phi0, phi1) => phi0
                .init_flat()
                .conj(phi1.init_flat())
                .disj(phi0.init_neg_flat().conj(phi1.init_neg_flat())),
            Formula::Box(phi) => phi.init_flat().bx(),
            Formula::Diamond(phi) => phi.init_flat().dm(),
            Formula::DiamondGe(c, phi) => phi.init_flat().ge(*c),
            Formula::DiamondLe(c, phi) => phi.init_flat().le(*c),
        }
    }

    fn init_neg_flat(self: &Rc<Formula>) -> FlatFormula {
        match self.as_ref() {
            Formula::Bottom => FlatFormula::Bool(true),
            Formula::Top => FlatFormula::Bool(false),
            Formula::PropVar(p, Some(i)) => FlatFormula::VarI(false, *p, *i as usize),
            Formula::PropVar(p, None) => FlatFormula::Var(false, *p),
            Formula::Not(phi) => phi.init_flat(),
            Formula::And(phi0, phi1) => phi0.init_neg_flat().disj(phi1.init_neg_flat()),
            Formula::Or(phi0, phi1) => phi0.init_neg_flat().conj(phi1.init_neg_flat()),
            Formula::Imply(phi0, phi1) => phi0.init_flat().conj(phi1.init_neg_flat()),
            Formula::Iff(phi0, phi1) => phi0
                .init_flat()
                .conj(phi1.init_neg_flat())
                .disj(phi0.init_neg_flat().conj(phi1.init_flat())),
            Formula::Box(phi) => phi.init_neg_flat().dm(),
            Formula::Diamond(phi) => phi.init_neg_flat().bx(),
            Formula::DiamondGe(c, phi) => phi.init_flat().le(*c - 1),
            Formula::DiamondLe(c, phi) => phi.init_flat().ge(*c + 1),
        }
    }
}

enum CutStatus {
    Neither,
    Disj(FlatFormula),
    Conj(FlatFormula),
    Both(FlatFormula, FlatFormula),
}

mod test {
    use super::*;

    #[test]
    fn test_depth1() {}
}
