use std::{cmp::max, mem, rc::Rc};

use crate::formula::Formula;

#[derive(Clone, Debug)]
enum Depth1F {
    Bool(bool),
    Var(bool, char),
    VarI(bool, char, usize),
    Disj(usize, Box<Depth1F>, Box<Depth1F>),
    Conj(usize, Box<Depth1F>, Box<Depth1F>),
    Dm(usize, Box<Depth1F>),
    Bx(usize, Box<Depth1F>),
    Ge(usize, u32, Box<Depth1F>),
    Le(usize, u32, Box<Depth1F>),
}

impl From<Rc<Formula>> for Depth1F {
    fn from(value: Rc<Formula>) -> Self {
        let mut d1f = value.init_depth1();
        d1f.unnest();
        d1f
    }
}

impl From<Depth1F> for Rc<Formula> {
    fn from(value: Depth1F) -> Self {
        match value {
            Depth1F::Bool(false) => Formula::bottom(),
            Depth1F::Bool(true) => Formula::top(),
            Depth1F::Var(true, p) => Rc::new(Formula::PropVar(p, None)),
            Depth1F::Var(false, p) => Rc::new(Formula::PropVar(p, None)).not(),
            Depth1F::VarI(false, p, i) => Rc::new(Formula::PropVar(p, Some(i as u32))),
            Depth1F::VarI(true, p, i) => Rc::new(Formula::PropVar(p, Some(i as u32))).not(),
            Depth1F::Disj(_, phi0, phi1) => Rc::<Formula>::from(*phi0).or(&(*phi1).into()),
            Depth1F::Conj(_, phi0, phi1) => Rc::<Formula>::from(*phi0).and(&(*phi1).into()),
            Depth1F::Dm(_, phi) => Rc::<Formula>::from(*phi).diamond(),
            Depth1F::Bx(_, phi) => Rc::<Formula>::from(*phi).box_(),
            Depth1F::Ge(_, c, phi) => Rc::<Formula>::from(*phi).dmge(c),
            Depth1F::Le(_, c, phi) => Rc::<Formula>::from(*phi).dmle(c),
        }
    }
}

impl Depth1F {
    fn depth(&self) -> usize {
        match self {
            Depth1F::Var(..) | Depth1F::VarI(..) | Depth1F::Bool(_) => 0,
            Depth1F::Disj(d, ..)
            | Depth1F::Conj(d, ..)
            | Depth1F::Dm(d, ..)
            | Depth1F::Bx(d, ..)
            | Depth1F::Ge(d, ..)
            | Depth1F::Le(d, ..) => *d,
        }
    }

    fn reset_depth(&mut self) {
        match self {
            Depth1F::Bool(_) | Depth1F::Var(_, _) | Depth1F::VarI(_, _, _) => {}
            Depth1F::Disj(d, phi0, phi1) | Depth1F::Conj(d, phi0, phi1) => {
                *d = max(phi0.depth(), phi1.depth());
            }
            Depth1F::Dm(d, phi)
            | Depth1F::Bx(d, phi)
            | Depth1F::Ge(d, _, phi)
            | Depth1F::Le(d, _, phi) => {
                *d = phi.depth() + 1;
            }
        }
    }

    fn not(self) -> Self {
        match self {
            Depth1F::Bool(sign) => Depth1F::Bool(!sign),
            Depth1F::Var(sign, p) => Depth1F::Var(!sign, p),
            Depth1F::VarI(sign, p, i) => Depth1F::VarI(!sign, p, i),
            Depth1F::Disj(_, phi0, phi1) => phi0.not().conj(phi1.not()),
            Depth1F::Conj(_, phi0, phi1) => phi0.not().disj(phi1.not()),
            Depth1F::Dm(_, phi) => phi.not().bx(),
            Depth1F::Bx(_, phi) => phi.not().dm(),
            Depth1F::Ge(_, c, phi) => phi.le(c - 1),
            Depth1F::Le(_, c, phi) => phi.ge(c + 1),
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
        Depth1F::Bx(self.depth() + 1, Box::new(self))
    }

    fn dm(self) -> Self {
        Depth1F::Dm(self.depth() + 1, Box::new(self))
    }

    fn ge(self, count: u32) -> Self {
        if count == 0 {
            Depth1F::Bool(true)
        } else if count == 1 {
            Depth1F::Dm(self.depth() + 1, Box::new(self))
        } else {
            Depth1F::Ge(self.depth() + 1, count, Box::new(self))
        }
    }

    fn le(self, count: u32) -> Self {
        Depth1F::Le(self.depth() + 1, count, Box::new(self))
    }

    fn unnest(&mut self) {
        match self {
            Depth1F::Var(..) | Depth1F::VarI(..) | Depth1F::Bool(_) => {}
            Depth1F::Disj(d, ..)
            | Depth1F::Conj(d, ..)
            | Depth1F::Dm(d, ..)
            | Depth1F::Bx(d, ..)
            | Depth1F::Ge(d, ..)
            | Depth1F::Le(d, ..)
                if *d <= 1 => {}
            Depth1F::Conj(d, phi0, phi1) | Depth1F::Disj(d, phi0, phi1) => {
                phi0.unnest();
                phi1.unnest();
                *d = 1;
            }
            Depth1F::Bx(_, phi) => {
                phi.unnest();
                *self = mem::replace(phi.as_mut(), Depth1F::Bool(false)).unnest_bx();
            }
            Depth1F::Dm(_, phi) => {
                phi.unnest();
                *self = mem::replace(phi.as_mut(), Depth1F::Bool(false)).unnest_dm();
            }
            Depth1F::Ge(_, c, phi) => {
                phi.unnest();
                *self = mem::replace(phi.as_mut(), Depth1F::Bool(false)).unnest_ge(*c);
            }
            Depth1F::Le(_, c, phi) => {
                phi.unnest();
                *self = mem::replace(phi.as_mut(), Depth1F::Bool(false)).unnest_le(*c);
            }
        }
    }

    fn unnest_bx(mut self) -> Depth1F {
        match self.unnest_rec() {
            CutStatus::Neither => self,
            CutStatus::Disj(nest) => {
                let mut phi = self.bx();
                phi.unnest();
                nest.disj(phi)
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.bx();
                phi.unnest();
                nest.conj(phi)
            }
            CutStatus::Both(nest, disj) => {
                let mut phi0 = self.bx();
                phi0.unnest();
                let mut phi1 = disj.bx();
                phi1.unnest();
                nest.conj(phi0).disj(phi1)
            }
        }
    }

    fn unnest_dm(mut self) -> Depth1F {
        match self.unnest_rec() {
            CutStatus::Neither => self.conj(Depth1F::Bool(true).dm()),
            CutStatus::Disj(nest) => {
                let mut phi = self.dm();
                phi.unnest();
                nest.conj(Depth1F::Bool(true).dm()).disj(phi)
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.dm();
                phi.unnest();
                nest.conj(phi)
            }
            CutStatus::Both(nest, disj) => {
                let mut phi0 = self.dm();
                phi0.unnest();
                let mut phi1 = disj.dm();
                phi1.unnest();
                nest.conj(phi0).disj(phi1)
            }
        }
    }

    fn unnest_ge(mut self, c: u32) -> Depth1F {
        match self.unnest_rec() {
            CutStatus::Neither => self.conj(Depth1F::Bool(true).ge(c)),
            CutStatus::Disj(nest) => {
                let mut phi = self.ge(c);
                phi.unnest();
                nest.conj(Depth1F::Bool(true).ge(c)).disj(phi)
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.ge(c);
                phi.unnest();
                nest.conj(phi)
            }
            CutStatus::Both(nest, disj) => {
                let mut phi0 = self.ge(c);
                phi0.unnest();
                let mut phi1 = disj.ge(c);
                phi1.unnest();
                nest.conj(phi0).disj(phi1)
            }
        }
    }

    fn unnest_le(mut self, c: u32) -> Depth1F {
        match self.unnest_rec() {
            CutStatus::Neither => self.not().disj(Depth1F::Bool(true).le(c)),
            CutStatus::Disj(nest) => {
                let mut phi = self.le(c);
                phi.unnest();
                nest.not().conj(phi).disj(Depth1F::Bool(true).le(c))
            }
            CutStatus::Conj(nest) => {
                let mut phi = self.le(c);
                phi.unnest();
                nest.not().disj(phi)
            }
            CutStatus::Both(nest, conj) => {
                let mut phi0 = self.le(c);
                phi0.unnest();
                let mut phi1 = conj.le(c);
                phi1.unnest();
                nest.not().conj(phi1).disj(phi0)
            }
        }
    }

    fn unnest_rec(&mut self) -> CutStatus {
        match self {
            Depth1F::Var(_, _) | Depth1F::VarI(_, _, _) | Depth1F::Bool(_) => {
                unreachable!("Function should only be called on nested modals")
            }
            Depth1F::Dm(..) | Depth1F::Bx(..) | Depth1F::Ge(..) | Depth1F::Le(..) => {
                CutStatus::Neither
            }
            Depth1F::Disj(_, phi0, phi1) => {
                if phi1.depth() == 0 {
                    mem::swap(phi0.as_mut(), phi1.as_mut());
                }
                match phi1.unnest_rec() {
                    CutStatus::Neither => {
                        let phi1 = mem::replace(phi1.as_mut(), Depth1F::Bool(false));
                        *self = mem::replace(phi0.as_mut(), Depth1F::Bool(false));
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
            Depth1F::Conj(_, phi0, phi1) => {
                if phi1.depth() == 0 {
                    mem::swap(phi0.as_mut(), phi1.as_mut());
                }
                match phi1.unnest_rec() {
                    CutStatus::Neither => {
                        let phi1 = mem::replace(phi1.as_mut(), Depth1F::Bool(false));
                        *self = mem::replace(phi0.as_mut(), Depth1F::Bool(false));
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
    fn init_depth1(self: &Rc<Formula>) -> Depth1F {
        match self.as_ref() {
            Formula::Bottom => Depth1F::Bool(false),
            Formula::Top => Depth1F::Bool(true),
            Formula::PropVar(p, Some(i)) => Depth1F::VarI(true, *p, *i as usize),
            Formula::PropVar(p, None) => Depth1F::Var(true, *p),
            Formula::Not(phi) => phi.init_neg_depth1(),
            Formula::And(phi0, phi1) => phi0.init_depth1().conj(phi1.init_depth1()),
            Formula::Or(phi0, phi1) => phi0.init_depth1().disj(phi1.init_depth1()),
            Formula::Imply(phi0, phi1) => phi0.init_neg_depth1().disj(phi1.init_depth1()),
            Formula::Iff(phi0, phi1) => phi0
                .init_depth1()
                .conj(phi1.init_depth1())
                .disj(phi0.init_neg_depth1().conj(phi1.init_neg_depth1())),
            Formula::Box(phi) => phi.init_depth1().bx(),
            Formula::Diamond(phi) => phi.init_depth1().dm(),
            Formula::DiamondGe(c, phi) => phi.init_depth1().ge(*c),
            Formula::DiamondLe(c, phi) => phi.init_depth1().le(*c),
        }
    }

    fn init_neg_depth1(self: &Rc<Formula>) -> Depth1F {
        match self.as_ref() {
            Formula::Bottom => Depth1F::Bool(true),
            Formula::Top => Depth1F::Bool(false),
            Formula::PropVar(p, Some(i)) => Depth1F::VarI(false, *p, *i as usize),
            Formula::PropVar(p, None) => Depth1F::Var(false, *p),
            Formula::Not(phi) => phi.init_depth1(),
            Formula::And(phi0, phi1) => phi0.init_neg_depth1().disj(phi1.init_neg_depth1()),
            Formula::Or(phi0, phi1) => phi0.init_neg_depth1().conj(phi1.init_neg_depth1()),
            Formula::Imply(phi0, phi1) => phi0.init_depth1().conj(phi1.init_neg_depth1()),
            Formula::Iff(phi0, phi1) => phi0
                .init_depth1()
                .conj(phi1.init_neg_depth1())
                .disj(phi0.init_neg_depth1().conj(phi1.init_depth1())),
            Formula::Box(phi) => phi.init_neg_depth1().dm(),
            Formula::Diamond(phi) => phi.init_neg_depth1().bx(),
            Formula::DiamondGe(c, phi) => phi.init_depth1().le(*c - 1),
            Formula::DiamondLe(c, phi) => phi.init_depth1().ge(*c + 1),
        }
    }
}

enum CutStatus {
    Neither,
    Disj(Depth1F),
    Conj(Depth1F),
    Both(Depth1F, Depth1F),
}

mod test {
    use super::*;

    #[test]
    fn test_parse_propvar() {}
}
