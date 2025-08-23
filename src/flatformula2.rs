use std::{cmp::max, mem, rc::Rc};

use crate::{
    formula::Formula,
    timeout::{MayTimeout, TimeoutHandler},
    util::Rx,
};

#[derive(Clone, Debug)]
pub(crate) enum FlatFormula {
    Bool(bool),
    Var(bool, char),
    VarI(bool, char, usize),
    Disj(usize, Rx<FlatFormula>, Rx<FlatFormula>),
    Conj(usize, Rx<FlatFormula>, Rx<FlatFormula>),
    Dm(usize, Rx<FlatFormula>),
    Bx(usize, Rx<FlatFormula>),
    Ge(usize, u32, Rx<FlatFormula>),
    Le(usize, u32, Rx<FlatFormula>),
}

impl From<&Rx<FlatFormula>> for Rc<Formula> {
    fn from(value: &Rx<FlatFormula>) -> Self {
        match value.as_ref() {
            FlatFormula::Bool(false) => Formula::bottom(),
            FlatFormula::Bool(true) => Formula::top(),
            FlatFormula::Var(true, p) => Rc::new(Formula::PropVar(*p, None)),
            FlatFormula::Var(false, p) => Rc::new(Formula::PropVar(*p, None)).not(),
            FlatFormula::VarI(true, p, i) => Rc::new(Formula::PropVar(*p, Some(*i))),
            FlatFormula::VarI(false, p, i) => Rc::new(Formula::PropVar(*p, Some(*i))).not(),
            FlatFormula::Disj(_, phi0, phi1) => Rc::<Formula>::from(phi0).or(&(phi1).into()),
            FlatFormula::Conj(_, phi0, phi1) => Rc::<Formula>::from(phi0).and(&(phi1).into()),
            FlatFormula::Dm(_, phi) => Rc::<Formula>::from(phi).diamond(),
            FlatFormula::Bx(_, phi) => Rc::<Formula>::from(phi).box_(),
            FlatFormula::Ge(_, c, phi) => Rc::<Formula>::from(phi).dmge(*c),
            FlatFormula::Le(_, c, phi) => Rc::<Formula>::from(phi).dmle(*c),
        }
    }
}

impl FlatFormula {
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
}

impl From<FlatFormula> for Rx<FlatFormula> {
    fn from(value: FlatFormula) -> Self {
        if value.depth() == 0 {
            Rx::rc(value)
        } else {
            Rx::bxx(value)
        }
    }
}

impl Rx<FlatFormula> {
    pub(crate) fn from_rcf(formula: Rc<Formula>, toh: &impl TimeoutHandler) -> MayTimeout<Self> {
        formula.init_flat().flatten(toh)
    }

    fn not(self) -> Self {
        let inner = self.try_into_inner().unwrap_or_else(|r| r.as_ref().clone());
        match inner {
            FlatFormula::Bool(sign) => FlatFormula::Bool(!sign).into(),
            FlatFormula::Var(sign, p) => FlatFormula::Var(!sign, p).into(),
            FlatFormula::VarI(sign, p, i) => FlatFormula::VarI(!sign, p, i).into(),
            FlatFormula::Disj(_, phi0, phi1) => phi0.not().conj(phi1.not()),
            FlatFormula::Conj(_, phi0, phi1) => phi0.not().disj(phi1.not()),
            FlatFormula::Dm(_, phi) => phi.not().bx(),
            FlatFormula::Bx(_, phi) => phi.not().dm(),
            FlatFormula::Ge(_, c, phi) => phi.le(c - 1),
            FlatFormula::Le(_, c, phi) => phi.ge(c + 1),
        }
    }

    fn conj(self, phi1: Self) -> Self {
        FlatFormula::Conj(max(self.depth(), phi1.depth()), self, phi1).into()
    }

    fn disj(self, phi1: Self) -> Self {
        FlatFormula::Disj(max(self.depth(), phi1.depth()), self, phi1).into()
    }

    fn bx(self) -> Self {
        FlatFormula::Bx(self.depth() + 1, self).into()
    }

    fn dm(self) -> Self {
        FlatFormula::Dm(self.depth() + 1, self).into()
    }

    fn ge(self, count: u32) -> Self {
        if count == 0 {
            FlatFormula::Bool(true).into()
        } else if count == 1 {
            FlatFormula::Dm(self.depth() + 1, self).into()
        } else {
            FlatFormula::Ge(self.depth() + 1, count, self).into()
        }
    }

    fn le(self, count: u32) -> Self {
        FlatFormula::Le(self.depth() + 1, count, self).into()
    }

    fn flatten(self, toh: &impl TimeoutHandler) -> MayTimeout<Self> {
        if self.depth() >= 2 {
            let f = self.try_into_inner().expect("Depth >= 2 must be Rx::Box");
            match f {
                FlatFormula::Bool(_) | FlatFormula::Var(_, _) | FlatFormula::VarI(_, _, _) => {
                    unreachable!("Atom cannot have depth >= 2");
                }
                FlatFormula::Conj(_, phi0, phi1) => {
                    Ok(phi0.flatten(toh)?.conj(phi1.flatten(toh)?).into())
                }
                FlatFormula::Disj(_, phi0, phi1) => {
                    Ok(phi0.flatten(toh)?.disj(phi1.flatten(toh)?).into())
                }
                FlatFormula::Bx(_, phi) => phi.flatten(toh)?.flatten_bx(toh),
                FlatFormula::Dm(_, phi) => phi.flatten(toh)?.flatten_dm(toh),
                FlatFormula::Ge(_, c, phi) => phi.flatten(toh)?.flatten_ge(c, toh),
                FlatFormula::Le(_, c, phi) => phi.flatten(toh)?.flatten_le(c, toh),
            }
        } else {
            Ok(self)
        }
    }

    fn flatten_bx(mut self, toh: &impl TimeoutHandler) -> MayTimeout<Self> {
        match self.flatten_rec(toh)? {
            CutStatus::Neither => Ok(self.disj(Self::from(FlatFormula::Bool(false)).bx())),
            CutStatus::Disj(nest) => Ok(nest.disj(self.bx().flatten(toh)?)),
            CutStatus::Conj(nest) => Ok(nest
                .conj(self.bx().flatten(toh)?)
                .disj(Self::from(FlatFormula::Bool(false)).bx())),
            CutStatus::Both(nest, disj) => Ok(nest
                .conj(self.bx().flatten(toh)?)
                .disj(disj.bx().flatten(toh)?)),
        }
    }

    fn flatten_dm(mut self, toh: &impl TimeoutHandler) -> MayTimeout<Self> {
        match self.flatten_rec(toh)? {
            CutStatus::Neither => Ok(self.conj(Self::from(FlatFormula::Bool(true)).dm())),
            CutStatus::Disj(nest) => Ok(nest
                .conj(Self::from(FlatFormula::Bool(true)).dm())
                .disj(self.dm().flatten(toh)?)),
            CutStatus::Conj(nest) => Ok(nest.conj(self.dm().flatten(toh)?)),
            CutStatus::Both(nest, disj) => Ok(nest
                .conj(self.dm().flatten(toh)?)
                .disj(disj.dm().flatten(toh)?)),
        }
    }

    fn flatten_ge(mut self, c: u32, toh: &impl TimeoutHandler) -> MayTimeout<Self> {
        match self.flatten_rec(toh)? {
            CutStatus::Neither => Ok(self.conj(Self::from(FlatFormula::Bool(true)).ge(c))),
            CutStatus::Disj(nest) => Ok(nest
                .conj(Self::from(FlatFormula::Bool(true)).ge(c))
                .disj(self.ge(c).flatten(toh)?)),
            CutStatus::Conj(nest) => Ok(nest.conj(self.ge(c).flatten(toh)?)),
            CutStatus::Both(nest, disj) => Ok(nest
                .conj(self.ge(c).flatten(toh)?)
                .disj(disj.ge(c).flatten(toh)?)),
        }
    }

    fn flatten_le(mut self, c: u32, toh: &impl TimeoutHandler) -> MayTimeout<Self> {
        match self.flatten_rec(toh)? {
            CutStatus::Neither => Ok(self.not().disj(Self::from(FlatFormula::Bool(true)).le(c))),
            CutStatus::Disj(nest) => Ok(nest
                .not()
                .conj(self.le(c).flatten(toh)?)
                .disj(Self::from(FlatFormula::Bool(true)).le(c))),
            CutStatus::Conj(nest) => Ok(nest.not().disj(self.le(c).flatten(toh)?)),
            CutStatus::Both(nest, conj) => Ok(nest
                .not()
                .conj(conj.le(c).flatten(toh)?)
                .disj(self.le(c).flatten(toh)?)),
        }
    }

    fn flatten_rec(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<CutStatus> {
        toh.timedout()?;
        let f = self.try_deref_mut().expect("Should only be called on depth == 1, which should be Box");
        match f {
            FlatFormula::Var(_, _) | FlatFormula::VarI(_, _, _) | FlatFormula::Bool(_) => {
                unreachable!("Function should only be called on nested modals")
            }
            FlatFormula::Dm(..)
            | FlatFormula::Bx(..)
            | FlatFormula::Ge(..)
            | FlatFormula::Le(..) => Ok(CutStatus::Neither),
            FlatFormula::Disj(_, phi0, phi1) => {
                if phi1.depth() == 0 {
                    mem::swap(phi0, phi1);
                }
                match phi1.flatten_rec(toh)? {
                    CutStatus::Neither => {
                        let phi1 = mem::replace(phi1, FlatFormula::Bool(false).into());
                        *self = mem::replace(phi0, FlatFormula::Bool(false).into());
                        Ok(CutStatus::Disj(phi1))
                    }
                    status @ CutStatus::Disj(..) => {
                        f.reset_depth();
                        Ok(status)
                    }
                    CutStatus::Both(nest, disj) => {
                        let disj = disj.disj(phi0.clone());
                        f.reset_depth();
                        Ok(CutStatus::Both(nest, disj))
                    }
                    CutStatus::Conj(nest) => {
                        let disj = phi0.clone();
                        f.reset_depth();
                        Ok(CutStatus::Both(nest, disj))
                    }
                }
            }
            FlatFormula::Conj(_, phi0, phi1) => {
                if phi1.depth() == 0 {
                    mem::swap(phi0, phi1);
                }
                match phi1.flatten_rec(toh)? {
                    CutStatus::Neither => {
                        let phi1 = mem::replace(phi1, FlatFormula::Bool(false).into());
                        *self = mem::replace(phi0, FlatFormula::Bool(false).into());
                        Ok(CutStatus::Conj(phi1))
                    }
                    status @ CutStatus::Conj(..) => {
                        f.reset_depth();
                        Ok(status)
                    }
                    CutStatus::Both(nest, subdisj) => {
                        let disj = subdisj.conj(phi0.clone());
                        f.reset_depth();
                        Ok(CutStatus::Both(nest, disj))
                    }
                    CutStatus::Disj(nest) => {
                        let mut disj = phi0.as_ref().clone().into();
                        f.reset_depth();
                        mem::swap(self, &mut disj);
                        Ok(CutStatus::Both(nest, disj))
                    }
                }
            }
        }
    }
}

impl Formula {
    fn init_flat(self: &Rc<Formula>) -> Rx<FlatFormula> {
        match self.as_ref() {
            Formula::Bottom => FlatFormula::Bool(false).into(),
            Formula::Top => FlatFormula::Bool(true).into(),
            Formula::PropVar(p, Some(i)) => FlatFormula::VarI(true, *p, *i as usize).into(),
            Formula::PropVar(p, None) => FlatFormula::Var(true, *p).into(),
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

    fn init_neg_flat(self: &Rc<Formula>) -> Rx<FlatFormula> {
        match self.as_ref() {
            Formula::Bottom => FlatFormula::Bool(true).into(),
            Formula::Top => FlatFormula::Bool(false).into(),
            Formula::PropVar(p, Some(i)) => FlatFormula::VarI(false, *p, *i as usize).into(),
            Formula::PropVar(p, None) => FlatFormula::Var(false, *p).into(),
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
    Disj(Rx<FlatFormula>),
    Conj(Rx<FlatFormula>),
    Both(Rx<FlatFormula>, Rx<FlatFormula>),
}

mod test {
    use super::*;

    #[test]
    fn test_depth1() {}
}
