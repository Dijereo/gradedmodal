use std::{cell::RefCell, rc::Rc};

use crate::{
    formula::Formula,
    rules3::GradedKCalc,
    tableau2::TableauNode2,
    transit::{Transit, Transit4, Transit5, TransitB5, TransitKOr45, TransitT},
};

#[derive(Clone, Copy, Debug)]
pub(crate) enum FrameCondition {
    K,
    D,
    T,
    // KB,
    // DB,
    // TB,
    K4,
    D4,
    // S4,
    K5,
    D5,
    K45,
    D45,
    KB5,
    S5,
}

pub(crate) trait Frames {
    type Transt: Transit;

    fn to_enum(&self) -> FrameCondition;

    fn sat(&self, formulae: Vec<Rc<Formula>>) -> Rc<RefCell<TableauNode2<Self::Transt>>> {
        GradedKCalc::sat(formulae, self.to_enum())
    }
}

pub(crate) struct FramesKOr45<const D: bool, const _45: bool>;
impl<const D: bool, const _45: bool> Frames for FramesKOr45<D, _45> {
    type Transt = TransitKOr45;

    fn to_enum(&self) -> FrameCondition {
        match (D, _45) {
            (true, true) => FrameCondition::D45,
            (true, false) => FrameCondition::D,
            (false, true) => FrameCondition::K45,
            (false, false) => FrameCondition::K,
        }
    }
}

pub(crate) struct FramesT;
impl Frames for FramesT {
    fn to_enum(&self) -> FrameCondition {
        FrameCondition::T
    }

    type Transt = TransitT;
}

pub(crate) struct FramesB5<const D: bool>;
impl<const D: bool> Frames for FramesB5<D> {
    type Transt = TransitB5;

    fn to_enum(&self) -> FrameCondition {
        if D {
            FrameCondition::S5
        } else {
            FrameCondition::KB5
        }
    }
}

pub(crate) struct Frames4<const D: bool>;
impl<const D: bool> Frames for Frames4<D> {
    type Transt = Transit4;

    fn to_enum(&self) -> FrameCondition {
        if D {
            FrameCondition::D4
        } else {
            FrameCondition::K4
        }
    }
}

pub(crate) struct Frames5<const D: bool>;
impl<const D: bool> Frames for Frames5<D> {
    type Transt = Transit5;

    fn to_enum(&self) -> FrameCondition {
        if D {
            FrameCondition::D5
        } else {
            FrameCondition::K5
        }
    }
}

impl FrameCondition {
    pub(crate) const fn ray(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::T
            | FrameCondition::K4
            | FrameCondition::K5
            | FrameCondition::K45
            | FrameCondition::KB5 => false,
            FrameCondition::D
            | FrameCondition::D4
            | FrameCondition::D5
            | FrameCondition::D45
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn reflexive(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5 => false,
            FrameCondition::T | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn euclidean(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::K4
            | FrameCondition::D4 => false,
            FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn spotlit(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => false,
            FrameCondition::K5 | FrameCondition::D5 => true,
        }
    }

    pub(crate) const fn luminal(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::K5
            | FrameCondition::D5 => false,
            FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn cliqued(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45 => false,
            FrameCondition::KB5 | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn diffractive(&self) -> bool {
        match self {
            FrameCondition::K4 | FrameCondition::D4 => true,
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => false,
        }
    }
}
