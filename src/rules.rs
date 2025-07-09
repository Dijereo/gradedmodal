use std::rc::Rc;

use crate::formula::Formula;

pub(crate) struct RuleCalculus {
    pub(crate) seq: &'static [&'static dyn SequenceRule],
    pub(crate) split: &'static [&'static dyn SplitRule],
    // pub(crate) pless_trans: &'static [&'static dyn PLessTransRule],
    pub(crate) trans: &'static [&'static dyn TransitionRule],
    // pub(crate) all_trans: &'static [&'static dyn AllTransRule],
}

pub(crate) const PROP_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules],
    split: &[&PropSplitRules],
    trans: &[],
};

pub(crate) const K_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules],
    split: &[&PropSplitRules],
    trans: &[&KRule],
};

pub(crate) const T_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules, &TRule],
    split: &[&PropSplitRules],
    trans: &[&KRule],
};

pub(crate) const D_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules, &DRule],
    split: &[&PropSplitRules],
    trans: &[&KRule],
};

// pub(crate) const D_PRIME_CALCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules],
//     split: &[&PropSplitRules],
//     trans: &[&KDRule],
// };

pub(crate) const K4_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules],
    split: &[&PropSplitRules],
    trans: &[&K4Rule],
};

pub(crate) const K4D_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules, &DRule],
    split: &[&PropSplitRules],
    trans: &[&K4Rule],
};

// pub(crate) const K45_CALCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules],
//     split: &[&PropSplitRules],
//     trans: &[&_45Rule],
// };

// pub(crate) const K45D_CALCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules],
//     split: &[&PropSplitRules],
//     trans: &[&_45DRule],
// };

pub(crate) const S4_CALCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules, &TRule],
    split: &[&PropSplitRules],
    trans: &[&S4Rule],
};

// pub(crate) const S5PI_CALCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules, &TRule],
//     split: &[&PropSplitRules],
//     trans: &[&S5Rule],
// };

// pub(crate) const K45_CUTCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules],
//     split: &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
//     trans: &[&_45Rule],
// };

// pub(crate) const K45D_CUTCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules],
//     split: &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
//     trans: &[&_45DRule],
// };

// pub(crate) const K4B_CUTCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules, &TDiamondRule, &_5Rule],
//     split: &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
//     trans: &[&K4Rule],
// };

pub(crate) const S4_CUTCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules, &TRule],
    split: &[&CutPropRules, &CutDiamondRule],
    trans: &[&S4Rule],
};

// pub(crate) const B_CUTCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules, &TRule, &BRule],
//     split: &[&CutPropRules, &CutDiamondRule],
//     trans: &[&KRule],
// };

pub(crate) const S5_CUTCULUS: RuleCalculus = RuleCalculus {
    seq: &[&PropSeqRules, &TRule, &_5Rule],
    split: &[&CutPropRules, &CutDiamondRule],
    trans: &[&S4Rule],
};

// pub(crate) const S5_PRIME_CUTCULUS: RuleCalculus = RuleCalculus {
//     seq: &[&PropSeqRules, &TRule],
//     split: &[&CutPropRules, &CutDiamondRule],
//     trans: &[&S5Rule],
// };

pub(crate) trait SequenceRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

pub(crate) trait CheckedSequenceRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

pub(crate) trait SplitRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>>;
}

pub(crate) trait TransitionRule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

pub(crate) trait PLessTransRule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

pub(crate) trait AllTransRule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

struct PropSeqRules;
struct PropSplitRules;
struct KRule;
struct TRule;
struct DRule;
struct KDRule;
struct K4Rule;
struct S4Rule;
struct _45Rule;
struct _45DRule;
struct BRule;
struct TDiamondRule;
struct _5Rule;
struct S5Rule;
struct CutBoxRule;
struct CutDiamondRule;
struct CutPropRules;

impl SequenceRule for PropSeqRules {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::Or(_, _)
            | Formula::Imply(_, _)
            | Formula::Iff(_, _) => vec![],
            Formula::Not(phi) => Self::expand_not(phi),
            Formula::And(phi1, phi2) => {
                vec![phi1.clone(), phi2.clone()]
            }
        }
    }
}

impl PropSeqRules {
    fn expand_not(phi: &Formula) -> Vec<Rc<Formula>> {
        match phi {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(_)
            | Formula::And(_, _)
            | Formula::Iff(_, _) => vec![],
            Formula::Not(psi) => vec![psi.clone()],
            Formula::Box(psi) => vec![psi.not().diamond()],
            Formula::Diamond(psi) => vec![psi.not().box_()],
            Formula::Or(psi1, psi2) => vec![psi1.not().and(&psi2.not())],
            Formula::Imply(psi1, psi2) => vec![psi1.clone(), psi2.not()],
        }
    }
}

impl SplitRule for PropSplitRules {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::And(_, _) => vec![],
            Formula::Not(phi) => Self::expand_split_not(phi),
            Formula::Or(phi1, phi2) => vec![vec![phi1.clone()], vec![phi2.clone()]],
            Formula::Imply(phi1, phi2) => vec![vec![phi1.not()], vec![phi2.clone()]],
            Formula::Iff(phi1, phi2) => vec![
                vec![phi1.clone(), phi2.clone()],
                vec![phi1.not(), phi2.not()],
            ],
        }
    }
}

impl PropSplitRules {
    fn expand_split_not(phi: &Formula) -> Vec<Vec<Rc<Formula>>> {
        match phi {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(_)
            | Formula::Not(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::Or(_, _)
            | Formula::Imply(_, _) => vec![],
            Formula::And(psi1, psi2) => vec![vec![psi1.not()], vec![psi2.not()]],
            Formula::Iff(psi1, psi2) => vec![
                vec![psi1.clone(), psi2.not()],
                vec![psi1.not(), psi2.clone()],
            ],
        }
    }
}

impl TransitionRule for KRule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }
}

impl SequenceRule for TRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }
}

impl SequenceRule for DRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![phi.diamond()],
            _ => vec![],
        }
    }
}

impl PLessTransRule for KDRule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }

    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }
}

impl TransitionRule for K4Rule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![formula.clone(), phi.clone()],
            _ => vec![],
        }
    }
}

impl TransitionRule for S4Rule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(_) => vec![formula.clone()],
            _ => vec![],
        }
    }
}

impl PLessTransRule for _45DRule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }

    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }
}

impl SplitRule for BRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![vec![phi.not()], vec![phi.clone(), formula.box_()]],
            _ => vec![],
        }
    }
}

impl CheckedSequenceRule for TDiamondRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }
}

impl SequenceRule for _5Rule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(_) => vec![formula.box_()],
            _ => vec![],
        }
    }
}

impl AllTransRule for S5Rule {
    fn try_apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }

    fn transition(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        todo!()
    }
}

impl SplitRule for CutBoxRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![vec![phi.clone()], vec![phi.not()]],
            _ => vec![],
        }
    }
}

impl SplitRule for CutDiamondRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![vec![phi.clone()], vec![phi.not()]],
            _ => vec![],
        }
    }
}

impl SplitRule for CutPropRules {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::And(_, _) => vec![],
            Formula::Not(phi) => Self::expand_split_not(phi),
            Formula::Or(phi1, phi2) => {
                vec![
                    vec![phi1.clone(), phi2.not()],
                    vec![phi1.not(), phi2.clone()],
                    vec![phi1.clone(), phi2.clone()],
                ]
            }
            Formula::Imply(phi1, phi2) => vec![
                vec![phi1.clone(), phi2.clone()],
                vec![phi1.not(), phi2.clone()],
                vec![phi1.not(), phi2.not()],
            ],
            Formula::Iff(phi1, phi2) => vec![
                vec![phi1.clone(), phi2.clone()],
                vec![phi1.not(), phi2.not()],
            ],
        }
    }
}

impl CutPropRules {
    fn expand_split_not(phi: &Formula) -> Vec<Vec<Rc<Formula>>> {
        match phi {
            Formula::Bottom
            | Formula::Top
            | Formula::PropVar(_)
            | Formula::Not(_)
            | Formula::Box(_)
            | Formula::Diamond(_)
            | Formula::Or(_, _)
            | Formula::Imply(_, _) => vec![],
            Formula::And(psi1, psi2) => vec![
                vec![psi1.clone(), psi2.not()],
                vec![psi1.not(), psi2.clone()],
                vec![psi1.not(), psi2.not()],
            ],
            Formula::Iff(psi1, psi2) => vec![
                vec![psi1.clone(), psi2.not()],
                vec![psi1.not(), psi2.clone()],
            ],
        }
    }
}
