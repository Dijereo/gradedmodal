use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{
    formula::Formula,
    tableau::{TableauNode, WorldTableau},
};

pub(crate) trait RuleCalc: StaticCalc {
    fn get_trans_rules(&self) -> &[&dyn TransitionRule];
    fn get_init_rules(&self) -> &[&dyn SequenceRule];

    fn apply(&self, world: &mut WorldTableau) {
        if world.is_closed {
            return;
        }
        self.apply_world(world);
        if world.is_closed {
            return;
        }
        self.apply_trans(world);
    }

    fn apply_trans(&self, world: &mut WorldTableau) {
        let mut new_worlds = vec![];
        for rule in self.get_trans_rules() {
            rule.transition(&world, &mut new_worlds);
        }
        for mut new_world in new_worlds {
            self.apply(&mut new_world);
            world.transitions.push(new_world);
        }
        if !world.transitions.is_empty() && world.transitions.iter().all(|nw| nw.is_closed) {
            world.is_closed = true;
        }
    }
}

trait SeqCalc {
    fn get_seq_rules(&self) -> &[&dyn SequenceRule];

    fn apply_seq_node(&self, node: &mut TableauNode) {
        let mut i = 0;
        while let Some(formula) = node.formulae.get(i) {
            let mut new_formulae = vec![];
            for rule in self.get_seq_rules() {
                new_formulae.extend(rule.expand(&formula));
            }
            for new_formula in new_formulae {
                node.add_check_dup_contra(new_formula);
                if node.is_closed {
                    return;
                }
            }
            i += 1;
        }
    }
}

trait StaticCalc: SeqCalc {
    fn get_split_rules(&self) -> &[&dyn SplitRule];

    fn apply_world(&self, world: &mut WorldTableau) {
        self.apply_static(&world.root, VecDeque::new());
        if world.root.borrow().is_closed {
            world.is_closed = true
        }
    }

    fn apply_static(
        &self,
        node: &Rc<RefCell<TableauNode>>,
        mut splits: VecDeque<Vec<Vec<Rc<Formula>>>>,
    ) {
        self.apply_seq_node(&mut node.borrow_mut());
        if !node.borrow().is_closed {
            self.add_splits(&node.borrow(), &mut splits);
            self.apply_splits(node, splits);
        }
    }

    fn add_splits(&self, node: &TableauNode, splits: &mut VecDeque<Vec<Vec<Rc<Formula>>>>) {
        if node.is_closed {
            return;
        }
        for formula in &node.formulae {
            for rule in self.get_split_rules() {
                let split = rule.expand(formula);
                if !split.is_empty() {
                    splits.push_back(split);
                }
            }
        }
    }

    fn apply_splits(
        &self,
        node: &Rc<RefCell<TableauNode>>,
        mut splits: VecDeque<Vec<Vec<Rc<Formula>>>>,
    ) {
        if node.borrow().is_closed {
            return;
        }
        while node.borrow().children.is_empty() {
            if let Some(split) = splits.pop_front() {
                for branch in split {
                    let child =
                        Rc::new(RefCell::new(TableauNode::from_formulae(branch, Some(node))));
                    node.borrow_mut().children.push(child);
                }
            } else {
                return;
            }
        }
        let mut all_closed = true;
        for child in &node.borrow().children {
            if child.borrow().is_closed {
                continue;
            }
            self.apply_static(&child, splits.clone());
            if !child.borrow().is_closed {
                all_closed = false;
            }
        }
        if all_closed {
            node.borrow_mut().is_closed = true;
        }
    }
}

struct PropCalc;

impl SeqCalc for PropCalc {
    fn get_seq_rules(&self) -> &[&dyn SequenceRule] {
        &[&PropSeqRules]
    }
}

impl StaticCalc for PropCalc {
    fn get_split_rules(&self) -> &[&dyn SplitRule] {
        &[&PropSplitRules]
    }
}

impl RuleCalc for PropCalc {
    fn get_trans_rules(&self) -> &[&dyn TransitionRule] {
        &[]
    }

    fn get_init_rules(&self) -> &[&dyn SequenceRule] {
        &[]
    }
}

impl SeqCalc for RuleCalculus {
    fn get_seq_rules(&self) -> &[&dyn SequenceRule] {
        self.seq
    }
}

impl StaticCalc for RuleCalculus {
    fn get_split_rules(&self) -> &[&dyn SplitRule] {
        self.split
    }
}

impl RuleCalc for RuleCalculus {
    fn get_trans_rules(&self) -> &[&dyn TransitionRule] {
        self.trans
    }

    fn get_init_rules(&self) -> &[&dyn SequenceRule] {
        self.init
    }
}

pub(crate) trait SequenceRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

pub(crate) trait CheckSeqRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
    fn check(&self) -> bool;
}

pub(crate) trait SplitRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>>;
}

pub(crate) trait TransitionRule {
    fn transition(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>);
}

pub(crate) trait SingleTransRule {
    fn apply_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
    fn transition_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;

    fn transition_world(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        self.apply_node_rec(&world.root.borrow(), new_worlds);
    }

    fn apply_node_rec(&self, node: &TableauNode, new_worlds: &mut Vec<WorldTableau>) {
        if node.is_closed {
            return;
        }
        for formula in &node.formulae {
            let mut trans_formulae = self.apply_formula(formula);
            if trans_formulae.is_empty() {
                continue;
            }
            node.traverse_anc_formulae(&mut |anc_form| {
                trans_formulae.extend(self.transition_formula(anc_form));
                true
            });

            let trans_node = Rc::new(RefCell::new(TableauNode::from_formulae(
                trans_formulae,
                None,
            )));
            self.transition_descendants(&node.children, &trans_node);
            let is_closed = trans_node.borrow().is_closed;
            let trans_world = WorldTableau {
                is_closed,
                root: trans_node,
                transitions: vec![],
            };
            new_worlds.push(trans_world);
        }
        for child in &node.children {
            if !child.borrow().is_closed {
                self.apply_node_rec(&child.borrow(), new_worlds);
            }
        }
    }

    fn transition_descendants(
        &self,
        children: &[Rc<RefCell<TableauNode>>],
        trans_node: &Rc<RefCell<TableauNode>>,
    ) {
        if trans_node.borrow().is_closed {
            return;
        }
        let mut is_closed = true;
        for child in children {
            if child.borrow().is_closed {
                continue;
            }
            let mut child_trans_formulae = vec![];
            for formula in &child.borrow().formulae {
                child_trans_formulae.extend(self.transition_formula(formula));
            }
            let trans_child = Rc::new(RefCell::new(TableauNode::from_formulae(
                child_trans_formulae,
                Some(trans_node),
            )));
            self.transition_descendants(&child.borrow().children, &trans_child);
            is_closed &= trans_child.borrow().is_closed;
            trans_node.borrow_mut().children.push(trans_child);
        }
        if !children.is_empty() && is_closed {
            trans_node.borrow_mut().is_closed = true;
        }
    }
}

struct SingleWrapper<R>(R);

impl<R> TransitionRule for SingleWrapper<R>
where
    R: SingleTransRule,
{
    fn transition(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        self.0.transition_world(world, new_worlds);
    }
}

pub(crate) trait OptTransRule {
    fn get_single_rule(&self) -> &dyn SingleTransRule;

    fn transition_world(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        if world.is_closed {
            return;
        }
        let single = self.get_single_rule();
        let count = new_worlds.len();
        single.transition_world(world, new_worlds);
        if new_worlds.len() == count {
            let mut trans_formulae = vec![];
            for formula in &world.root.borrow().formulae {
                trans_formulae.extend(single.transition_formula(formula));
            }
            let trans_root = Rc::new(RefCell::new(TableauNode::from_formulae(
                trans_formulae,
                None,
            )));
            single.transition_descendants(&world.root.borrow().children, &trans_root);
        }
    }
}

struct OptWrapper<R>(R);

impl<R> TransitionRule for OptWrapper<R>
where
    R: OptTransRule,
{
    fn transition(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        self.0.transition_world(world, new_worlds);
    }
}

pub(crate) trait AllTransRule {
    fn transition_formula(&self, formula: &Rc<Formula>) -> (Vec<Rc<Formula>>, bool);
    fn require_applicable(&self) -> bool;

    fn transition_world(&self, world: &WorldTableau) -> Option<WorldTableau> {
        if let Some((new_root, applicable)) = self.apply_node_rec(&world.root.borrow(), None, false)
        {
            if self.require_applicable() && !applicable {
                None
            } else {
                Some(WorldTableau::from_root(new_root))
            }
        } else {
            None
        }
    }

    fn apply_node_rec(
        &self,
        node: &TableauNode,
        trans_parent: Option<&Rc<RefCell<TableauNode>>>,
        mut applicable: bool,
    ) -> Option<(Rc<RefCell<TableauNode>>, bool)> {
        if node.is_closed {
            return None;
        }
        let mut trans_formulae: Vec<Rc<Formula>> = vec![];
        for formula in &node.formulae {
            let (new_tfae, app) = self.transition_formula(formula);
            trans_formulae.extend(new_tfae);
            applicable |= app;
        }
        let trans_node = Rc::new(RefCell::new(TableauNode::from_formulae(
            trans_formulae,
            trans_parent,
        )));
        if trans_node.borrow().is_closed {
            return Some((trans_node, applicable));
        }
        let mut should_close = node.children.is_empty();
        for child in &node.children {
            if let Some((trans_child, app)) =
                self.apply_node_rec(&child.borrow(), Some(&trans_node), applicable)
            {
                applicable |= app;
                should_close &= trans_child.borrow().is_closed;
                if !self.require_applicable() || applicable {
                    trans_node.borrow_mut().children.push(trans_child);
                }
            }
        }
        if should_close {
            trans_node.borrow_mut().is_closed = true;
        }
        return Some((trans_node, applicable));
    }
}

struct AllWrapper<R>(R);

impl<R> TransitionRule for AllWrapper<R>
where
    R: AllTransRule,
{
    fn transition(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        if let Some(new_world) = self.0.transition_world(world) {
            new_worlds.push(new_world);
        }
    }
}

pub(crate) trait CheckLoopTransRule {
    fn transition_formula(&self, formula: &Rc<Formula>) -> (Vec<Rc<Formula>>, Vec<Rc<Formula>>);

    fn transition_world(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        todo!()
    }
}

struct CheckLoopWrapper<R>(R);

impl<R> TransitionRule for CheckLoopWrapper<R>
where
    R: CheckLoopTransRule,
{
    fn transition(&self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        self.0.transition_world(world, new_worlds);
    }
}

pub(crate) struct RuleCalculus {
    pub(crate) init: &'static [&'static dyn SequenceRule],
    pub(crate) seq: &'static [&'static dyn SequenceRule],
    pub(crate) check: &'static [&'static dyn CheckSeqRule],
    pub(crate) split: &'static [&'static dyn SplitRule],
    pub(crate) trans: &'static [&'static dyn TransitionRule],
}

pub(crate) const PROP_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[],
};

pub(crate) const K_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&SingleWrapper(KRule)],
};

pub(crate) const T_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &TRule],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&SingleWrapper(KRule)],
};

pub(crate) const D_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &DRule],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&SingleWrapper(KRule)],
};

pub(crate) const D_PRIME_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&OptWrapper(KDRule)],
};

pub(crate) const K4_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&SingleWrapper(K4Rule)],
};

pub(crate) const K4D_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &DRule],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&SingleWrapper(K4Rule)],
};

pub(crate) const K45_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&AllWrapper(_45Rule)],
};

pub(crate) const K45D_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&AllWrapper(_45DRule)],
};

pub(crate) const S4_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &TRule],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&SingleWrapper(S4Rule)],
};

pub(crate) const S5PI_CALCULUS: RuleCalculus = RuleCalculus {
    init: &[&PiRule],
    seq: &[&PropSeqRules, &TRule],
    check: &[],
    split: &[&PropSplitRules],
    trans: &[&AllWrapper(S5Rule)],
};

pub(crate) const K45_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
    trans: &[&AllWrapper(_45Rule)],
};

pub(crate) const K45D_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules],
    check: &[],
    split: &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
    trans: &[&AllWrapper(_45DRule)],
};

pub(crate) const K4B_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &_5Rule],
    check: &[&TDiamondRule],
    split: &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
    trans: &[&SingleWrapper(K4Rule)],
};

pub(crate) const S4_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &TRule],
    check: &[],
    split: &[&CutPropRules, &CutDiamondRule],
    trans: &[&SingleWrapper(S4Rule)],
};

pub(crate) const B_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &TRule],
    check: &[],
    split: &[&CutPropRules, &CutDiamondRule, &BRule],
    trans: &[&SingleWrapper(KRule)],
};

pub(crate) const S5_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &TRule, &_5Rule],
    check: &[],
    split: &[&CutPropRules, &CutDiamondRule],
    trans: &[&SingleWrapper(S4Rule)],
};

pub(crate) const S5_PRIME_CUTCULUS: RuleCalculus = RuleCalculus {
    init: &[],
    seq: &[&PropSeqRules, &TRule],
    check: &[],
    split: &[&CutPropRules, &CutDiamondRule],
    trans: &[&AllWrapper(S5Rule)],
};

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
struct PiRule;

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

impl SingleTransRule for KRule {
    fn apply_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
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

impl OptTransRule for KDRule {
    fn get_single_rule(&self) -> &'static dyn SingleTransRule {
        &KRule
    }
}

impl SingleTransRule for K4Rule {
    fn apply_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![formula.clone(), phi.clone()],
            _ => vec![],
        }
    }
}

impl SingleTransRule for S4Rule {
    fn apply_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn transition_formula(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(_) => vec![formula.clone()],
            _ => vec![],
        }
    }
}

impl AllTransRule for _45Rule {
    fn require_applicable(&self) -> bool {
        true
    }

    fn transition_formula(&self, formula: &Rc<Formula>) -> (Vec<Rc<Formula>>, bool) {
        match formula.as_ref() {
            Formula::Diamond(phi) => (vec![phi.clone(), formula.clone()], true),
            Formula::Box(phi) => (vec![phi.clone(), formula.clone()], false),
            _ => (vec![], false),
        }
    }
}

impl AllTransRule for _45DRule {
    fn transition_formula(&self, formula: &Rc<Formula>) -> (Vec<Rc<Formula>>, bool) {
        match formula.as_ref() {
            Formula::Diamond(phi) => (vec![phi.clone(), formula.clone()], true),
            Formula::Box(phi) => (vec![phi.clone(), formula.clone()], false),
            _ => (vec![], false),
        }
    }

    fn require_applicable(&self) -> bool {
        false
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

impl CheckSeqRule for TDiamondRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }

    fn check(&self) -> bool {
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
    fn transition_formula(&self, formula: &Rc<Formula>) -> (Vec<Rc<Formula>>, bool) {
        match formula.as_ref() {
            Formula::Diamond(phi) => (vec![formula.clone(), phi.clone()], true),
            Formula::Box(_) => (vec![formula.clone()], false),
            _ => (vec![], false),
        }
    }

    fn require_applicable(&self) -> bool {
        true
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

impl SequenceRule for PiRule {
    fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        vec![formula.diamond()]
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
