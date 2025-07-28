use std::{cell::RefCell, rc::Rc};

use crate::{
    formula::Formula,
    tableau::{TableauNode, WorldTableau},
};

impl<'a> TransCalc<'a> {
    fn apply(&mut self, world: &mut WorldTableau) {
        if world.is_closed {
            return;
        }
        self.get_init_rules().apply(world.root);
        if world.is_closed {
            return;
        }
        self.apply_world(world);
        if world.is_closed {
            return;
        }
        self.apply_trans(world);
    }

    fn apply_trans(&mut self, world: &mut WorldTableau) {
        let mut new_worlds = vec![];
        for rule in self.get_trans_rules() {
            rule.apply(&world, &mut new_worlds);
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

struct SimpleSequenceCalc<'a> {
    rules: &'a [&'a dyn SequenceRule],
}

impl SimpleSequenceCalc<'_> {
    fn apply(&mut self, node: &mut TableauNode) {
        let mut i = 0;
        while let Some(formula) = node.formulae.get(i) {
            let mut new_formulae = vec![];
            for rule in self.rules {
                new_formulae.extend(rule.apply(&formula));
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

trait SequenceRule {
    fn apply(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>>;
}

trait SplitRule {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>>;
}

trait TransitionRule {
    fn apply(&mut self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>);
}

// trait SequenceCalc {
//     fn get_rules() -> &[&]
// }

struct SplitIter {
    i: usize,
    j: usize,
}

struct StaticCalc<'a> {
    split_rules: &'a [&'a dyn SplitRule],
    seq_calc: SimpleSequenceCalc<'a>,
    splits: Vec<Vec<Vec<Vec<Rc<Formula>>>>>,
    iters: Vec<SplitIter>,
}

struct TransCalc<'a> {
    trans_rules: &'a [&'a dyn TransitionRule],
    init_rules: &'a [&'a dyn SequenceRule],
    static_calc: StaticCalc<'a>,
}

impl<'a> TransCalc<'a> {
    fn sat(mut self, formulae: Vec<Rc<Formula>>) -> WorldTableau {
        let mut tab = WorldTableau::from_formulae(formulae);
        if !tab.is_closed {
            self.apply(&mut tab);
        }
        tab
    }
}

impl<'a> StaticCalc<'a> {
    fn new(split_rules: &'a [&'a dyn SplitRule], seq_calc: SimpleSequenceCalc<'a>) -> Self {
        Self {
            split_rules,
            seq_calc,
            splits: vec![],
            iters: vec![SplitIter { i: 0, j: 0 }],
        }
    }

    fn push_split_node(&mut self, splits: Vec<Vec<Vec<Rc<Formula>>>>) {
        self.splits.push(splits);
        self.iters
            .push(self.iters.last().cloned().expect("Should not be empty"));
    }

    fn pop_split(&mut self) -> Option<&Vec<Vec<Rc<Formula>>>> {
        let curr = &mut self.iters.last().expect("Should not be empty");
        while let Some(split_node) = self.splits.get(curr.i) {
            if let Some(split) = split_node.get(curr.j) {
                return Some(split);
            }
            curr.j = 0;
            curr.i += 1;
        }
        None
    }

    fn pop_split_node(&mut self) {
        self.splits.pop();
        self.iters.pop();
    }

    fn apply(&mut self, world: &mut WorldTableau) {
        self.apply_node(&world.root);
        if world.root.borrow().is_closed {
            world.is_closed = true
        }
    }

    fn apply_node(&mut self, node: &Rc<RefCell<TableauNode>>) {
        self.get_seq_calc().apply(&mut node.borrow_mut());
        if !node.borrow().is_closed {
            self.add_splits(&node.borrow());
            self.apply_splits(node);
            self.pop_split_node();
        }
    }

    fn add_splits(&mut self, node: &TableauNode) {
        let mut splits = vec![];
        for formula in &node.formulae {
            for rule in self.get_split_rules() {
                let split = rule.apply(formula);
                if !split.is_empty() {
                    splits.push(split);
                }
            }
        }
        self.push_split_node(splits);
    }

    fn apply_splits(&self, node: &Rc<RefCell<TableauNode>>) {
        if node.borrow().children.is_empty() {
            if let Some(split) = self.pop_split() {
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
            self.apply_node(&child);
            if !child.borrow().is_closed {
                all_closed = false;
            }
        }
        if all_closed {
            node.borrow_mut().is_closed = true;
        }
    }
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
    fn apply(&mut self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
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
    fn apply(&mut self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
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
    fn apply(&mut self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
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
    fn apply(&mut self, world: &WorldTableau, new_worlds: &mut Vec<WorldTableau>) {
        self.0.transition_world(world, new_worlds);
    }
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
struct PiRule;

const PROP_SEQ_CALC: SimpleSequenceCalc = SimpleSequenceCalc {
    rules: &[&mut PropSeqRules],
};

const T_SEQ_CALC: SimpleSequenceCalc = SimpleSequenceCalc {
    rules: &[&PropSeqRules, &TRule],
};

const D_SEQ_CALC: SimpleSequenceCalc = SimpleSequenceCalc {
    rules: &[&PropSeqRules, &DRule],
};

fn t_diam_5_seq_calc() -> SimpleSequenceCalc<'static> {
    SimpleSequenceCalc {
        rules: &[&PropSeqRules, &_5Rule, &TDiamondRule],
    }
}

const T5_SEQ_CALC: SimpleSequenceCalc = SimpleSequenceCalc {
    rules: &[&PropSeqRules, &TRule, &_5Rule],
};

fn prop_static_calc() -> StaticCalc<'static> {
    StaticCalc::new(&[&PropSplitRules], PROP_SEQ_CALC)
}

fn t_static_calc() -> StaticCalc<'static> {
    StaticCalc::new(&[&PropSplitRules], T_SEQ_CALC)
}

fn d_static_calc() -> StaticCalc<'static> {
    StaticCalc::new(&[&PropSplitRules], D_SEQ_CALC)
}

fn modal_static_cut() -> StaticCalc<'static> {
    StaticCalc::new(
        &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
        PROP_SEQ_CALC,
    )
}

fn t_diam_5_static_cut() -> StaticCalc<'static> {
    StaticCalc::new(
        &[&CutPropRules, &CutBoxRule, &CutDiamondRule],
        t_diam_5_seq_calc(),
    )
}

fn t_static_cut() -> StaticCalc<'static> {
    StaticCalc::new(&[&CutPropRules, &CutDiamondRule], T_SEQ_CALC)
}

fn b_static_cut() -> StaticCalc<'static> {
    StaticCalc::new(&[&CutPropRules, &CutDiamondRule, &BRule], T_SEQ_CALC)
}

fn t5_static_cut() -> StaticCalc<'static> {
    StaticCalc::new(&[&CutPropRules, &CutDiamondRule], T5_SEQ_CALC)
}

fn prop_full_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[],
        init_rules: &[],
        static_calc: prop_static_calc(),
    }
}

fn k_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(KRule)],
        init_rules: &[],
        static_calc: prop_static_calc(),
    }
}

fn t_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(KRule)],
        init_rules: &[],
        static_calc: t_static_calc(),
    }
}

fn d_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(KRule)],
        init_rules: &[],
        static_calc: d_static_calc(),
    }
}

fn d_prime_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&OptWrapper(KDRule)],
        init_rules: &[],
        static_calc: prop_static_calc(),
    }
}

fn k4_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(K4Rule)],
        init_rules: &[],
        static_calc: prop_static_calc(),
    }
}

fn k4d_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(K4Rule)],
        init_rules: &[],
        static_calc: d_static_calc(),
    }
}

fn k45_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&AllWrapper(_45Rule)],
        init_rules: &[],
        static_calc: prop_static_calc(),
    }
}

fn k45d_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&AllWrapper(_45DRule)],
        init_rules: &[],
        static_calc: prop_static_calc(),
    }
}

fn s4_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(S4Rule)],
        init_rules: &[],
        static_calc: t_static_calc(),
    }
}

fn s5_pi_calc() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&AllWrapper(S5Rule)],
        init_rules: &[&PiRule],
        static_calc: t_static_calc(),
    }
}

fn k45_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&AllWrapper(_45Rule)],
        init_rules: &[],
        static_calc: modal_static_cut(),
    }
}

fn k45d_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&AllWrapper(_45DRule)],
        init_rules: &[],
        static_calc: modal_static_cut(),
    }
}

fn k4b_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(K4Rule)],
        init_rules: &[],
        static_calc: t_diam_5_static_cut(),
    }
}

fn s4_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(S4Rule)],
        init_rules: &[],
        static_calc: t_static_cut(),
    }
}

fn b_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(S4Rule)],
        init_rules: &[],
        static_calc: t_static_cut(),
    }
}

fn s5_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&SingleWrapper(S4Rule)],
        init_rules: &[],
        static_calc: t5_static_cut(),
    }
}

fn s5_prime_cut() -> TransCalc<'static> {
    TransCalc {
        trans_rules: &[&AllWrapper(S5Rule)],
        init_rules: &[],
        static_calc: t_static_cut(),
    }
}

impl SequenceRule for PropSeqRules {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
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
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
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
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![phi.clone()],
            _ => vec![],
        }
    }
}

impl SequenceRule for DRule {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
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
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![vec![phi.not()], vec![phi.clone(), formula.box_()]],
            _ => vec![],
        }
    }
}

// impl CheckSeqRule for TDiamondRule {
//     fn expand(&self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
//         match formula.as_ref() {
//             Formula::Box(phi) => vec![phi.clone()],
//             _ => vec![],
//         }
//     }

//     fn check(&self) -> bool {
//         todo!()
//     }
// }

impl SequenceRule for _5Rule {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
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
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Box(phi) => vec![vec![phi.clone()], vec![phi.not()]],
            _ => vec![],
        }
    }
}

impl SplitRule for CutDiamondRule {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
        match formula.as_ref() {
            Formula::Diamond(phi) => vec![vec![phi.clone()], vec![phi.not()]],
            _ => vec![],
        }
    }
}

impl SequenceRule for PiRule {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Rc<Formula>> {
        vec![formula.diamond()]
    }
}

impl SplitRule for CutPropRules {
    fn apply(&mut self, formula: &Rc<Formula>) -> Vec<Vec<Rc<Formula>>> {
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
