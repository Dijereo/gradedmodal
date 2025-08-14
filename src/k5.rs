use std::{cell::RefCell, collections::{HashMap, VecDeque}, fmt, ops::RangeInclusive, rc::Rc};

use good_lp::{solvers, variable, Expression, ProblemVariables, Solution, SolverModel};

use crate::{model::{Edge, IntoModelGraph, Node}, rules3::{Calculus, Feasibility}, tableau2::{LabeledFormula, TableauNode2}, transit::{general_transit, BaseTransit, Constraints, DisplayTransit, Grading, Modals, ParallelWorlds, SolveTransit}};

pub(crate) struct Transit5 {
    pub(crate) feasibility: Feasibility,
    pub(crate) submodals: Vec<LabeledFormula>,
    pub(crate) spotconstraints: Constraints,
    pub(crate) paracliques: Vec<ParaClique<Self>>,
}

pub(crate) struct ParaClique<T> {
    pub(crate) feasibility: Feasibility,
    pub(crate) settings: Vec<bool>,
    pub(crate) spotws: ParallelWorlds<T>,
    pub(crate) cliquews: ParallelWorlds<T>,
    pub(crate) cliqueconstraints: Constraints,
    pub(crate) spotsolution: Vec<u32>,
    pub(crate) cliquesolution: Vec<u32>,
}

impl BaseTransit for Transit5 {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(fruit: &Rc<RefCell<TableauNode2<Self>>>, calc: &mut Calculus) -> Option<Self> {
        general_transit(calc, fruit)
    }
}

impl<T: BaseTransit> ParaClique<T> {
    pub(crate) fn new<'a>(
        submodals: impl Iterator<Item = &'a LabeledFormula>,
        signs: &Vec<bool>,
        modalboxes: impl Iterator<Item = LabeledFormula>,
        mut spotranges: Vec<RangeInclusive<usize>>,
        leaf: &Rc<RefCell<TableauNode2<T>>>,
        calc: &mut Calculus,
    ) -> Self {
        let settings: Vec<_> = submodals
            .zip(signs.iter())
            .map(|(label, sign)| LabeledFormula {
                formula: if *sign {
                    label.formula.clone()
                } else {
                    label.formula.not()
                },
                conflictset: label.conflictset.clone(),
                lemma: false,
            })
            .collect();
        let cliquemodals = Modals::new(settings.iter(), false, false);
        let mut spotformulae = settings;
        spotformulae.extend(cliquemodals.bx.iter().cloned());
        spotformulae.extend(modalboxes);
        let (cliquews, cliqueconstraints) =
            ParallelWorlds::from_modals(cliquemodals, Some(leaf), calc);
        spotranges.extend_from_slice(&cliquews.forkids);
        let spotws = ParallelWorlds::from_forks(spotformulae, spotranges, Some(leaf), calc);
        let cliquefeas = spotws.tab.borrow().feasibility;
        ParaClique {
            settings: signs.clone(),
            feasibility: cliquefeas,
            spotws,
            cliquews,
            cliqueconstraints,
            spotsolution: vec![],
            cliquesolution: vec![],
        }
    }
}

impl SolveTransit for Transit5 {
    fn recurse(&mut self, _calc: &mut Calculus) {}

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
    ) -> Self {
        let submodals = modals.submodals();
        let mut settings = vec![true; submodals.len()];
        let mut paracliques = vec![];
        let (spotranges, spotconstraints) = modals.to_forks_constraints(&mut calc.forks);
        let mut feasibility = Feasibility::Contradiction;
        // OPT: Get initial spotlight para worlds and break if contradiction
        loop {
            let paraclique = ParaClique::new(
                submodals.iter(),
                &settings,
                spotconstraints.boxsubforms.iter().cloned(),
                Vec::from_iter(spotranges.iter().cloned()),
                leaf,
                calc,
            );
            feasibility = feasibility.better(&paraclique.feasibility);
            paracliques.push(paraclique);
            if !Self::next_setting(&mut settings) {
                break;
            }
        }
        Self {
            submodals,
            spotconstraints,
            paracliques,
            feasibility,
        }
    }

    fn solve(&mut self) {
        let mut feasibility = Feasibility::NoSolution;
        for paraclique in &mut self.paracliques {
            paraclique.solve(&self.spotconstraints.gradings);
            feasibility = feasibility.better(&paraclique.feasibility);
        }
        self.feasibility = feasibility;
    }
}

impl Transit5 {
    fn next_setting(settings: &mut Vec<bool>) -> bool {
        for st in settings {
            *st = !*st;
            if !*st {
                return true;
            }
        }
        return false;
    }
}

impl<T: BaseTransit> ParaClique<T> {
    fn solve(&mut self, spotconstraints: &Vec<Grading>) {
        let mut problem = ProblemVariables::new();
        self.spotws.set_choices(true);
        self.cliquews.set_choices(true);
        let spotvars = problem.add_vector(variable().integer().min(0), self.spotws.choices.len());
        let cliqvars = problem.add_vector(variable().integer().min(0), self.cliquews.choices.len());
        let mut exprs =
            HashMap::with_capacity(spotconstraints.len() + self.cliqueconstraints.gradings.len());
        for c in spotconstraints
            .iter()
            .chain(self.cliqueconstraints.gradings.iter())
        {
            exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        }
        for (world, var) in self
            .spotws
            .choices
            .iter()
            .zip(spotvars.iter())
            .chain(self.cliquews.choices.iter().zip(cliqvars.iter()))
        {
            for (forkid, branchid) in world {
                if *branchid == 1 {
                    exprs
                        .get_mut(forkid)
                        .expect("Forkid should have been entered into hashmap")
                        .2
                        .push(var);
                }
            }
        }
        let mut model =
            solvers::scip::scip(problem.minimise(
                spotvars.iter().sum::<Expression>() + cliqvars.iter().sum::<Expression>(),
            ));
        for (_, (ge, count, worlds)) in exprs {
            let expr = worlds.into_iter().sum::<Expression>();
            let constr = if ge {
                expr.geq(count as f64)
            } else {
                expr.leq(count)
            };
            model.add_constraint(constr);
        }
        match model.solve() {
            Ok(solution) => {
                self.spotsolution = spotvars
                    .into_iter()
                    .map(|v| solution.value(v) as u32)
                    .collect();
                self.cliquesolution = cliqvars
                    .into_iter()
                    .map(|v| solution.value(v) as u32)
                    .collect();
                self.feasibility = Feasibility::Feasible;
            }
            Err(_) => self.feasibility = Feasibility::NoSolution,
        }
    }
}

impl DisplayTransit for Transit5 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        writeln!(f, "{}", self.spotconstraints)?;
        writeln!(f)?;
        writeln!(f, "Second Transition Modals:")?;
        for (i, submodal) in self.submodals.iter().enumerate() {
            writeln!(f, "ψ{i} := {}", submodal.formula)?;
        }
        writeln!(f)?;
        for (i, paracliq) in self.paracliques.iter().enumerate() {
            writeln!(f, "Clique {i}:")?;
            TableauNode2::display_root(&paracliq.spotws.tab, f, curri, roots)?;
            writeln!(f)?;
            TableauNode2::display_root(&paracliq.cliquews.tab, f, curri, roots)?;
            for (i, choice) in paracliq.spotws.choices.iter().enumerate() {
                write!(f, "u{i}: ")?;
                for (forkid, branchid) in choice {
                    write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
                }
                writeln!(f)?;
            }
            for (i, choice) in paracliq.cliquews.choices.iter().enumerate() {
                write!(f, "w{i}: ")?;
                for (forkid, branchid) in choice {
                    write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
                }
                writeln!(f)?;
            }
            if paracliq.spotsolution.is_empty() {
                writeln!(f, "No solution")?;
            } else {
                write!(f, "Solution: ")?;
                for (i, val) in paracliq.spotsolution.iter().enumerate() {
                    write!(f, "{val}*u{i} ")?;
                }
                writeln!(f)?;
                for (i, val) in paracliq.cliquesolution.iter().enumerate() {
                    write!(f, "{val}*w{i} ")?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl IntoModelGraph for Transit5 {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<Node>, edges: &mut Vec<Edge>) {
        let selfi = nodes.len();
        let selfid = selfi.to_string();
        nodes.push(Node {
            id: selfid.clone(),
            label: format!("#{selfi}"),
            extra: String::new(),
        });
        edges.push(Edge {
            source: parenti.to_string(),
            target: selfid,
            label: String::new(),
            extra: String::new(),
        });
        for pcq in &self.paracliques {
            pcq.spotws.tab.borrow().model_graph(selfi, nodes, edges);
            pcq.cliquews.tab.borrow().model_graph(selfi, nodes, edges);
        }
    }
}
