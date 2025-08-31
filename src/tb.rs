use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
    ops::RangeInclusive,
    rc::Rc,
};

use good_lp::{Expression, ProblemVariables, Solution, SolverModel, Variable, solvers, variable};

use crate::{
    formula::Formula,
    model::{EdgeView, IntoModelGraph, NodeView},
    rules3::{Calculus, Feasibility, ForkType},
    tableau2::{TabChildren, TableauNode2},
    timeout::{MayTimeout, TimeoutHandler},
    transit::{
        self, BaseTransit, Constraints, DisplayTransit, Grading, Modals, ParallelWorlds,
        SolveTransit,
    },
};

pub(crate) struct TransitTB {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) boxforks: Vec<(Rc<Formula>, usize)>,
    pub(crate) forkids: Vec<RangeInclusive<usize>>,
    pub(crate) solution: Vec<u32>,
    pub(crate) backchoices: Option<Vec<(usize, usize)>>,
}

impl BaseTransit for TransitTB {
    fn feasibility(&self) -> Feasibility {
        self.feasibility
    }

    fn transit(
        fruit: &Rc<RefCell<TableauNode2<TransitTB>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>> {
        transit::general_transit(calc, fruit, toh)
    }
}

impl SolveTransit for TransitTB {
    fn solve(&mut self, toh: &impl TimeoutHandler) -> MayTimeout<()> {
        Ok(())
    }

    fn from_modals(
        modals: Modals,
        leaf: &Rc<RefCell<TableauNode2<Self>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Self> {
        let (forks, constraints, boxforks) =
            modals.to_deep_forks_constraints(&mut calc.forks, toh)?;
        let mut formulae = vec![];
        leaf.borrow().traverse_anc_formulae(&mut |formula| {
            formulae.push(formula.clone());
            true
        });
        for formula in &mut formulae {
            formula.lemma = true;
        }
        let paraws = ParallelWorlds::from_forks(formulae, forks.clone(), Some(leaf), calc, toh)?;
        let feasibility = paraws.tab.borrow().feasibility;
        Ok(Self {
            feasibility,
            paraws,
            constraints,
            solution: vec![],
            backchoices: None,
            boxforks,
            forkids: forks,
        })
    }

    fn recurse(&mut self, calc: &mut Calculus, toh: &impl TimeoutHandler) -> MayTimeout<()> {
        if self.is_closed() {
            return Ok(());
        }
        let mut flowers = Vec::new();
        TableauNode2::get_flowers(&self.paraws.tab, &mut flowers);
        for flower in flowers {
            match Self::full_transit(&flower, calc, toh)? {
                TransitResult::None => {}
                TransitResult::BackContra => {
                    flower.borrow_mut().feasibility = Feasibility::Contradiction
                }
                TransitResult::Transit(transit) => {
                    flower.borrow_mut().feasibility = transit.feasibility();
                    flower.borrow_mut().children = TabChildren::Transition(transit);
                }
            }
        }
        TableauNode2::set_feasibility_rec(&self.paraws.tab);
        Ok(())
    }
}

pub(crate) enum TransitResult {
    None,
    BackContra,
    Transit(TransitTB),
}

impl TransitTB {
    fn full_transit(
        fruit: &Rc<RefCell<TableauNode2<TransitTB>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<TransitResult> {
        let mut formulae = vec![];
        fruit.borrow().traverse_anc_formulae(&mut |formula| {
            formulae.push(formula.clone());
            true
        });
        let modals = Modals::new(formulae.iter(), false, false, toh)?;
        if modals.ge.is_empty() {
            // TODO: check all le <=2 or valid
            return Ok(TransitResult::None);
        }
        let mut transit = Self::from_existing_modals(modals, fruit, calc, toh)?;
        let mut transit = if let Some(transit) = transit {
            transit
        } else {
            return Ok(TransitResult::BackContra);
        };
        if transit.is_closed() {
            return Ok(TransitResult::Transit(transit));
        }
        transit.recurse(calc, toh)?;
        if transit.is_closed() {
            return Ok(TransitResult::Transit(transit));
        }
        transit.full_solve();
        Ok(TransitResult::Transit(transit))
    }

    fn from_existing_modals(
        modals: Modals,
        fruit: &Rc<RefCell<TableauNode2<TransitTB>>>,
        calc: &mut Calculus,
        toh: &impl TimeoutHandler,
    ) -> MayTimeout<Option<Self>> {
        let (forks, constraints, boxforks) = modals.to_deep_forks_constraints(&mut calc.forks, toh)?;
        let mut back_choices = vec![];
        fruit.borrow().get_matching_choices(&mut back_choices, &mut constraints.gradings.clone(), &calc.forks);
        for boxsubf in constraints.boxsubforms {
            todo!()
        }
        let formulae = todo!();
        let paraws = ParallelWorlds::from_forks(formulae, forks.clone(), Some(fruit), calc, toh)?;
        let feasibility = paraws.tab.borrow().feasibility;
        Ok(Some(Self {
                    feasibility,
                    paraws,
                    constraints,
                    solution: vec![],
                    backchoices: None,
                    boxforks,
                    forkids: forks.clone(),
                }))
    }

    pub(crate) fn solve_forward(&mut self, i: usize) -> bool {
        todo!()
        // let mut exprs = HashMap::with_capacity(self.constraints[i].len());
        // for c in &self.constraints[i] {
        //     let value = if !c.sense && c.value < 2 {
        //         return false;
        //     } else if !c.sense {
        //         c.value - 2
        //     } else {
        //         c.value
        //     };
        //     exprs.insert(c.forkid, (c.sense, value, vec![]));
        // }
        // if let Some(solution) = Self::solve_general(exprs, &self.paraws[i + 1], &[]) {
        //     self.solutions[i] = TBSolution::Forward(solution);
        //     true
        // } else {
        //     false
        // }
    }

    pub(crate) fn solve_reflex(&mut self, i: usize) {
        todo!()
        // for (j, reflx_choice) in self.paraws[i].choices.iter().enumerate() {
        //     let mut exprs = HashMap::with_capacity(self.constraints[i].len());
        //     for c in &self.constraints[i] {
        //         let value = if !c.sense && c.value < 1 {
        //             continue;
        //         } else if !c.sense {
        //             c.value - 1
        //         } else {
        //             c.value
        //         };
        //         exprs.insert(c.forkid, (c.sense, value, vec![]));
        //     }
        //     if let Some(solution) = Self::solve_general(exprs, &self.paraws[i + 1], &[reflx_choice])
        //     {
        //         if let TBSolution::Backward(solutions) = &mut self.solutions[i] {
        //             solutions.push((solution, j, None));
        //         } else {
        //             self.solutions[i] = TBSolution::Backward(vec![(solution, j, None)]);
        //         }
        //     }
        // }
    }

    pub(crate) fn solve_backward(&mut self, i: usize) {
        todo!()
        // let completed_js = if let TBSolution::Backward(solutions) = &self.solutions[i] {
        //     solutions.iter().map(|(_, j, _)| *j).collect()
        // } else {
        //     vec![]
        // };
        // for (j, reflx_choice) in self.paraws[i].choices.iter().enumerate() {
        //     if completed_js.binary_search(&j).is_ok() {
        //         continue;
        //     }
        //     for (k, back_choice) in self.paraws[i - 1].choices.iter().enumerate() {
        //         let mut exprs = HashMap::with_capacity(self.constraints[i].len());
        //         for c in &self.constraints[i] {
        //             exprs.insert(c.forkid, (c.sense, c.value, vec![]));
        //         }
        //         if let Some(solution) =
        //             Self::solve_general(exprs, &self.paraws[i + 1], &[reflx_choice, back_choice])
        //         {
        //             if let TBSolution::Backward(solutions) = &mut self.solutions[i] {
        //                 solutions.push((solution, j, Some(k)));
        //             } else {
        //                 self.solutions[i] = TBSolution::Backward(vec![(solution, j, Some(k))]);
        //             }
        //         }
        //     }
        // }
    }

    pub(crate) fn solve_general(
        mut exprs: HashMap<usize, (bool, u32, Vec<Variable>)>,
        paraws: &ParallelWorlds<Self>,
        extra_choices: &[&[(usize, usize)]],
    ) -> Option<Vec<u32>> {
        todo!()
        //     let mut problem = ProblemVariables::new();
        //     let vars = problem.add_vector(variable().integer().min(0), paraws.choices.len());
        //     let extra_vars =
        //         problem.add_vector(variable().integer().min(1).max(1), extra_choices.len());
        //     for (world, var) in paraws.choices.iter().zip(vars.iter()) {
        //         for (forkid, branchid) in world {
        //             if *branchid == 1 {
        //                 exprs
        //                     .get_mut(forkid)
        //                     .expect("Forkid should have been entered into hashmap")
        //                     .2
        //                     .push(var.clone());
        //             }
        //         }
        //     }
        //     for (choice, var) in extra_choices.iter().zip(extra_vars.iter()) {
        //         for (forkid, branchid) in *choice {
        //             if *branchid == 1 {
        //                 if let Some(entry) = exprs.get_mut(forkid) {
        //                     entry.2.push(var.clone());
        //                 }
        //             }
        //         }
        //     }
        //     let mut model = solvers::scip::scip(problem.minimise(vars.iter().sum::<Expression>()));
        //     for (_, (ge, count, worlds)) in exprs {
        //         let expr = worlds.into_iter().sum::<Expression>();
        //         let constr = if ge {
        //             expr.geq(count as f64)
        //         } else {
        //             expr.leq(count)
        //         };
        //         model.add_constraint(constr);
        //     }
        //     match model.solve() {
        //         Ok(solution) => Some(vars.into_iter().map(|v| solution.value(v) as u32).collect()),
        //         Err(_) => None,
        //     }
    }
    
    fn full_solve(&self) {
        todo!()
    }
}

impl DisplayTransit for TransitTB {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        todo!();
    }
}

impl IntoModelGraph for TransitTB {
    fn model_graph_rec(&self, parenti: usize, nodes: &mut Vec<NodeView>, edges: &mut Vec<EdgeView>) {
        todo!()
    }
}
