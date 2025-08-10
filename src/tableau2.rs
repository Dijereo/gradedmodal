use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    ops::RangeInclusive,
    rc::{Rc, Weak},
};

use crate::{
    formula::Formula,
    rules3::Feasibility,
    transit::{BaseTransit, Transit, Transit4, Transit5, TransitB5, TransitKOr45, TransitT},
};

pub(crate) enum TabChildren<T> {
    Leaf,
    Fork {
        id: usize,
        branches: Vec<TabBranch<T>>,
    },
    Transition(T),
}

pub(crate) struct TabBranch<T> {
    pub(crate) id: usize,
    pub(crate) node: Rc<RefCell<TableauNode2<T>>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Conflict {
    forkid: usize,
    branchid: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct LabeledFormula {
    pub(crate) formula: Rc<Formula>,
    pub(crate) conflictset: Vec<Conflict>,
    pub(crate) lemma: bool,
}

pub(crate) struct TableauNode2<T> {
    pub(crate) feasibility: Feasibility,
    pub(crate) formulae: Vec<LabeledFormula>,
    pub(crate) choices: Vec<(usize, usize)>,
    pub(crate) children: TabChildren<T>,
    pub(crate) parent: Weak<RefCell<TableauNode2<T>>>,
}

pub(crate) trait DisplayTransit: Transit {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result;
}

impl<T: BaseTransit> TableauNode2<T> {
    pub(crate) fn from_formulae(
        labels: Vec<LabeledFormula>,
        parent: Option<&Rc<RefCell<Self>>>,
    ) -> Self {
        let mut tab = Self {
            feasibility: Feasibility::Feasible,
            formulae: vec![],
            choices: vec![],
            children: TabChildren::Leaf,
            parent: parent.map_or(Weak::new(), Rc::downgrade),
        };
        for label in labels {
            tab.add_check_dup_contra(label);
        }
        if tab.formulae.is_empty() {
            tab.formulae.push(LabeledFormula {
                formula: Formula::top(),
                conflictset: vec![],
                lemma: false,
            });
        }
        tab
    }

    pub(crate) fn traverse_anc_formulae(
        &self,
        map_while: &mut impl FnMut(&LabeledFormula) -> bool,
    ) {
        for label in &self.formulae {
            if !map_while(label) {
                return;
            }
        }
        if let Some(parent) = &self.parent.upgrade() {
            if let TabChildren::Fork { .. } = parent.borrow().children {
                parent.borrow().traverse_anc_formulae(map_while);
            }
        }
    }

    fn check_dup(&self, new_formula: &Rc<Formula>) -> Option<Vec<Conflict>> {
        let mut dup = None;
        self.traverse_anc_formulae(&mut |label| {
            if new_formula.directly_equivalent(&label.formula) {
                dup = Some(label.conflictset.clone());
                false
            } else {
                true
            }
        });
        dup
    }

    fn check_contra(&self, new_formula: &Rc<Formula>) -> Option<Vec<Conflict>> {
        let mut conflicts = None;
        self.traverse_anc_formulae(&mut |label| {
            // println!("Check contra: {new_formula} vs {}", label.formula);
            if new_formula.directly_contradicts(&label.formula) {
                conflicts = Some(label.conflictset.clone());
                // println!("Contra");
                false
            } else {
                // println!("Ok");
                true
            }
        });
        conflicts
    }

    pub(crate) fn check_dup_contra(&self, formula: &Rc<Formula>) -> DupContra {
        if let Some(confs) = self.check_dup(formula) {
            DupContra::Dup(confs)
        } else if formula.is_bottom() {
            DupContra::Bottom
        } else if let Some(confs) = self.check_contra(formula) {
            DupContra::Contra(confs)
        } else {
            DupContra::Ok
        }
    }

    pub(crate) fn add_check_dup_contra(&mut self, new_label: LabeledFormula) -> DupContra {
        // println!("New Formula: {}", new_label.formula);
        if let Some(confs) = self.check_dup(&new_label.formula) {
            // println!("Dup");
            DupContra::Dup(confs)
        } else if new_label.formula.is_bottom() {
            self.formulae.push(new_label);
            self.feasibility = Feasibility::Contradiction;
            // println!("Bottom");
            DupContra::Bottom
        } else if let Some(confs) = self.check_contra(&new_label.formula) {
            let mut confs2 = confs.clone();
            confs2.extend(new_label.conflictset.clone());
            self.formulae.push(new_label);
            self.formulae.push(LabeledFormula {
                formula: Formula::bottom(),
                conflictset: confs2,
                lemma: false,
            });
            self.feasibility = Feasibility::Contradiction;
            // println!("Contra");
            DupContra::Contra(confs)
        } else {
            self.formulae.push(new_label);
            // println!("Ok");
            DupContra::Ok
        }
    }

    pub(crate) const fn is_closed(&self) -> bool {
        self.feasibility.is_bad()
    }

    pub(crate) fn get_flowers(this: &Rc<RefCell<Self>>, flowers: &mut Vec<Rc<RefCell<Self>>>) {
        if this.borrow().is_closed() {
            return;
        }
        match &this.borrow().children {
            TabChildren::Leaf => {
                flowers.push(this.clone());
            }
            TabChildren::Fork { branches, .. } => {
                for child in branches {
                    Self::get_flowers(&child.node, flowers);
                }
            }
            TabChildren::Transition(..) => {}
        }
    }

    pub(crate) fn get_fruits(this: &Rc<RefCell<Self>>, fruits: &mut Vec<Rc<RefCell<Self>>>) {
        if this.borrow().is_closed() {
            return;
        }
        match &this.borrow().children {
            TabChildren::Leaf => {
                fruits.push(this.clone());
            }
            TabChildren::Fork { branches, .. } => {
                for child in branches {
                    Self::get_fruits(&child.node, fruits);
                }
            }
            TabChildren::Transition(transit) => {
                if !transit.is_closed() {
                    fruits.push(this.clone());
                }
            }
        }
    }

    pub(crate) fn set_feasibility_rec(this: &Rc<RefCell<Self>>) -> Feasibility {
        match this.borrow().feasibility {
            Feasibility::Feasible => {
                let mut thismut = this.borrow_mut();
                match &thismut.children {
                    TabChildren::Leaf => todo!(),
                    TabChildren::Fork { branches, .. } => {
                        let mut feasibility = Feasibility::Contradiction;
                        for branch in branches {
                            feasibility =
                                feasibility.better(&Self::set_feasibility_rec(&branch.node));
                        }
                        thismut.feasibility = feasibility;
                        feasibility
                    }
                    TabChildren::Transition(transit) => {
                        thismut.feasibility = transit.feasibility();
                        thismut.feasibility
                    }
                }
            }
            Feasibility::NoSolution | Feasibility::Contradiction => this.borrow().feasibility,
        }
    }

    pub(crate) fn get_choices(
        &self,
        choices: &mut Vec<(usize, usize)>,
        forkranges: &Vec<RangeInclusive<usize>>,
    ) {
        // OPT: bin search + remove
        if let Some(parent) = self.parent.upgrade() {
            if let TabChildren::Fork { .. } = parent.borrow().children {
                parent.borrow().get_choices(choices, forkranges);
            }
        }
        choices.extend(
            self.choices
                .iter()
                .filter(|(fid, _)| forkranges.iter().any(|r| r.contains(fid))),
        )
    }

    pub(crate) fn get_depths(&self) -> VecDeque<Vec<usize>> {
        let mut flat_depths = vec![];
        self.get_depths_rec(&mut flat_depths, 0);
        println!("{:?}", flat_depths);
        let mut nested_depths = VecDeque::new();
        for (i, &curr_depth) in flat_depths.iter().enumerate() {
            let mut future_depths = vec![];
            let mut min_future = None;
            for &future_depth in flat_depths[i + 1..].iter() {
                if min_future.map_or(future_depth <= curr_depth, |m| future_depth < m) {
                    future_depths.push(future_depth);
                    min_future = Some(future_depth);
                }
            }
            future_depths.reverse();
            nested_depths.push_back(future_depths);
        }
        println!("{:?}", nested_depths);
        nested_depths
    }

    pub(crate) fn get_depths_rec(&self, out: &mut Vec<usize>, depth: usize) {
        out.push(depth);
        match &self.children {
            TabChildren::Leaf => {}
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    branch.node.borrow().get_depths_rec(out, depth + 1);
                }
            }
            TabChildren::Transition(..) => out.push(depth + 1),
        }
    }

    pub(crate) fn display_root(
        this: &Rc<RefCell<Self>>,
        f: &mut fmt::Formatter<'_>,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<Self>>)>,
    ) -> fmt::Result {
        let mut depths = this.borrow().get_depths();
        Self::display_rec(this, f, 0, &mut depths, curri, roots)
    }

    pub(crate) fn display_rec(
        this: &Rc<RefCell<Self>>,
        f: &mut impl fmt::Write,
        depth: usize,
        depths_iter: &mut VecDeque<Vec<usize>>,
        curri: &mut usize,
        fruits: &mut VecDeque<(usize, Rc<RefCell<Self>>)>,
    ) -> fmt::Result {
        let thisref = this.borrow();
        let next_depths = depths_iter.pop_front().unwrap_or_default();
        if let Some(label) = thisref.formulae.first() {
            let mut js = next_depths.iter().cloned();
            let mut i = 0;
            let mut skip = false;
            while let Some(j) = js.next() {
                while i < j - 1 {
                    write!(f, "  ")?;
                    i += 1;
                }
                if j == depth {
                    write!(f, "┣━")?;
                    i += 1;
                    skip = true;
                    break;
                } else {
                    write!(f, "┃ ")?;
                    i += 1;
                }
            }
            if !skip && depth > 0 {
                while i < depth - 1 {
                    write!(f, "  ")?;
                    i += 1;
                }
                write!(f, "┗━")?;
            }
            writeln!(f, "✱ {}", label.formula)?;
        }
        for label in &thisref.formulae[1..] {
            let mut js = next_depths.iter().cloned();
            let mut i = 0;
            while let Some(j) = js.next() {
                while i < j - 1 {
                    write!(f, "  ")?;
                    i += 1;
                }
                write!(f, "┃ ")?;
                i += 1;
            }
            while i < depth {
                write!(f, "  ")?;
                i += 1;
            }
            writeln!(f, "✱ {}", label.formula)?;
        }
        match &thisref.children {
            TabChildren::Leaf => {}
            TabChildren::Fork { branches, .. } => {
                for branch in branches {
                    Self::display_rec(&branch.node, f, depth + 1, depths_iter, curri, fruits)?
                }
            }
            TabChildren::Transition(transit) => {
                Self::display_transition(f, transit.feasibility(), depth + 1, depths_iter, *curri)?;
                fruits.push_back((*curri, this.clone()));
                *curri += 1;
            }
        }
        Ok(())
    }

    fn display_transition(
        f: &mut impl fmt::Write,
        feasiblity: Feasibility,
        depth: usize,
        depths_iter: &mut VecDeque<Vec<usize>>,
        rooti: usize,
    ) -> fmt::Result {
        let next_depths = depths_iter.pop_front().unwrap_or_default();
        let mut js = next_depths.iter().cloned();
        let mut i = 0;
        let mut skip = false;
        while let Some(j) = js.next() {
            while i < j - 1 {
                write!(f, "  ")?;
                i += 1;
            }
            if j == depth {
                write!(f, "┣━")?;
                i += 1;
                skip = true;
                break;
            } else {
                write!(f, "┃ ")?;
                i += 1;
            }
        }
        if !skip && depth > 0 {
            while i < depth - 1 {
                write!(f, "  ")?;
                i += 1;
            }
            write!(f, "┗━")?;
        }
        writeln!(f, "➤ {rooti} {}", feasiblity.symbol())
    }
}

pub(crate) struct DisplayTableau<T>(pub(crate) Rc<RefCell<TableauNode2<T>>>);

impl<T: DisplayTransit> fmt::Display for DisplayTableau<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 1;
        let mut seeds = VecDeque::new();
        writeln!(f, "0: {}", self.0.borrow().feasibility.symbol())?;
        TableauNode2::display_root(&self.0, f, &mut i, &mut seeds)?;
        writeln!(f)?;
        writeln!(f)?;
        while let Some((seedi, seed)) = seeds.pop_front() {
            if let TabChildren::Transition(transit) = &seed.borrow().children {
                transit.display_transit(f, seedi, &mut i, &mut seeds)?;
            }
        }
        Ok(())
    }
}

impl DisplayTransit for TransitKOr45 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        if self.feasibility.is_bad() {
            writeln!(f, "No solution")?
        } else {
            write!(f, "Solution: ")?;
            for (i, val) in self.solution.iter().enumerate() {
                write!(f, "{val}*w{i} ")?;
            }
            writeln!(f)?;
        }
        writeln!(f)
    }
}

impl DisplayTransit for TransitT {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(
            f,
            "{rooti} {}: {}",
            if self.reflexion { "[self]" } else { "" },
            self.feasibility.symbol()
        )?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        if self.reflexion {
            return writeln!(f);
        }
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        if self.is_closed() {
            writeln!(f, "No solution")?
        } else {
            write!(f, "Solution: ")?;
            for (i, val) in self.solution.iter().enumerate() {
                write!(f, "{val}*w{i} ")?;
            }
            writeln!(f)?;
        }
        writeln!(f)
    }
}

impl DisplayTransit for TransitB5 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.reflexion.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        for (i, choice) in self.reflexion.choices.iter().enumerate() {
            write!(f, "u{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        if self.is_closed() {
            writeln!(f, "No solution")?
        } else {
            write!(f, "Solution: ")?;
            for (i, val) in self.solution.iter().enumerate() {
                if i == self.rfxsolution {
                    write!(f, "{val}*w{i}+u ")?;
                } else {
                    write!(f, "{val}*w{i} ")?;
                }
            }
            writeln!(f)?;
        }
        writeln!(f)
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

impl DisplayTransit for Transit4 {
    fn display_transit(
        &self,
        f: &mut fmt::Formatter<'_>,
        rooti: usize,
        curri: &mut usize,
        roots: &mut VecDeque<(usize, Rc<RefCell<TableauNode2<Self>>>)>,
    ) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "{rooti}: {}", self.feasibility.symbol())?;
        writeln!(f, "{}", self.constraints)?;
        writeln!(f)?;
        TableauNode2::display_root(&self.paraws.tab, f, curri, roots)?;
        writeln!(f)?;
        for (i, choice) in self.paraws.choices.iter().enumerate() {
            write!(f, "w{i}: ")?;
            for (forkid, branchid) in choice {
                write!(f, "{}φ{forkid} ", if *branchid == 0 { "¬" } else { "" })?;
            }
            writeln!(f)?;
        }
        if self.feasibility.is_bad() {
            writeln!(f, "No solution")?
        } else {
            write!(f, "Solution: ")?;
            for (i, val) in self.solution.iter().enumerate() {
                write!(f, "{val}*w{i} ")?;
            }
            writeln!(f)?;
        }
        writeln!(f)
    }
}

pub(crate) enum DupContra {
    Ok,
    Bottom,
    Dup(Vec<Conflict>),
    Contra(Vec<Conflict>),
}
