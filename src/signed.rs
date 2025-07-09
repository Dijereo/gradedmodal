use std::{mem::take, rc::Rc};

use crate::formula::Formula;

type SignedFormula = (bool, Rc<Formula>);

#[derive(Debug)]
pub(crate) struct SignedTableau {
    is_closed: Option<bool>,
    formulae: Vec<SignedFormula>,
    children: Vec<SignedTableau>,
}

enum Expansion {
    Sequence(Vec<SignedFormula>),
    Split(SignedFormula, SignedFormula, Vec<SignedFormula>),
}

impl SignedTableau {
    pub(crate) fn create(signedformula: SignedFormula) -> Self {
        let mut tab = Self::new(signedformula);
        tab.expand(vec![]);
        tab.set_openess();
        tab
    }

    fn new(signedformula: SignedFormula) -> Self {
        Self {
            is_closed: None,
            formulae: vec![signedformula],
            children: vec![],
        }
    }

    fn set_openess(&mut self) -> bool {
        let mut ancestors = vec![];
        self.set_openess_rec(&mut ancestors)
    }

    fn set_openess_rec<'a>(&'a mut self, ancestors: &mut Vec<&'a Vec<SignedFormula>>) -> bool {
        for (i, (sign1, form1)) in self.formulae.iter().enumerate() {
            if *sign1 && form1.is_bottom() {
                self.set_closed();
                return false;
            }
            for (sign2, form2) in &self.formulae[i + 1..] {
                // println!(
                //     "{sign1} {} vs {sign2} {} = {}",
                //     form1,
                //     form2,
                //     (*sign1 != *sign2) && form1 == form2
                // );
                if (*sign1 != *sign2) && form1 == form2 {
                    self.set_closed();
                    return false;
                }
            }
        }
        for (sign1, form1) in &self.formulae {
            for &ancestor in ancestors.iter() {
                for (sign2, form2) in ancestor {
                    // println!(
                    //     "{sign1} {} vs {sign2} {} = {}",
                    //     form1,
                    //     form2,
                    //     (*sign1 != *sign2) && form1 == form2
                    // );
                    if (*sign1 != *sign2) && form1 == form2 {
                        self.set_closed();
                        return false;
                    }
                }
            }
        }
        if self.children.is_empty() {
            self.is_closed = Some(false);
            // println!("Opening Leaf");
            return true;
        }
        ancestors.push(&self.formulae);
        for child in &mut self.children {
            let opened = child.set_openess_rec(ancestors);
            if opened {
                // Might not want to break out early to eval entire tree
                self.is_closed = Some(false);
                // println!("Opening From Child");
                ancestors.pop();
                return true;
            }
        }
        ancestors.pop();
        self.is_closed = Some(true);
        // println!("All Children Closing");
        return false;
    }

    fn set_closed(&mut self) {
        self.is_closed = Some(true);
        // println!("Set SubTree Closed");
        for child in &mut self.children {
            child.set_closed();
        }
    }

    fn expand(&mut self, mut splits: Vec<(SignedFormula, SignedFormula, Vec<SignedFormula>)>) {
        // println!("Formula");
        // for (sign, f) in &self.formulae {
        // println!("{sign} {f}");
        // }
        // println!("Splits");
        // for (sf0, sf1, sfs) in &splits {
        // for (sign, f) in sfs {
        // println!("{sign} {f}");
        // }
        // }

        // println!("Splits Raw: {:?}", splits);
        let mut processed = vec![];
        let mut queue = take(&mut self.formulae);
        while let Some(sform) = queue.pop() {
            match Self::expand_once(&sform) {
                Expansion::Sequence(mut sfs) => queue.append(&mut sfs),
                Expansion::Split(sf0, sf1, sfs) => {
                    splits.push((sf0, sf1, sfs));
                }
            }
            processed.push(sform);
        }
        self.formulae = processed;

        // println!("Expanded");
        // for (sign, f) in &self.formulae {
        // println!("{sign} {f}");
        // }
        // println!("New Splits");
        // for ((sign0, f0), (sign1, f1), sfs) in &splits {
        // println!("{sign0} {f0}");
        // println!("{sign1} {f1}");
        // for (sign, f) in sfs {
        // println!("{sign} {f}");
        // }
        // }
        self.add_splits(splits);
    }

    fn add_splits(&mut self, mut splits: Vec<(SignedFormula, SignedFormula, Vec<SignedFormula>)>) {
        // println!("Splits Raw: {:?}", splits);
        if let Some((sf0, sf1, sfs)) = splits.pop() {
            // println!("Splitting");
            // println!("{} {}", sf0.0, sf0.1);
            self.children.push(SignedTableau::new(sf0));
            // println!("{} {}", sf1.0, sf1.1);
            self.children.push(SignedTableau::new(sf1));
            for sf in sfs {
                // println!("{} {}", sf.0, sf.1);
                self.children.push(SignedTableau::new(sf));
            }
        }
        // println!();
        // println!();
        for child in &mut self.children {
            child.expand(splits.clone());
        }
    }

    fn expand_once(signedformula: &SignedFormula) -> Expansion {
        let (sign, f) = signedformula;
        match (f.as_ref(), sign) {
            (Formula::Bottom | Formula::PropVar(_) | Formula::Box(_) | Formula::Diamond(_), _) => {
                Expansion::Sequence(vec![])
            }
            (Formula::Top, true) => Expansion::Sequence(vec![]),
            (Formula::Top, false) => Expansion::Sequence(vec![(true, Rc::new(Formula::Bottom))]),
            (Formula::Not(formula), true) => Expansion::Sequence(vec![(false, formula.clone())]),
            (Formula::Not(formula), false) => Expansion::Sequence(vec![(true, formula.clone())]),
            (Formula::And(formula, formula1), true) => {
                Expansion::Sequence(vec![(true, formula.clone()), (true, formula1.clone())])
            }
            (Formula::And(formula, formula1), false) => {
                Expansion::Split((false, formula.clone()), (false, formula1.clone()), vec![])
            }
            (Formula::Or(formula, formula1), true) => {
                Expansion::Split((true, formula.clone()), (true, formula1.clone()), vec![])
            }
            (Formula::Or(formula, formula1), false) => {
                Expansion::Sequence(vec![(false, formula.clone()), (false, formula1.clone())])
            }
            (Formula::Imply(formula, formula1), true) => {
                Expansion::Split((false, formula.clone()), (true, formula1.clone()), vec![])
            }
            (Formula::Imply(formula, formula1), false) => {
                Expansion::Sequence(vec![(true, formula.clone()), (false, formula1.clone())])
            }
            (Formula::Iff(formula, formula1), true) => Expansion::Split(
                (true, formula.and(formula1)),
                (true, formula.not().and(&formula1.not())),
                vec![],
            ),
            (Formula::Iff(formula, formula1), false) => Expansion::Split(
                (true, formula.and(&formula1.not())),
                (true, formula.not().and(formula1)),
                vec![],
            ),
        }
    }

    pub(crate) fn to_table_string(&self) -> String {
        let mut out = String::new();
        let mut level_nodes = vec![self];
        while let Some(n) = level_nodes.iter().map(|node| node.formulae.len()).max() {
            for i in 0..n {
                for node in &level_nodes {
                    if let Some((sign, f)) = node.formulae.get(i) {
                        out.push_str(if *sign { "T " } else { "F " });
                        out.push_str(&f.to_string());
                    }
                    out.push_str(" |:| ");
                }
                out.push('\n');
            }
            level_nodes = level_nodes
                .into_iter()
                .flat_map(|node| &node.children)
                .collect();
        }
        out
    }

    pub(crate) fn to_tree_string(&self) -> String {
        let mut out = String::new();
        self.to_tree_string_rec(&mut out, 0);
        out
    }

    // pub(crate) fn to_tree_string_rec(&self, out: &mut String, indent: usize) {
    //     for _ in 1..indent {
    //         out.push_str("  ");
    //     }
    //     if indent > 0 {
    //         out.push_str("+ ");
    //     }
    //     for (sign, form) in &self.formulae {
    //         out.push_str(if *sign { "T: \"" } else { "F: \"" });
    //         out.push_str(&form.to_string());
    //         out.push_str("\"; ");
    //     }
    //     out.push_str("\n");
    //     for child in &self.children {
    //         child.to_tree_string_rec(out, indent + 1);
    //     }
    // }

    pub(crate) fn to_tree_string_rec(&self, out: &mut String, indent: usize) {
        for _ in 1..indent {
            out.push_str("  ");
        }
        if indent > 0 {
            out.push_str("+ ");
        }
        if let Some((sign, form)) = self.formulae.first() {
            out.push_str(if *sign { "T: " } else { "F: " });
            out.push_str(&form.to_string());
            out.push_str("\n");
        }
        for (sign, form) in &self.formulae[1..] {
            for _ in 0..indent {
                out.push_str("  ");
            }
            out.push_str(if *sign { "T: " } else { "F: " });
            out.push_str(&form.to_string());
            out.push_str("\n");
        }
        if let Some(true) = self.is_closed {
            for _ in 0..indent {
                out.push_str("  ");
            }
            out.push_str("_!_\n");
        }
        for child in &self.children {
            child.to_tree_string_rec(out, indent + 1);
        }
    }
}
