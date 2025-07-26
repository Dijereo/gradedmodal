use std::rc::Rc;

use crate::formula::Formula;

enum MD1NF {
    Disj(Box<MD1NF>, Box<MD1NF>),
    Conj(Box<MD1NF>, Box<MD1NF>),
    Var(bool, char),
    VarI(bool, char, usize),
    Bool(bool),
    Dm(Box<MD1NF>),
    Bx(Box<MD1NF>),
    Ge(u32, Box<MD1NF>),
    Le(u32, Box<MD1NF>),
}

fn to_md1nf(formula: &Rc<Formula>) -> MD1NF {
    let mut md1f = init(formula);
    to_md1nf_rec(&mut md1f);
    md1f
}

fn to_md1nf_rec(formula: &mut MD1NF) {
    match formula {
        MD1NF::Disj(phi0, phi1) | MD1NF::Conj(phi0, phi1) => {
            to_md1nf_rec(phi0);
            to_md1nf_rec(phi1);
        }
        MD1NF::Var(_, _) | MD1NF::VarI(_, _, _) | MD1NF::Bool(_) => {}
        MD1NF::Dm(phi) => {
            if let Some((psinest, psi0, psi1)) = to_md1(phi) {
                let mut psi0 = MD1NF::Dm(Box::new(psi0));
                to_md1nf_rec(&mut psi0);
                let mut psi1 = MD1NF::Dm(Box::new(psi1));
                to_md1nf_rec(&mut psi1);
                *formula = MD1NF::Disj(
                    Box::new(psi1),
                    Box::new(MD1NF::Conj(Box::new(psinest), Box::new(psi0))),
                );
            }
        }
        MD1NF::Bx(phi) => todo!(),
        MD1NF::Ge(c, phi) => {
            if let Some((nest, psi0, psi1)) = to_md1(phi) {
                let mut psi0 = MD1NF::Ge(*c, Box::new(psi0));
                to_md1nf_rec(&mut psi0);
                let mut psi1 = MD1NF::Ge(*c, Box::new(psi1));
                to_md1nf_rec(&mut psi1);
                *formula = MD1NF::Disj(
                    Box::new(psi1),
                    Box::new(MD1NF::Conj(Box::new(nest), Box::new(psi0))),
                );
            }
        }
        MD1NF::Le(_, phi) => todo!(),
    }
}

fn to_md1(formula: &mut MD1NF) -> Option<(MD1NF, MD1NF, MD1NF)> {
    todo!()
}

fn break_disj(formula: MD1NF) -> MD1NF {
    todo!()
}

fn break_conj(formula: MD1NF) -> MD1NF {
    todo!()
}

fn init(formula: &Rc<Formula>) -> MD1NF {
    match formula.as_ref() {
        Formula::Bottom => MD1NF::Bool(false),
        Formula::Top => MD1NF::Bool(true),
        Formula::PropVar(p, Some(i)) => MD1NF::VarI(true, *p, *i as usize),
        Formula::PropVar(p, None) => MD1NF::Var(true, *p),
        Formula::Not(phi) => init_neg(phi),
        Formula::Box(phi) => MD1NF::Bx(Box::new(init(phi))),
        Formula::Diamond(phi) => MD1NF::Dm(Box::new(init(phi))),
        Formula::DiamondGe(c, phi) => MD1NF::Ge(*c, Box::new(init(phi))),
        Formula::DiamondLe(c, phi) => MD1NF::Le(*c, Box::new(init(phi))),
        Formula::And(phi0, phi1) => MD1NF::Conj(Box::new(init(phi0)), Box::new(init(phi1))),
        Formula::Or(phi0, phi1) => MD1NF::Disj(Box::new(init(phi0)), Box::new(init(phi1))),
        Formula::Imply(phi0, phi1) => MD1NF::Disj(Box::new(init_neg(phi0)), Box::new(init(phi1))),
        Formula::Iff(phi0, phi1) => MD1NF::Disj(
            Box::new(MD1NF::Conj(Box::new(init(phi0)), Box::new(init(phi1)))),
            Box::new(MD1NF::Conj(
                Box::new(init_neg(phi0)),
                Box::new(init_neg(phi1)),
            )),
        ),
    }
}

fn init_neg(formula: &Rc<Formula>) -> MD1NF {
    match formula.as_ref() {
        Formula::Bottom => MD1NF::Bool(true),
        Formula::Top => MD1NF::Bool(false),
        Formula::PropVar(p, Some(i)) => MD1NF::VarI(false, *p, *i as usize),
        Formula::PropVar(p, None) => MD1NF::Var(false, *p),
        Formula::Not(phi) => init(phi),
        Formula::Box(phi) => MD1NF::Dm(Box::new(init_neg(phi))),
        Formula::Diamond(phi) => MD1NF::Bx(Box::new(init_neg(phi))),
        Formula::DiamondGe(c, phi) => MD1NF::Le(*c - 1, Box::new(init(phi))),
        Formula::DiamondLe(c, phi) => MD1NF::Ge(*c + 1, Box::new(init(phi))),
        Formula::And(phi0, phi1) => MD1NF::Disj(Box::new(init_neg(phi0)), Box::new(init_neg(phi1))),
        Formula::Or(phi0, phi1) => MD1NF::Conj(Box::new(init_neg(phi0)), Box::new(init_neg(phi1))),
        Formula::Imply(phi0, phi1) => MD1NF::Conj(Box::new(init(phi0)), Box::new(init_neg(phi1))),
        Formula::Iff(phi0, phi1) => MD1NF::Disj(
            Box::new(MD1NF::Conj(Box::new(init(phi0)), Box::new(init_neg(phi1)))),
            Box::new(MD1NF::Conj(Box::new(init_neg(phi0)), Box::new(init(phi1)))),
        ),
    }
}
