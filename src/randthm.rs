use std::{
    cmp::{self, max},
    rc::Rc,
};

use good_lp::constraint::eq;
use rand::Rng;

use crate::{
    formula::Formula,
    frame::FrameCondition,
    randgen::{
        Atom, Conj, Disj, DisjConn, GRADE, Modal, Phi, Unit, rand_choice, rand_choice_weighted,
    },
};

fn rand_thm(rng: &mut impl Rng) -> Rc<Formula> {
    let symbols = rand_symbols(rng);
    let frames = *rand_choice(&FrameCondition::array(), rng);
    rand_true(&symbols, rng, frames, 2)
}

fn rand_true(
    symbols: &[Rc<Formula>],
    rng: &mut impl Rng,
    frames: FrameCondition,
    deeper: u8,
) -> Rc<Formula> {
    if deeper <= 1 || rng.random_range(0..=1) == 1 {
        let phi = rand_any(symbols, rng);
        let deeper = deeper.saturating_sub(1);
        match rng.random_range(0..=7) {
            0 => nec(&phi, rng, deeper).imply(&suff(&phi, rng, deeper)),
            1 => suff(&phi, rng, deeper).or(&nec(&phi, rng, deeper).not()),
            2 => eqv(&phi, rng, deeper).iff(&eqv(&phi, rng, deeper)),
            3 => {
                let c = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c);
                rand_any(symbols, rng)
                    .dmge(c)
                    .and(&nec(&phi, rng, deeper))
                    .imply(&suff(&phi, rng, deeper).dmge(c2))
            }
            4 => {
                let c = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c);
                nec(&phi, rng, deeper)
                    .dmle(c)
                    .or(&suff(&phi, rng, deeper).dmge(c2))
            }
            5 => {
                let phi2 = rand_any(symbols, rng);
                nec(&phi, rng, deeper)
                    .imply(&suff(&phi2, rng, deeper))
                    .box_()
                    .imply(
                        &nec(&phi, rng, deeper)
                            .box_()
                            .imply(&suff(&phi2, rng, deeper).box_()),
                    )
            }
            6 if frames.reflexive() => {
                nec(&phi.box_(), rng, deeper).imply(&suff(&phi, rng, deeper))
            }
            6 if frames.serial() => {
                nec(&phi.box_(), rng, deeper).imply(&suff(&phi.diamond(), rng, deeper))
            }
            7 if frames.symmetric() => {
                let c = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c);
                nec(&phi, rng, deeper)
                    .and(&rand_any(symbols, rng).dmge(c))
                    .imply(&suff(&phi.diamond().dmge(c2), rng, deeper))
            }
            7 if frames.euclidean() => {
                let c1 = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c1);
                let c3 = rng.random_range(1..=c1);
                nec(&phi.dmge(c1), rng, deeper).imply(&suff(&phi.dmge(c2).dmge(c3), rng, deeper))
            }
            7 if frames.transitive() => {
                let c1 = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(GRADE) as u32;
                let c3 = rng.random_range(1..=max(c1, c2));
                nec(&phi.dmge(c1).dmge(c2), rng, deeper).imply(&suff(&phi.dmge(c3), rng, deeper))
            }
            _ => unreachable!("Only values in 0..=7 should occur."),
        }
    } else {
        let thm = rand_true(symbols, rng, frames, deeper.saturating_sub(1));
        match rng.random_range(0..=5) {
            0 => thm.box_(),
            1 => thm.not().dmle(rng.random_range(GRADE) as u32 - 1),
            2 if frames.serial() => thm.box_(),
            2 => thm.not().not(),
            3 => thm.not().imply(&rand_any(symbols, rng)),
            4 => rand_any(symbols, rng).or(&thm),
            5 => rand_any(symbols, rng).imply(&thm),
            _ => unreachable!("Only values in 0..=5 should occur."),
        }
    }
}

fn nec(formula: &Rc<Formula>, rng: &mut impl Rng, deeper: u8) -> Rc<Formula> {
    if deeper == 0 {
        return formula.clone();
    }
    todo!()
}

fn suff(formula: &Rc<Formula>, rng: &mut impl Rng, deeper: u8) -> Rc<Formula> {
    if deeper == 0 {
        return formula.clone();
    }
    todo!()
}

fn eqv(formula: &Rc<Formula>, rng: &mut impl Rng, deeper: u8) -> Rc<Formula> {
    if deeper == 0 {
        return formula.clone();
    }
    todo!()
}

fn rand_any(symbols: &[Rc<Formula>], rng: &mut impl Rng) -> Rc<Formula> {
    match rand_modal(rng) {
        Modal::Bx => rand_inner_any(symbols, rng).box_(),
        Modal::Dm => rand_inner_any(symbols, rng).diamond(),
        Modal::Ge(c) => rand_inner_any(symbols, rng).dmge(c as u32),
        Modal::Le(c) => rand_inner_any(symbols, rng).dmle(c as u32),
    }
}

fn rand_inner_any(symbols: &[Rc<Formula>], rng: &mut impl Rng) -> Rc<Formula> {
    match rand_choice_weighted(&[0u8, 1, 2], &[4u8, 2, 1], rng) {
        0 => rand_unit(symbols, rng)
            .and(&rand_unit(symbols, rng))
            .or(&rand_unit(symbols, rng).and(&rand_unit(symbols, rng))),
        1 => {
            rand_unit(symbols, rng).imply(&rand_unit(symbols, rng).imply(&rand_unit(symbols, rng)))
        }
        2 => rand_unit(symbols, rng).iff(&rand_unit(symbols, rng)),
        _ => unreachable!("Only values in 0..=2 should occur."),
    }
}

fn rand_unit(symbols: &[Rc<Formula>], rng: &mut impl Rng) -> Rc<Formula> {
    if rng.random_ratio(1, 3) {
        match rand_modal(rng) {
            Modal::Bx => rand_atom(symbols, rng).box_(),
            Modal::Dm => rand_atom(symbols, rng).diamond(),
            Modal::Ge(c) => rand_atom(symbols, rng).dmge(c as u32),
            Modal::Le(c) => rand_atom(symbols, rng).dmle(c as u32),
        }
    } else {
        rand_atom(symbols, rng)
    }
}

const NUM_SYMBOLS: usize = 10;
fn rand_symbols(rng: &mut impl Rng) -> [Rc<Formula>; NUM_SYMBOLS] {
    std::array::from_fn(|_| rand_symbol(rng))
}

fn rand_symbol(rng: &mut impl Rng) -> Rc<Formula> {
    let sign = rng.random_ratio(1, 2);
    let letter = (b'a' + rng.random_range(0..26)) as char;
    let index = rng.random_range(0..=10);
    let index = if index == 10 { None } else { Some(index) };
    let p = Rc::new(Formula::PropVar(letter, index));
    if !sign { p.not() } else { p }
}

fn rand_atom(symbols: &[Rc<Formula>], rng: &mut impl Rng) -> Rc<Formula> {
    if rng.random_ratio(1, 200) {
        rand_choice(&[Formula::bottom(), Formula::top()], rng).clone()
    } else {
        rand_choice(symbols, rng).clone()
    }
}

fn rand_modal(rng: &mut impl Rng) -> Modal {
    match rand_choice_weighted(&[0u8, 1, 2, 3], &[3u8, 2u8, 5u8, 5u8], rng) {
        0 => Modal::Bx,
        1 => Modal::Dm,
        2 => Modal::Ge(rng.random_range(GRADE)),
        3 => Modal::Le(rng.random_range(GRADE) - 1),
        _ => unreachable!("Only values in 0..=3 should occur."),
    }
}
