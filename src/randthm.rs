use std::rc::Rc;

use rand::Rng;

use crate::{
    formula::Formula,
    frame::FrameCondition,
    randgen::{GRADE, Modal, rand_choice, rand_choice_weighted},
};

pub(crate) fn rand_thm(rng: &mut impl Rng) -> (Rc<Formula>, FrameCondition) {
    let symbols = rand_symbols(rng);
    let frames = *rand_choice(&FrameCondition::array(), rng);
    (rand_true(&symbols, rng, frames, 2), frames)
}

fn rand_true(
    symbols: &[Rc<Formula>],
    rng: &mut impl Rng,
    frames: FrameCondition,
    deeper: u8,
) -> Rc<Formula> {
    let deeper = deeper.saturating_sub(1);
    if deeper == 0 || rng.random_range(0..=1) == 1 {
        let phi = rand_any(symbols, rng);
        match rng.random_range(0..=6) {
            0 => nec(&phi, symbols, rng, frames, deeper)
                .imply(&suff(&phi, symbols, rng, frames, deeper)),
            1 => suff(&phi, symbols, rng, frames, deeper)
                .or(&nec(&phi, symbols, rng, frames, deeper).not()),
            2 => eqv(&phi, symbols, rng, frames, deeper)
                .iff(&eqv(&phi, symbols, rng, frames, deeper)),
            3 => {
                let c = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c);
                rand_any(symbols, rng)
                    .dmge(c)
                    .and(&nec(&phi, symbols, rng, frames, deeper).box_())
                    .imply(&suff(&phi, symbols, rng, frames, deeper).dmge(c2))
            }
            4 => {
                let c = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c);
                nec(&phi, symbols, rng, frames, deeper)
                    .dmle(c)
                    .or(&suff(&phi, symbols, rng, frames, deeper).dmge(c2))
            }
            5 if frames.reflexive() => nec(&phi.box_(), symbols, rng, frames, deeper)
                .imply(&suff(&phi, symbols, rng, frames, deeper)),
            5 if frames.serial() => suff(&phi, symbols, rng, frames, deeper).diamond().or(&nec(
                &phi.not(),
                symbols,
                rng,
                frames,
                deeper,
            )
            .diamond()),
            6 if frames.symmetric() => {
                let c = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c);
                nec(&phi, symbols, rng, frames, deeper)
                    .and(&rand_any(symbols, rng).dmge(c))
                    .imply(&suff(&phi.diamond().dmge(c2), symbols, rng, frames, deeper))
            }
            6 if frames.euclidean() => {
                let c1 = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(1..=c1);
                let c3 = rng.random_range(1..=c1);
                nec(&phi.dmge(c1), symbols, rng, frames, deeper).imply(&suff(
                    &phi.dmge(c2).dmge(c3),
                    symbols,
                    rng,
                    frames,
                    deeper,
                ))
            }
            6 if frames.transitive() => {
                let c1 = rng.random_range(GRADE) as u32;
                let c2 = rng.random_range(GRADE) as u32;
                let c3 = rng.random_range(1..=c1);
                nec(&phi.dmge(c1).dmge(c2), symbols, rng, frames, deeper).imply(&suff(
                    &phi.dmge(c3),
                    symbols,
                    rng,
                    frames,
                    deeper,
                ))
            }
            5 | 6 => {
                let phi2 = rand_any(symbols, rng);
                nec(&phi, symbols, rng, frames, deeper)
                    .imply(&suff(&phi2, symbols, rng, frames, deeper))
                    .box_()
                    .imply(
                        &nec(&phi, symbols, rng, frames, deeper)
                            .box_()
                            .imply(&suff(&phi2, symbols, rng, frames, deeper).box_()),
                    )
            }
            _ => unreachable!("Only values in 0..=6 should occur."),
        }
    } else {
        let thm = rand_true(symbols, rng, frames, deeper);
        match rng.random_range(0..=5) {
            0 => thm.box_(),
            1 => thm.not().dmle(rng.random_range(GRADE) as u32 - 1),
            2 => thm.not().not(),
            3 => thm.not().imply(&rand_any(symbols, rng)),
            4 => rand_any(symbols, rng).or(&thm),
            5 => rand_any(symbols, rng).imply(&thm),
            _ => unreachable!("Only values in 0..=5 should occur."),
        }
    }
}

fn nec(
    formula: &Rc<Formula>,
    symbols: &[Rc<Formula>],
    rng: &mut impl Rng,
    frames: FrameCondition,
    deeper: u8,
) -> Rc<Formula> {
    if deeper == 0 {
        return formula.clone();
    }
    let deeper = deeper.saturating_sub(1);
    match rand_choice_weighted(&[0, 1, 2], &[2, 6, 2], rng) {
        0 => rand_any(symbols, rng).and(&nec(formula, symbols, rng, frames, deeper)),
        1 => match formula.as_ref() {
            Formula::Top => rand_any(symbols, rng),
            Formula::Bottom | Formula::PropVar(_, _) | Formula::Iff(_, _) => formula.clone(),
            Formula::Not(phi) => suff(phi, symbols, rng, frames, deeper).not(),
            Formula::Box(phi) => nec(phi, symbols, rng, frames, deeper).box_(),
            Formula::Diamond(phi) => {
                nec(phi, symbols, rng, frames, deeper).dmge(rng.random_range(GRADE) as u32)
            }
            Formula::DiamondGe(c, phi) => {
                let c2 = rng.random_range(*c..=(*GRADE.end() as u32));
                nec(phi, symbols, rng, frames, deeper).dmge(c2)
            }
            Formula::DiamondLe(c, phi) => {
                let c2 = rng.random_range(1..=*c);
                suff(phi, symbols, rng, frames, deeper).dmle(c2)
            }
            Formula::And(phi1, phi2) => nec(phi1, symbols, rng, frames, deeper)
                .and(&nec(phi2, symbols, rng, frames, deeper)),
            Formula::Or(phi1, phi2) => {
                if rng.random_ratio(1, 2) {
                    nec(phi1, symbols, rng, frames, deeper)
                } else {
                    nec(phi2, symbols, rng, frames, deeper)
                }
            }
            Formula::Imply(phi1, phi2) => suff(phi1, symbols, rng, frames, deeper)
                .imply(&nec(phi2, symbols, rng, frames, deeper)),
        },
        2 => formula.clone(),
        _ => unreachable!("Only values in 0..=2 should occur."),
    }
}

fn suff(
    formula: &Rc<Formula>,
    symbols: &[Rc<Formula>],
    rng: &mut impl Rng,
    frames: FrameCondition,
    deeper: u8,
) -> Rc<Formula> {
    if deeper == 0 {
        return formula.clone();
    }
    let deeper = deeper.saturating_sub(1);
    match rand_choice_weighted(&[0, 1, 2, 3, 4], &[1, 1, 6, 1, 1], rng) {
        0 => rand_any(symbols, rng).or(&suff(formula, symbols, rng, frames, deeper)),
        1 => rand_any(symbols, rng).imply(&suff(formula, symbols, rng, frames, deeper)),
        2 => match formula.as_ref() {
            Formula::Bottom => rand_any(symbols, rng),
            Formula::Top | Formula::PropVar(_, _) | Formula::Iff(_, _) => formula.clone(),
            Formula::Not(phi) => nec(phi, symbols, rng, frames, deeper).not(),
            Formula::Box(phi) => suff(phi, symbols, rng, frames, deeper).box_(),
            Formula::Diamond(phi) => suff(phi, symbols, rng, frames, deeper).diamond(),
            Formula::DiamondGe(c, phi) => {
                suff(phi, symbols, rng, frames, deeper).dmge(rng.random_range(1..=*c))
            }
            Formula::DiamondLe(c, phi) => suff(phi, symbols, rng, frames, deeper)
                .dmle(rng.random_range(*c..=(*GRADE.end() as u32))),
            Formula::And(phi1, phi2) => {
                if rng.random_ratio(1, 2) {
                    suff(phi1, symbols, rng, frames, deeper)
                } else {
                    suff(phi2, symbols, rng, frames, deeper)
                }
            }
            Formula::Or(phi1, phi2) => suff(phi1, symbols, rng, frames, deeper)
                .or(&suff(phi2, symbols, rng, frames, deeper)),
            Formula::Imply(phi1, phi2) => nec(phi1, symbols, rng, frames, deeper)
                .imply(&suff(phi2, symbols, rng, frames, deeper)),
        },
        i @ (3 | 4) => match formula.as_ref() {
            Formula::Box(phi) if *i == 2 && frames.serial() => {
                suff(&phi.diamond(), symbols, rng, frames, deeper)
            }
            Formula::Box(phi) if *i == 2 && frames.reflexive() => {
                suff(&phi, symbols, rng, frames, deeper)
            }
            Formula::Box(phi) if *i == 3 && frames.symmetric() => suff(
                &phi.diamond().diamond().box_(),
                symbols,
                rng,
                frames,
                deeper,
            ),
            Formula::Box(phi) if *i == 3 && frames.euclidean() => {
                suff(&phi.diamond().box_().box_(), symbols, rng, frames, deeper)
            }
            Formula::Box(_) if *i == 3 && frames.transitive() => {
                suff(&formula.box_(), symbols, rng, frames, deeper)
            }
            _ => formula.clone(),
        },
        _ => unreachable!("Only values in 0..=4 should occur."),
    }
}

fn eqv(
    formula: &Rc<Formula>,
    symbols: &[Rc<Formula>],
    rng: &mut impl Rng,
    frames: FrameCondition,
    deeper: u8,
) -> Rc<Formula> {
    if deeper == 0 {
        return formula.clone();
    }
    let deeper = deeper.saturating_sub(1);
    match rand_choice_weighted(&[0, 1, 2, 3], &[1, 3, 1, 4], rng) {
        0 => {
            rand_true(symbols, rng, frames, deeper).and(&eqv(formula, symbols, rng, frames, deeper))
        }
        1 => rand_true(symbols, rng, frames, deeper)
            .not()
            .or(&eqv(formula, symbols, rng, frames, deeper)),
        2 => formula.not().not(),
        3 => formula.clone(),
        _ => unreachable!("Only values in 0..=3 should occur."),
    }
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
