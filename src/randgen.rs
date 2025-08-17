use std::{fmt, ops::RangeInclusive};

use crate::formula::Formula;

const NUM_PROPS: [usize; 8] = [2, 3, 4, 5, 6, 7, 8, 9];
const CONJ_SIZE: [RangeInclusive<usize>; 6] = [2..=3, 2..=5, 2..=8, 3..=5, 3..=8, 5..=8];
const DISJ_SIZE: [RangeInclusive<usize>; 6] = [2..=3, 2..=5, 2..=8, 3..=5, 3..=8, 5..=8];
const P_NEST: [[f64; 2]; 8] = [
    [0.10, 0.20],
    [0.11, 0.33],
    [0.10, 0.50],
    [0.22, 0.33],
    [0.25, 0.50],
    [0.33, 0.67],
    [0.40, 0.50],
    [0.40, 0.80],
];
const DEPTH: [RangeInclusive<usize>; 10] = [
    0..=0,
    0..=1,
    0..=2,
    0..=3,
    1..=1,
    1..=2,
    1..=3,
    2..=2,
    2..=3,
    3..=3,
];
const P_NEG: [f64; 4] = [0.0, 0.1, 0.25, 0.5];
const P_MODAL: [[f64; 3]; 9] = [
    [0.1, 0.2, 0.3],
    [0.1, 0.2, 0.9],
    [0.1, 0.8, 0.9],
    [0.7, 0.8, 0.9],
    [0.1, 0.2, 0.6],
    [0.4, 0.8, 0.9],
    [0.1, 0.5, 0.9],
    [0.4, 0.5, 0.6],
    [0.25, 0.5, 0.75],
];
const GRADE: [RangeInclusive<u32>; 1] = [2..=5];

struct Settings {
    num_props: usize,
    conj_size: RangeInclusive<usize>,
    disj_size: RangeInclusive<usize>,
    p_nest: [f64; 2],
    depth: RangeInclusive<usize>,
    p_neg: f64,
    p_modal: [f64; 3],
    min_grade: u32,
    max_grade: u32,
}

fn rand_gen_n(n: usize, out: &mut impl fmt::Write) {
    todo!()
}

fn rand_gen_one(setting: Settings) -> Formula {
    todo!()
}

fn rand_setting() -> Settings {
    todo!()
}
