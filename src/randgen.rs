use std::{
    borrow::Cow,
    cmp,
    fmt::{self, Write},
    ops::{Index, RangeInclusive},
    path::Path,
};

use rand::{
    distr::{
        uniform::SampleUniform,
        weighted::{Weight, WeightedIndex},
    },
    prelude::*,
    rngs::StdRng,
};
use serde::{Deserialize, Serialize};

use crate::{
    eval::{DataPoint, EvalError, add_formulae, load_formulae, save_results},
    vecfor,
};

const NUM_PROPS: [u8; 5] = [2, 3, 4, 5, 10];
const CONJ_SIZE: [u8; 3] = [2, 3, 5];
const DISJ_SIZE: [u8; 3] = [2, 3, 5];
const W_NEST: [[u8; 3]; 4] = [[6, 1, 2], [6, 2, 1], [2, 1, 1], [1, 1, 1]];
const W_BOOL: [u8; 3] = [1, 1, 198];
const DEPTH: [u8; 4] = [0, 1, 2, 3];
const NUMER_NEG: [u8; 4] = [0, 2, 5, 10];
const DENOM_NEG: u8 = 20;
const W_MODAL: [[u8; 4]; 5] = [
    [1, 1, 4, 4],
    [4, 4, 1, 1],
    [1, 4, 4, 1],
    [4, 1, 1, 4],
    [1, 1, 1, 1],
];
const GRADE: RangeInclusive<u8> = 2..=5;
const IMPLY: [u8; 3] = [8, 1, 1];

#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct Setting {
    num_props: u8,
    conj_size: u8,
    disj_size: u8,
    w_nest: [u8; 3],
    depth: u8,
    numer_neg: u8,
    w_modal: [u8; 4],
}

pub(crate) fn gen_formulae(
    n: usize,
    seed: u64,
    craftedtxt: impl AsRef<Path>,
    datajson: impl AsRef<Path>,
) -> Result<(), EvalError> {
    let mut craftedformulae = vec![];
    load_formulae(craftedtxt, &mut craftedformulae)?;
    let mut datapoints: Vec<DataPoint<Cow<'_, str>, &str, &'static str>> = Vec::with_capacity(2 * n + craftedformulae.len());
    const INIT_KEYS: [&'static str; 2] = ["vampire", "prover"];
    add_formulae(&mut datapoints, craftedformulae.into_iter(), INIT_KEYS.into_iter());
    let mut rng = StdRng::seed_from_u64(seed);
    let mut buffers = vec!["¬(".to_string(); n];
    for buffer in &mut buffers {
        let setting = Setting::rand(&mut rng);
        setting.formula(&mut rng, buffer)?;
        write!(buffer, ")")?;
        let mut subbuffer = buffer.chars();
        subbuffer.nth(1);
        subbuffer.next_back();
        datapoints.push(DataPoint::new(
            Cow::Borrowed(subbuffer.as_str()),
            Some(setting.clone()),
            INIT_KEYS.into_iter(),
        ));
        datapoints.push(DataPoint::new(
            Cow::Borrowed(buffer.as_str()),
            Some(setting.clone()),
            INIT_KEYS.into_iter(),
        ));
    }
    save_results(&datapoints, datajson)?;
    Ok(())
}

impl Setting {
    fn rand(rng: &mut impl Rng) -> Self {
        Self {
            num_props: *rand_choice(&NUM_PROPS, rng),
            conj_size: rand_choice(&CONJ_SIZE, rng).clone(),
            disj_size: rand_choice(&DISJ_SIZE, rng).clone(),
            w_nest: *rand_choice(&W_NEST, rng),
            depth: *rand_choice(&DEPTH, rng),
            numer_neg: *rand_choice(&NUMER_NEG, rng),
            w_modal: *rand_choice(&W_MODAL, rng),
        }
    }

    fn formula(&self, rng: &mut impl Rng, out: &mut impl fmt::Write) -> fmt::Result {
        write!(out, "{}", Phi::new(&self, rng))
    }
}

fn rand_choice_weighted<'a, C, W>(
    choices: &'a C,
    weights: &[W],
    rng: &mut impl Rng,
) -> &'a C::Output
where
    C: Index<usize>,
    C::Output: Sized,
    W: SampleUniform + PartialOrd + Weight + fmt::Debug,
{
    &choices[WeightedIndex::new(weights)
        .expect(&format!("Invalid weights: {:?}", weights))
        .sample(rng)]
}

fn rand_choice<'a, T>(choices: &'a [T], rng: &mut impl Rng) -> &'a T {
    &choices[rng.random_range(0..choices.len())]
}

struct Phi(Disj);

struct Disj {
    conjs: Vec<Conj>,
    conn: DisjConn,
}

#[derive(Debug, Clone, Copy)]
enum DisjConn {
    Or,
    Imply,
    Iff,
}

struct Conj {
    units: Vec<Unit>,
}

enum Unit {
    A(Atom),
    M(Modal, Atom),
    Nest(Modal, Disj),
}

enum Modal {
    Bx,
    Dm,
    Ge(u8),
    Le(u8),
}

enum Atom {
    P(bool, u8),
    B(bool),
}

impl Phi {
    fn new(setting: &Setting, rng: &mut impl Rng) -> Self {
        let mut this = Self(Disj::new(setting.disj_size, 0, true, true, setting, rng));
        let numatoms = this.0.count_atoms();
        let mut atoms = vecfor!(
            _i in setting.num_props as usize..numatoms,
            cap=numatoms
            => Atom::new(setting, rng)
        );
        vecfor!(
            i in 0..cmp::min(numatoms, setting.num_props as usize) as u8,
            into atoms
            => Atom::P(Atom::rand_sign(setting, rng), i)
        );
        atoms.partial_shuffle(rng, cmp::min(numatoms, setting.num_props as usize));
        this.0.set_atoms(&mut atoms.into_iter());
        this
    }
}

impl Disj {
    fn new(
        len: u8,
        depth: u8,
        deep: bool,
        wide: bool,
        setting: &Setting,
        rng: &mut impl Rng,
    ) -> Self {
        let mut sublens =
            vecfor!(_i in 1..len, cap=len as usize => rng.random_range(1..=setting.conj_size));
        if wide {
            sublens.push(setting.conj_size);
            sublens.partial_shuffle(rng, 1);
        } else {
            sublens.push(rng.random_range(1..=setting.conj_size));
        };
        Self {
            conjs: vecfor!(
                sublen in sublens,
                cap=len as usize
                => Conj::new(sublen, depth, deep, setting, rng)
            ),
            conn: DisjConn::new(rng),
        }
    }

    fn count_atoms(&self) -> usize {
        self.conjs.iter().map(|c| c.count_atoms()).sum()
    }

    fn set_atoms(&mut self, atoms: &mut impl Iterator<Item = Atom>) {
        for conj in &mut self.conjs {
            conj.set_atoms(atoms);
        }
    }
}

impl Conj {
    fn new(len: u8, currdepth: u8, deep: bool, setting: &Setting, rng: &mut impl Rng) -> Self {
        if currdepth >= setting.depth {
            return Self {
                units: vecfor!(_i in 0..len => Unit::A(Atom::B(false))),
            };
        }
        let units = if deep {
            let deepunit = Unit::Nest(
                Modal::new(setting, rng),
                Disj::new(
                    rng.random_range(1..=setting.disj_size),
                    currdepth + 1,
                    true,
                    false,
                    setting,
                    rng,
                ),
            );
            let mut units =
                vecfor!(_i in 1..len, cap=len as usize => Unit::new(currdepth, setting, rng));
            vecfor!(i in [deepunit], into units);
            units.partial_shuffle(rng, 1);
            units
        } else {
            vecfor!(_i in 0..len => Unit::new(currdepth, setting, rng))
        };
        Self { units }
    }

    fn count_atoms(&self) -> usize {
        self.units.iter().map(|u| u.count_atoms()).sum()
    }

    fn set_atoms(&mut self, atoms: &mut impl Iterator<Item = Atom>) {
        for unit in &mut self.units {
            unit.set_atoms(atoms);
        }
    }
}

impl DisjConn {
    fn new(rng: &mut impl Rng) -> Self {
        *rand_choice_weighted(&[DisjConn::Or, DisjConn::Imply, DisjConn::Iff], &IMPLY, rng)
    }
}

impl Unit {
    fn new(currdepth: u8, setting: &Setting, rng: &mut impl Rng) -> Self {
        match rand_choice_weighted(&[0u8, 1, 2], &setting.w_nest, rng) {
            0 => Unit::A(Atom::B(false)),
            1 => Unit::M(Modal::new(setting, rng), Atom::B(false)),
            2 => Unit::Nest(
                Modal::new(setting, rng),
                Disj::new(
                    rng.random_range(1..=setting.disj_size),
                    currdepth + 1,
                    false,
                    false,
                    setting,
                    rng,
                ),
            ),
            _ => unreachable!("Only values in 0..=2 should occur."),
        }
    }

    fn count_atoms(&self) -> usize {
        match self {
            Unit::A(_) => 1,
            Unit::M(..) => 1,
            Unit::Nest(_, disj) => disj.count_atoms(),
        }
    }

    fn set_atoms(&mut self, atoms: &mut impl Iterator<Item = Atom>) {
        match self {
            Unit::A(atom) => atom.set_atom(atoms),
            Unit::M(_, atom) => atom.set_atom(atoms),
            Unit::Nest(_, disj) => disj.set_atoms(atoms),
        }
    }
}

impl Modal {
    fn new(setting: &Setting, rng: &mut impl Rng) -> Self {
        match rand_choice_weighted(&[0u8, 1, 2, 3], &setting.w_modal, rng) {
            0 => Self::Bx,
            1 => Self::Dm,
            2 => Self::Ge(rng.random_range(GRADE)),
            3 => Self::Le(rng.random_range(GRADE)),
            _ => unreachable!("Only values in 0..=3 should occur."),
        }
    }
}

impl Atom {
    fn new(setting: &Setting, rng: &mut impl Rng) -> Self {
        let i = rand_choice_weighted(&[0, 1, 2], &W_BOOL, rng);
        match i {
            0 => Atom::B(false),
            1 => Atom::B(true),
            2 => Atom::P(
                Self::rand_sign(setting, rng),
                rng.random_range(0..setting.num_props),
            ),
            _ => unreachable!("Only values in 0..=2 should occur."),
        }
    }

    fn rand_sign(setting: &Setting, rng: &mut impl Rng) -> bool {
        !rng.random_ratio(setting.numer_neg as u32, DENOM_NEG as u32)
    }

    fn set_atom(&mut self, atoms: &mut impl Iterator<Item = Atom>) {
        if let Some(atom) = atoms.next() {
            *self = atom;
        }
    }
}

impl fmt::Display for Phi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Disj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut conjs = self.conjs.iter();
        write!(f, "{}", conjs.next().expect("Disj's conjs cannot be empty"))?;
        for conj in conjs {
            write!(f, " {} {}", self.conn, conj)?;
        }
        Ok(())
    }
}

impl fmt::Display for DisjConn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisjConn::Or => write!(f, "∨"),
            DisjConn::Imply => write!(f, "→"),
            DisjConn::Iff => write!(f, "↔"),
        }
    }
}
impl fmt::Display for Conj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut units = self.units.iter();
        write!(f, "{}", units.next().expect("Conj's units cannot be empty"))?;
        for unit in units {
            write!(f, " ∧ {}", unit)?;
        }
        Ok(())
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unit::A(atom) => write!(f, "{atom}"),
            Unit::M(modal, atom) => match modal {
                Modal::Bx | Modal::Dm => write!(f, "{modal}{atom}"),
                Modal::Ge(_) | Modal::Le(_) => write!(f, "{modal} {atom}"),
            },
            Unit::Nest(modal, disj) => write!(f, "{modal}({disj})"),
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::P(true, i) => write!(f, "p{i}"),
            Atom::P(false, i) => write!(f, "¬p{i}"),
            Atom::B(true) => write!(f, "⊤"),
            Atom::B(false) => write!(f, "⊥"),
        }
    }
}

impl fmt::Display for Modal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modal::Bx => write!(f, "□"),
            Modal::Dm => write!(f, "◇"),
            Modal::Ge(n) => write!(f, "◇≥{n}"),
            Modal::Le(n) => write!(f, "◇≤{n}"),
        }
    }
}
