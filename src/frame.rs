use std::{fmt::Write, rc::Rc, str::FromStr, time::Instant};

use crate::{
    api::ServerResponse, b5::TransitB5, formula::Formula, k5::Transit5, k45::TransitKOr45,
    rules3::Calculus, tableau2::DisplayTableau, tb::TransitTB, tt::TransitT,
};

#[derive(Clone, Copy, Debug)]
pub(crate) enum FrameCondition {
    K,
    D,
    T,
    KB,
    DB,
    TB,
    K4,
    D4,
    S4,
    K5,
    D5,
    K45,
    D45,
    KB5,
    S5,
}

impl FromStr for FrameCondition {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim().to_uppercase();
        Ok(match s.as_str() {
            "K" | "" => FrameCondition::K,
            "D" | "KD" => FrameCondition::D,
            "T" | "KT" => FrameCondition::T,
            "KB" => FrameCondition::KB,
            "DB" | "KDB" => FrameCondition::DB,
            "TB" | "KTB" => FrameCondition::TB,
            "K4" => FrameCondition::K4,
            "D4" | "KD4" => FrameCondition::D4,
            "S4" => FrameCondition::S4,
            "K5" => FrameCondition::K5,
            "D5" | "KD5" => FrameCondition::D5,
            "K45" => FrameCondition::K45,
            "D45" | "KD45" => FrameCondition::D45,
            "KB5" | "KB4" | "KB45" => FrameCondition::KB5,
            "S5" => FrameCondition::S5,
            _ => return Err(s),
        })
    }
}

macro_rules! time_sat {
    ($clsr:expr, $satter:expr, $frame:expr, $formula:expr) => {{
        let start = Instant::now();
        let tab = $satter($frame, $formula);
        let time = format!("{:.3?}", start.elapsed());
        $clsr(tab, time)
    }};
}

macro_rules! sat_and {
    ($frame:expr, $formula:expr, $clsr:expr) => {{
        match $frame {
            FrameCondition::K | FrameCondition::D | FrameCondition::K45 | FrameCondition::D45 => {
                time_sat!($clsr, Calculus::sat::<TransitKOr45>, $frame, $formula)
            }
            FrameCondition::T => time_sat!($clsr, Calculus::sat::<TransitT>, $frame, $formula),
            FrameCondition::KB | FrameCondition::DB => todo!("B"), //$clsr($f::<Transit>($frame, $formula)),
            FrameCondition::TB => time_sat!($clsr, Calculus::sat::<TransitTB>, $frame, $formula),
            FrameCondition::K4 | FrameCondition::D4 => todo!("4"), //$clsr(GradedKCalc::sat::<Transit4>($frame, $formula)),
            FrameCondition::S4 => todo!("S4"), // $clsr(GradedKCalc::sat::<Transit4>($frame, $formula)),
            FrameCondition::K5 | FrameCondition::D5 => time_sat!($clsr, Calculus::sat::<Transit5>, $frame, $formula),
            FrameCondition::KB5 | FrameCondition::S5 => time_sat!($clsr, Calculus::sat::<TransitB5>, $frame, $formula),
        }
    }};
}

impl FrameCondition {
    pub(crate) fn print_sat(&self, formula: Rc<Formula>) {
        sat_and!(*self, formula, |tab, time| {
            println!("Solve Time: {time}");
            println!("{}", DisplayTableau(tab))
        });
    }

    pub(crate) fn graph_tab(
        &self,
        mut formula: Rc<Formula>,
        validate: bool,
        parse_time: String,
    ) -> ServerResponse {
        let mut formulae_str = String::new();
        if let Err(e) = write!(&mut formulae_str, "{}", formula.as_ref()) {
            eprintln!("Error writing formula.");
            eprintln!("{e}");
            return ServerResponse::ServerErr;
        }
        if validate {
            formula = formula.not();
        }
        sat_and!(*self, formula, |tab, solve_time| DisplayTableau(tab).model(
            formulae_str,
            solve_time,
            parse_time,
            self.symmetric()
        ))
    }

    pub(crate) const fn ray(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::TB
            | FrameCondition::K4
            | FrameCondition::S4
            | FrameCondition::K5
            | FrameCondition::K45
            | FrameCondition::KB5 => false,
            FrameCondition::D
            | FrameCondition::D4
            | FrameCondition::D5
            | FrameCondition::D45
            | FrameCondition::S5 => true,
            FrameCondition::DB => todo!(),
        }
    }

    pub(crate) const fn reflexive(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5 => false,
            FrameCondition::T | FrameCondition::TB | FrameCondition::S4 | FrameCondition::S5 => {
                true
            }
        }
    }

    pub(crate) const fn symmetric(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::S4 => false,
            FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn transitive(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::K5
            | FrameCondition::D5 => false,
            FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::S4
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn euclidean(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::S4 => false,
            FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn spotlit(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::S4
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => false,
            FrameCondition::K5 | FrameCondition::D5 => true,
        }
    }

    pub(crate) const fn luminal(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::S4
            | FrameCondition::K5
            | FrameCondition::D5 => false,
            FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn cliqued(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::K4
            | FrameCondition::D4
            | FrameCondition::S4
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45 => false,
            FrameCondition::KB5 | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn diffractive(&self) -> bool {
        match self {
            FrameCondition::K4 | FrameCondition::D4 => true,
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::T
            | FrameCondition::KB
            | FrameCondition::DB
            | FrameCondition::TB
            | FrameCondition::K5
            | FrameCondition::D5
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => false,
            FrameCondition::S4 => todo!(),
        }
    }
}
