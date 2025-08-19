use std::{
    collections::HashSet,
    ffi::OsStr,
    fmt,
    fs::File,
    hash::Hash,
    io::{self, BufRead, BufReader, Write},
    mem,
    num::{self},
    path::Path,
    process::Command,
    string,
    sync::{
        Arc, RwLock,
        atomic::{self, AtomicBool},
    },
    thread,
    time::Duration,
};

use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    api::{self, ServerResponse},
    frame::FrameCondition,
    randgen::Setting,
    translate::ToTPTP,
    vecfor,
};

#[derive(Debug)]
pub(crate) enum EvalError {
    Io(io::Error),
    Utf8(string::FromUtf8Error),
    Re(regex::Error),
    FormatError,
    FmtWriteError(fmt::Error),
    ParseErr(num::ParseFloatError),
    NoMatch,
    Timeout,
    Json(serde_json::Error),
}

#[derive(Serialize, Deserialize)]
pub(crate) struct EvalFormula<S> {
    pub(crate) formula: S,
    pub(crate) setting: Option<Setting>,
    pub(crate) tests: Vec<EvalOutput>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum EvalStatus {
    Pending,
    Failed,
    Timedout,
    Theorem,
    CounterSatisfiable,
}

#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct EvalOutput {
    frames: FrameCondition,
    vampirestatus: EvalStatus,
    vampiretime: Option<String>,
    proverstatus: EvalStatus,
    provertime: Option<String>,
}

pub(crate) fn eval_provers(
    datajson: impl AsRef<Path> + Send + 'static,
    maxtime: &'static str,
) -> Result<(), EvalError> {
    let results = Arc::new(RwLock::new(EvalFormula::<Arc<str>>::load_results(
        &datajson,
    )?));
    let finished = Arc::new(AtomicBool::new(false));
    let handle = {
        let results = results.clone();
        let finished = finished.clone();
        thread::spawn(move || {
            let mut shutdown = false;
            while !shutdown {
                thread::sleep(Duration::from_secs(5));
                shutdown |= finished.load(atomic::Ordering::Relaxed);
                if let Err(e) = EvalFormula::save_results(&results.read().unwrap(), &datajson) {
                    eprintln!("{e}");
                }
            }
        })
    };
    EvalFormula::run_vampire(&results, "eval/tmp.p", maxtime)?;
    EvalFormula::run_prover(&results, maxtime)?;
    finished.store(true, atomic::Ordering::Relaxed);
    handle.join().unwrap();
    Ok(())
}

pub(crate) fn load_formulae<S>(path: impl AsRef<Path>, formulae: &mut Vec<S>) -> io::Result<()>
where
    S: AsRef<str> + From<String>,
{
    for line in BufReader::new(File::open(path)?).lines() {
        formulae.push(S::from(line?));
    }
    Ok(())
}

impl<S> EvalFormula<S>
where
    S: AsRef<str>,
{
    pub(crate) fn new(formula: S, setting: Option<Setting>) -> Self {
        let tests = vecfor!(
            f in FrameCondition::iter()
            => EvalOutput {
                frames: f,
                vampiretime: None,
                provertime: None,
                vampirestatus: EvalStatus::Pending,
                proverstatus: EvalStatus::Pending
            }
        );
        Self {
            formula,
            setting,
            tests,
        }
    }

    fn load_results(path: impl AsRef<Path>) -> Result<Vec<Self>, EvalError>
    where
        S: for<'d> serde::Deserialize<'d>,
    {
        Ok(serde_json::from_reader(BufReader::new(File::open(path)?))?)
    }

    pub(crate) fn add_formulae(this: &mut Vec<Self>, formulae: impl Iterator<Item = S>)
    where
        S: Eq + Hash,
    {
        let mut set: HashSet<&str> = HashSet::with_capacity(this.len());
        for formula in this.iter() {
            set.insert(formula.formula.as_ref());
        }
        let new = vecfor!(f in formulae, if !set.contains(f.as_ref()) => Self::new(f, None));
        this.extend(new);
    }

    pub(crate) fn save_results(this: &[Self], path: impl AsRef<Path>) -> io::Result<()>
    where
        S: Serialize,
    {
        let mut outfile = File::create(path)?;
        serde_json::to_writer_pretty(&mut outfile, this)?;
        outfile.write_all(b"\n")
    }

    fn run_prover(this: &Arc<RwLock<Vec<Self>>>, maxtime: &'static str) -> Result<(), EvalError>
    where
        S: Clone,
    {
        let mut queue = vec![];
        {
            let guard = this.read().unwrap();
            for (i, formula) in guard.iter().enumerate() {
                for (j, test) in formula.tests.iter().enumerate() {
                    match (test.proverstatus, &test.provertime) {
                        (EvalStatus::Pending | EvalStatus::Failed | EvalStatus::Timedout, None) => {
                            queue.push((i, j, formula.formula.clone(), test.frames))
                        }
                        (EvalStatus::Timedout, Some(time)) => {
                            let time: f64 = time
                                .trim()
                                .strip_suffix('s')
                                .ok_or(EvalError::FormatError)?
                                .trim()
                                .parse()?;
                            let maxtime = maxtime.parse()?;
                            if time < maxtime {
                                queue.push((i, j, formula.formula.clone(), test.frames))
                            }
                        }
                        _ => {}
                    }
                }
            }
        };
        println!("Len: {}", queue.len());
        for (i, j, formula, frames) in mem::take(&mut queue) {
            let (out, time) = match api::solve(formula.as_ref(), frames.as_str(), true) {
                ServerResponse::Ok(output) => (
                    if output.satisfiable {
                        EvalStatus::CounterSatisfiable
                    } else {
                        EvalStatus::Theorem
                    },
                    Some(output.times.server_time),
                ),
                ServerResponse::ActionErr(e)
                | ServerResponse::FrameErr(e)
                | ServerResponse::ParseErr(e) => {
                    eprintln!("{e}");
                    (EvalStatus::Failed, None)
                }
                ServerResponse::ServerErr => (EvalStatus::Failed, None),
                ServerResponse::NotImplemented(e) => {
                    eprintln!("{e}");
                    (EvalStatus::Pending, None)
                }
            };
            {
                let mut guard = this.write().unwrap();
                if let Some(evalformula) = guard.get_mut(i) {
                    if let Some(test) = evalformula.tests.get_mut(j)
                        && evalformula.formula.as_ref() == formula.as_ref()
                        && test.frames == frames
                    {
                        test.proverstatus = out;
                        test.provertime = time;
                    }
                }
            }
        }
        Ok(())
    }

    fn run_vampire<P>(
        this: &Arc<RwLock<Vec<Self>>>,
        path: P,
        maxtime: &'static str,
    ) -> Result<(), EvalError>
    where
        P: AsRef<OsStr> + AsRef<Path>,
        S: Clone,
    {
        let mut queue = vec![];
        {
            let guard = this.read().unwrap();
            for (i, formula) in guard.iter().enumerate() {
                for (j, test) in formula.tests.iter().enumerate() {
                    match (test.vampirestatus, &test.vampiretime) {
                        (EvalStatus::Pending | EvalStatus::Failed | EvalStatus::Timedout, None) => {
                            queue.push((
                                i,
                                j,
                                ToTPTP {
                                    formula: formula.formula.clone(),
                                    frames: test.frames,
                                },
                            ))
                        }
                        (EvalStatus::Timedout, Some(time)) => {
                            let time: f64 = time
                                .trim()
                                .strip_suffix('s')
                                .ok_or(EvalError::FormatError)?
                                .trim()
                                .parse()?;
                            let maxtime = maxtime.parse()?;
                            if time < maxtime {
                                queue.push((
                                    i,
                                    j,
                                    ToTPTP {
                                        formula: formula.formula.clone(),
                                        frames: test.frames,
                                    },
                                ))
                            }
                        }
                        _ => {}
                    }
                }
            }
        };
        for (i, j, tptp) in mem::take(&mut queue) {
            let (out, time) = match vampire(&tptp, &path, &maxtime) {
                Ok((status, time)) => (status, Some(time)),
                Err(EvalError::Timeout) => (EvalStatus::Timedout, Some(format!("{maxtime} s"))),
                Err(e) => {
                    eprintln!("{e}");
                    (EvalStatus::Failed, None)
                }
            };
            {
                let mut guard = this.write().unwrap();
                if let Some(formula) = guard.get_mut(i) {
                    if let Some(test) = formula.tests.get_mut(j)
                        && formula.formula.as_ref() == tptp.formula.as_ref()
                        && test.frames == tptp.frames
                    {
                        test.vampirestatus = out;
                        test.vampiretime = time;
                    }
                }
            }
        }
        Ok(())
    }
}

fn vampire<S: AsRef<str>>(
    tptp: &ToTPTP<S>,
    path: impl AsRef<OsStr> + AsRef<Path>,
    maxtime: impl AsRef<OsStr>,
) -> Result<(EvalStatus, String), EvalError> {
    {
        let mut file = File::create(&path)?;
        write!(file, "{tptp}")?;
    }
    let output = Command::new("./eval/vampire")
        .arg("--mode")
        .arg("casc")
        .arg("-t")
        .arg(maxtime)
        .arg("--cores")
        .arg("12")
        .arg(path)
        .output()?;
    let stdout = String::from_utf8(output.stdout)?;
    let re0 = Regex::new(r"SZS status (Timeout)")?;
    let re1 = Regex::new(r"SZS status (Theorem|CounterSatisfiable)")?;
    let re2 = Regex::new(r"Success in time (.*)$")?;
    let mut first_match = None;
    let mut second_match = None;
    for line in stdout.lines() {
        if let Some(_) = re0.captures(line) {
            return Err(EvalError::Timeout);
        }
        if first_match.is_none() {
            if let Some(caps) = re1.captures(line) {
                first_match = Some(match &caps[1] {
                    "Theorem" => EvalStatus::Theorem,
                    "CounterSatisfiable" => EvalStatus::CounterSatisfiable,
                    _ => unreachable!("Only `Theorem` or `CounterSatisfiable` should match."),
                })
            }
        }
        if second_match.is_none() {
            if let Some(caps) = re2.captures(line) {
                second_match = Some(caps[1].to_string());
            }
        }
        if first_match.is_some() && second_match.is_some() {
            break;
        }
    }
    first_match.zip(second_match).ok_or(EvalError::NoMatch)
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::Io(e) => write!(f, "{e}"),
            EvalError::Utf8(e) => write!(f, "{e}"),
            EvalError::Re(e) => write!(f, "{e}"),
            EvalError::NoMatch => write!(f, "Vampire unexpected output"),
            EvalError::Timeout => write!(f, "Vampire timeout"),
            EvalError::Json(e) => write!(f, "{e}"),
            EvalError::FormatError => write!(f, "Time Format Error"),
            EvalError::ParseErr(e) => write!(f, "{e}"),
            EvalError::FmtWriteError(e) => write!(f, "{e}"),
        }
    }
}

impl From<io::Error> for EvalError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<string::FromUtf8Error> for EvalError {
    fn from(value: string::FromUtf8Error) -> Self {
        Self::Utf8(value)
    }
}

impl From<regex::Error> for EvalError {
    fn from(value: regex::Error) -> Self {
        Self::Re(value)
    }
}

impl From<serde_json::Error> for EvalError {
    fn from(value: serde_json::Error) -> Self {
        Self::Json(value)
    }
}

impl From<num::ParseFloatError> for EvalError {
    fn from(value: num::ParseFloatError) -> Self {
        EvalError::ParseErr(value)
    }
}

impl From<fmt::Error> for EvalError {
    fn from(value: fmt::Error) -> Self {
        Self::FmtWriteError(value)
    }
}

mod test {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_output() {
        let results = EvalFormula::<Rc<str>>::load_results("eval/output.json").unwrap();
        for formula in results {
            for test in formula.tests {
                match (test.proverstatus, test.vampirestatus) {
                    (EvalStatus::Theorem, EvalStatus::Theorem)
                    | (EvalStatus::Theorem, EvalStatus::CounterSatisfiable)
                    | (EvalStatus::CounterSatisfiable, EvalStatus::Theorem)
                    | (EvalStatus::CounterSatisfiable, EvalStatus::CounterSatisfiable) => {
                        assert_eq!(
                            test.proverstatus, test.vampirestatus,
                            "Formula: {}; Frames: {}",
                            formula.formula, test.frames
                        )
                    }
                    _ => {}
                }
            }
        }
    }
}
