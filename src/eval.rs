use std::{
    borrow::Cow,
    collections::HashSet,
    ffi::OsStr,
    fmt,
    fs::File,
    hash::Hash,
    io::{self, BufRead, BufReader, Write},
    mem,
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

use crate::{frame::FrameCondition, translate::ToTPTP, vecfor};

#[derive(Debug)]
pub(crate) enum EvalError {
    Io(io::Error),
    Utf8(string::FromUtf8Error),
    Re(regex::Error),
    NoMatch,
    Timeout,
    Json(serde_json::Error),
}

#[derive(Serialize, Deserialize)]
struct EvalFormula<S> {
    formula: S,
    tests: Vec<EvalOutput>,
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum EvalStatus {
    Pending,
    Failed,
    Timedout,
    Theorem,
    CounterSatisfiable,
}

#[derive(Clone, Serialize, Deserialize)]
struct EvalOutput {
    frames: FrameCondition,
    vampirestatus: EvalStatus,
    vampiretime: Option<Cow<'static, str>>,
    proverstatus: EvalStatus,
    provertime: Option<String>,
}

pub(crate) fn eval_vampire(
    formulae_file: impl AsRef<Path>,
    output_json: impl AsRef<Path> + Send + 'static,
    maxtime: &'static str,
) -> Result<(), EvalError> {
    let results = Arc::new(RwLock::new(EvalFormula::<Arc<str>>::load_results(
        &output_json,
    )?));
    let mut formulae: Vec<_> = vec![];
    load_formulae::<Arc<str>>(formulae_file, &mut formulae)?;
    let finished = Arc::new(AtomicBool::new(false));
    let handle = {
        let results = results.clone();
        let finished = finished.clone();
        thread::spawn(move || {
            let mut shutdown = false;
            while !shutdown {
                thread::sleep(Duration::from_secs(5));
                shutdown |= finished.load(atomic::Ordering::Relaxed);
                if let Err(e) = EvalFormula::save_results(&results.read().unwrap(), &output_json) {
                    eprintln!("{e}");
                }
            }
        })
    };
    EvalFormula::run_vampire(&results, "eval/tmp.p", maxtime)?;
    handle.join().unwrap();
    Ok(())
}

fn load_formulae<S>(path: impl AsRef<Path>, formulae: &mut Vec<S>) -> io::Result<()>
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
    fn load_results(path: impl AsRef<Path>) -> Result<Vec<Self>, EvalError>
    where
        S: for<'d> serde::Deserialize<'d>,
    {
        Ok(serde_json::from_reader(BufReader::new(File::open(path)?))?)
    }

    fn add_formulae(this: &mut Vec<Self>, formulae: impl Iterator<Item = S>)
    where
        S: Eq + Hash,
    {
        let mut set: HashSet<&str> = HashSet::with_capacity(this.len());
        for formula in this.iter() {
            set.insert(formula.formula.as_ref());
        }
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
        let new = vecfor!(
            f in formulae,
            if !set.contains(f.as_ref())
            => Self { formula: f, tests: tests.clone() });
        this.extend(new);
    }

    fn save_results(this: &[Self], path: impl AsRef<Path>) -> io::Result<()>
    where
        S: Serialize,
    {
        let mut outfile = File::create(path)?;
        serde_json::to_writer_pretty(&mut outfile, this)?;
        outfile.write_all(b"\n")
    }

    fn run_vampire<P>(
        this: &Arc<RwLock<Vec<Self>>>,
        path: P,
        maxtime: &'static str,
    ) -> io::Result<()>
    where
        P: AsRef<OsStr> + AsRef<Path>,
        S: Clone,
    {
        let mut queue = vec![];
        {
            let guard = this.read().unwrap();
            for (i, formula) in guard.iter().enumerate() {
                for (j, test) in formula.tests.iter().enumerate() {
                    if test.vampirestatus == EvalStatus::Pending {
                        queue.push((
                            i,
                            j,
                            ToTPTP {
                                formula: formula.formula.clone(),
                                frames: test.frames,
                            },
                        ));
                    }
                }
            }
        };
        for (i, j, tptp) in mem::take(&mut queue) {
            let (out, time) = match vampire(&tptp, &path, &maxtime) {
                Ok((status, time)) => (status, Some(Cow::Owned(time))),
                Err(EvalError::Timeout) => (EvalStatus::Timedout, Some(Cow::Borrowed(maxtime))),
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
