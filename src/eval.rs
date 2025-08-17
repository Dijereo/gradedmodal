use std::{
    array,
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
struct EvalFormula<'a, 'b, S> {
    formula: S,
    tests: Vec<EvalOutput<'a, 'b>>,
}

#[derive(Clone, Serialize, Deserialize)]
struct EvalOutput<'a, 'b> {
    frames: FrameCondition,
    vampireoutput: Option<Cow<'a, str>>,
    vampiretime: Option<Cow<'b, str>>,
    proveroutput: Option<String>,
    provertime: Option<String>,
}

pub(crate) fn eval_vampire<S>(
    formulae_file: impl AsRef<Path>,
    output_json: impl AsRef<Path> + Send + 'static,
) -> Result<(), EvalError> {
    let results = Arc::new(RwLock::new(EvalFormula::<Arc<str>>::load_results(
        &output_json,
    )?));
    let mut formulae: Vec<_> = vec![];
    EvalFormula::<Arc<str>>::load_formulae(formulae_file, &mut formulae)?;
    let finished = Arc::new(AtomicBool::new(false));
    let handle = {
        let results = results.clone();
        let finished = finished.clone();
        thread::spawn(move || {
            let mut shutdown = false;
            while !shutdown {
                thread::sleep(Duration::from_secs(5));
                shutdown |= finished.load(atomic::Ordering::Relaxed);
                if let Err(e) =
                    EvalFormula::save_results(&results.read().unwrap(), &output_json)
                {
                    eprintln!("{e}");
                }
            }
        })
    };
    let filename = "eval/tmp.p";
    for maxtime in ["1", "10"] {
        EvalFormula::run_vampire(&results, filename, maxtime);
    }
    handle.join().unwrap();
    Ok(())
}

impl<'a, 'b, S: AsRef<str>> EvalFormula<'a, 'b, S> {
    fn load_formulae(path: impl AsRef<Path>, formulae: &mut Vec<S>) -> io::Result<()>
    where
        S: From<String>,
    {
        for line in BufReader::new(File::open(path)?).lines() {
            formulae.push(S::from(line?));
        }
        Ok(())
    }

    fn load_results(path: impl AsRef<Path>) -> Result<Vec<Self>, EvalError>
    where
        S: for<'c> serde::Deserialize<'c>,
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
        let tests = vecfor!(f in FrameCondition::iter() => {EvalOutput {
            frames: f,
            vampireoutput: None,
            vampiretime: None,
            proveroutput: None,
            provertime: None,
        }});
        let new = vecfor!(
            f in formulae,
            if {!set.contains(f.as_ref())}
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

    fn run_vampire<P>(this: &Arc<RwLock<Vec<Self>>>, path: P, maxtime: &'b str) -> io::Result<()>
    where
        P: AsRef<OsStr> + AsRef<Path>,
        S: Clone,
    {
        let mut queue =vec![];
        {
            let guard = this.read().unwrap();
            for (i, formula) in guard.iter().enumerate() {
                for (j, test) in formula.tests.iter().enumerate() {
                    if test.vampireoutput.is_none() {
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
            let (out, time) = match vampire(&tptp, &path, maxtime) {
                Ok((status, time)) => (Some(Cow::Owned(status)), Some(Cow::Owned(time))),
                Err(EvalError::Timeout) => (
                    Some(Cow::Borrowed("Timedout")),
                    Some(Cow::Borrowed(maxtime)),
                ),
                Err(e) => (Some(Cow::Owned(e.to_string())), None),
            };
            {
                let mut guard = this.write().unwrap();
                if let Some(formula) = guard.get_mut(i) {
                    if let Some(test) = formula.tests.get_mut(j)
                        && formula.formula.as_ref() == tptp.formula.as_ref()
                        && test.frames == tptp.frames
                    {
                        test.vampireoutput = out;
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
    maxtime: &str,
) -> Result<(String, String), EvalError> {
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
    let mut timeout = false;
    for line in stdout.lines() {
        if let Some(_) = re0.captures(line) {
            timeout = true;
            break;
        }
        if first_match.is_none() {
            if let Some(caps) = re1.captures(line) {
                first_match = Some(caps[1].to_string());
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
    if timeout {
        Err(EvalError::Timeout)
    } else {
        first_match.zip(second_match).ok_or(EvalError::NoMatch)
    }
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
