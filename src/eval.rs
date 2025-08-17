use std::{
    borrow::Cow,
    ffi::OsStr,
    fmt,
    fs::File,
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
use serde::Serialize;

use crate::{frame::FrameCondition, translate::ToTPTP};

#[derive(Debug)]
pub(crate) enum EvalError {
    Io(io::Error),
    Utf8(string::FromUtf8Error),
    Re(regex::Error),
    NoMatch,
    Timeout,
    Json(serde_json::Error),
}

#[derive(Serialize)]
struct EvalFormula {
    formula: Arc<str>,
    tests: Vec<EvalOutput>,
}

#[derive(Serialize)]
struct EvalOutput {
    frames: FrameCondition,
    vampireoutput: Option<Cow<'static, str>>,
    vampiretime: Option<Cow<'static, str>>,
    proveroutput: Option<String>,
    provertime: Option<String>,
}

pub(crate) fn eval_vampire(
    formulae_file: impl AsRef<Path>,
    output_json: impl AsRef<Path> + Clone + Send + 'static,
) -> Result<(), EvalError> {
    let (results, mut queue) = {
        let mut results = vec![];
        let mut queue = vec![];
        for line in BufReader::new(File::open(formulae_file)?).lines() {
            let mut tests = vec![];
            for frames in FrameCondition::iter() {
                queue.push((results.len(), tests.len()));
                tests.push(EvalOutput {
                    frames,
                    vampireoutput: None,
                    vampiretime: None,
                    proveroutput: None,
                    provertime: None,
                });
            }
            results.push(EvalFormula {
                formula: Arc::from(line?),
                tests,
            });
        }
        (Arc::new(RwLock::new(results)), queue)
    };
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
                    EvalFormula::save_to_json(&results.read().unwrap(), output_json.clone())
                {
                    eprintln!("{e}");
                }
            }
        })
    };
    let filename = "eval/tmp.p";
    const MAX_TIMES: [&'static str; 2] = ["1", "10"];
    for maxtime in MAX_TIMES {
        for (i, j) in mem::take(&mut queue) {
            let tptp = {
                let guard = results.read().unwrap();
                ToTPTP {
                    formula: guard[i].formula.clone(),
                    frames: guard[i].tests[j].frames,
                }
            };
            let (vampireoutput, vampiretime) = match vampire(&tptp, filename, maxtime) {
                Ok((status, time)) => (status, Some(time)),
                Err(EvalError::Timeout) => {
                    queue.push((i, j));
                    continue;
                }
                Err(e) => (e.to_string(), None),
            };
            {
                let mut guard = results.write().unwrap();
                guard[i].tests[j].vampireoutput = Some(Cow::Owned(vampireoutput));
                guard[i].tests[j].vampiretime = vampiretime.map(Cow::Owned);
            }
        }
    }
    for (i, j) in mem::take(&mut queue) {
        let mut guard = results.write().unwrap();
        guard[i].tests[j].vampireoutput = Some(Cow::Borrowed("Timedout"));
        guard[i].tests[j].vampiretime = Some(Cow::Borrowed(MAX_TIMES[MAX_TIMES.len() - 1]));
    }
    finished.store(true, atomic::Ordering::Relaxed);
    handle.join().unwrap();
    Ok(())
}

impl EvalFormula {
    fn save_to_json(this: &[Self], path: impl AsRef<Path>) -> std::io::Result<()> {
        let mut outfile = File::create(path)?;
        serde_json::to_writer_pretty(&mut outfile, this)?;
        outfile.write_all(b"\n")
    }
}

fn vampire<S: AsRef<str>>(
    tptp: &ToTPTP<S>,
    filename: impl AsRef<OsStr> + AsRef<Path> + Clone,
    maxtime: &str,
) -> Result<(String, String), EvalError> {
    {
        let mut file = File::create(filename.clone())?;
        write!(file, "{tptp}")?;
    }
    let output = Command::new("./eval/vampire")
        .arg("--mode")
        .arg("casc")
        .arg("-t")
        .arg(maxtime)
        .arg("--cores")
        .arg("12")
        .arg(filename)
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
            EvalError::NoMatch => write!(f, "Vampire output error"),
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
