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
struct EvalReport {
    tests: Vec<EvalOutput>,
}

#[derive(Serialize)]
struct EvalOutput {
    formula: String,
    frames: FrameCondition,
    vampireoutput: String,
    vampiretime: Option<String>,
    proveroutput: Option<String>,
    provertime: Option<String>,
}

pub(crate) fn eval_vampire(
    formulae_file: impl AsRef<Path>,
    output_json: impl AsRef<Path> + Clone + Send + 'static,
) -> Result<(), EvalError> {
    let infile = File::open(formulae_file)?;
    let reader = BufReader::new(infile);
    let filename = "eval/tmp.p";
    let mut timedouts = vec![];
    let report = Arc::new(RwLock::new(EvalReport { tests: vec![] }));
    let finished = Arc::new(AtomicBool::new(false));
    let handle = {
        let report = report.clone();
        let finished = finished.clone();
        thread::spawn(move || {
            let mut shutdown = false;
            while !shutdown {
                thread::sleep(Duration::from_secs(5));
                shutdown |= finished.load(atomic::Ordering::Relaxed);
                if let Err(e) = report.read().unwrap().save_to_json(output_json.clone()) {
                    eprintln!("{e}");
                }
            }
        })
    };

    let maxtime = "1";
    for line in reader.lines() {
        let line = line?;
        for frames in FrameCondition::iter() {
            let tptp = ToTPTP {
                formula: Cow::Borrowed(&line),
                frames,
            };
            match vampire(&tptp, filename, maxtime) {
                Ok((status, time)) => report.write().unwrap().tests.push(EvalOutput {
                    formula: tptp.formula.to_string(),
                    frames: tptp.frames,
                    vampireoutput: status,
                    vampiretime: Some(time),
                    proveroutput: None,
                    provertime: None,
                }),
                Err(EvalError::Timeout) => timedouts.push(tptp.unbind_lifetime()),
                Err(e) => report.write().unwrap().tests.push(EvalOutput {
                    formula: tptp.formula.to_string(),
                    frames: tptp.frames,
                    vampireoutput: e.to_string(),
                    vampiretime: None,
                    proveroutput: None,
                    provertime: None,
                }),
            }
        }
    }
    for maxtime in ["10"] {
        for timedout in mem::take(&mut timedouts) {
            let tptp = ToTPTP {
                formula: timedout.formula,
                frames: timedout.frames,
            };
            match vampire(&tptp, filename, maxtime) {
                Ok((status, time)) => report.write().unwrap().tests.push(EvalOutput {
                    formula: tptp.formula.to_string(),
                    frames: tptp.frames,
                    vampireoutput: status,
                    vampiretime: Some(time),
                    proveroutput: None,
                    provertime: None,
                }),
                Err(EvalError::Timeout) => timedouts.push(tptp),
                Err(e) => report.write().unwrap().tests.push(EvalOutput {
                    formula: tptp.formula.to_string(),
                    frames: tptp.frames,
                    vampireoutput: e.to_string(),
                    vampiretime: None,
                    proveroutput: None,
                    provertime: None,
                }),
            }
        }
    }
    finished.store(true, atomic::Ordering::Relaxed);
    handle.join().unwrap();
    Ok(())
}

impl EvalReport {
    fn save_to_json(&self, path: impl AsRef<Path>) -> std::io::Result<()> {
        let mut outfile = File::create(path)?;
        serde_json::to_writer_pretty(&mut outfile, self)?;
        outfile.write_all(b"\n")
    }
}

fn vampire(
    tptp: &ToTPTP,
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
