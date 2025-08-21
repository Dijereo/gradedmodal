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
    api::{self, ServerError},
    frame::FrameCondition,
    randgen::Setting,
    timeout::{StopHandler, ThreadReaper},
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
    FloatParseErr(num::ParseFloatError),
    TimeParseErr(num::ParseIntError),
    FormulaParseErr(String),
    NoMatch,
    Timeout,
    Json(serde_json::Error),
}

#[derive(Serialize, Deserialize)]
pub(crate) struct DataPoint<S> {
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

pub(crate) trait Prover<S> {
    fn run(&self, formula: &S, frames: FrameCondition) -> (EvalStatus, Option<String>);
    fn init(&mut self) -> Result<(), EvalError>;
    fn get_maxtime(&self) -> &str;
    fn check_status<'a>(&self, test: &'a EvalOutput) -> (EvalStatus, Option<&'a str>);
    fn set_status(&self, test: &mut EvalOutput, status: EvalStatus, time: Option<String>);
}

struct Vampire<P, T> {
    tmptptpfile: P,
    maxtime: T,
}

impl<P, T> Vampire<P, T> {
    fn new(tmptptpfile: P, maxtime: T) -> Self {
        Self {
            tmptptpfile,
            maxtime,
        }
    }

    fn run_one<S>(
        &self,
        formula: &S,
        frames: FrameCondition,
    ) -> Result<(EvalStatus, Option<String>), EvalError>
    where
        P: AsRef<Path> + AsRef<OsStr>,
        T: AsRef<OsStr>,
        S: AsRef<str>,
    {
        {
            let totptp = ToTPTP { formula, frames };
            let mut file = File::create(&self.tmptptpfile)?;
            write!(&mut file, "{}", totptp.to_st_frames()?)?;
        }
        let output = Command::new("./eval/vampire")
            .arg("--mode")
            .arg("casc")
            .arg("-t")
            .arg(&self.maxtime)
            .arg("--cores")
            .arg("12")
            .arg(&self.tmptptpfile)
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
        first_match
            .zip(second_match)
            .map(|(s, t)| (s, Some(t)))
            .ok_or(EvalError::NoMatch)
    }
}

impl<P, T, S> Prover<S> for Vampire<P, T>
where
    P: AsRef<Path> + AsRef<OsStr>,
    T: AsRef<OsStr> + AsRef<str>,
    S: AsRef<str>,
{
    fn run(&self, formula: &S, frames: FrameCondition) -> (EvalStatus, Option<String>) {
        match self.run_one(&formula, frames) {
            Ok((status, time)) => (status, time),
            Err(EvalError::Timeout) => (
                EvalStatus::Timedout,
                Some(format!("{} s", AsRef::<str>::as_ref(&self.maxtime))),
            ),
            Err(e) => {
                eprintln!("{e}");
                (EvalStatus::Failed, None)
            }
        }
    }

    fn init(&mut self) -> Result<(), EvalError> {
        Ok(())
    }

    fn check_status<'a>(&self, test: &'a EvalOutput) -> (EvalStatus, Option<&'a str>) {
        (test.vampirestatus, test.vampiretime.as_deref())
    }

    fn set_status(&self, test: &mut EvalOutput, status: EvalStatus, time: Option<String>) {
        test.vampirestatus = status;
        test.vampiretime = time;
    }

    fn get_maxtime(&self) -> &str {
        AsRef::<str>::as_ref(&self.maxtime)
    }
}

struct MyProver<T> {
    maxtime: T,
    duration: Option<Duration>,
    reaper: Option<ThreadReaper>,
}

impl<T> MyProver<T> {
    fn new(maxtime: T) -> Self {
        Self {
            maxtime,
            duration: None,
            reaper: None,
        }
    }
}

impl<T, S> Prover<S> for MyProver<T>
where
    T: AsRef<str>,
    S: AsRef<str>,
{
    fn run(&self, formula: &S, frames: FrameCondition) -> (EvalStatus, Option<String>) {
        let (stop, handle) =
            StopHandler::new(self.duration.expect("Prover not initiated by .init()"));
        self.reaper
            .as_ref()
            .expect("Prover not initiated by .init()")
            .add_thread(handle);
        match api::solve(formula.as_ref(), frames.as_str(), true, stop) {
            Ok(output) => (
                if output.satisfiable {
                    EvalStatus::CounterSatisfiable
                } else {
                    EvalStatus::Theorem
                },
                Some(output.times.server_time),
            ),
            Err(
                ServerError::ActionErr(e) | ServerError::FrameErr(e) | ServerError::ParseErr(e),
            ) => {
                eprintln!("{e}");
                (EvalStatus::Failed, None)
            }
            Err(ServerError::ServerErr) => (EvalStatus::Failed, None),
            Err(ServerError::NotImplemented(e)) => {
                eprintln!("{e}");
                (EvalStatus::Pending, None)
            }
            Err(ServerError::Timedout) => (
                EvalStatus::Timedout,
                Some(format!("{} s", self.maxtime.as_ref())),
            ),
        }
    }

    fn init(&mut self) -> Result<(), EvalError> {
        self.reaper = Some(ThreadReaper::new(Arc::new(AtomicBool::new(false))));
        self.duration = Some(Duration::from_secs(self.maxtime.as_ref().parse()?));
        Ok(())
    }

    fn check_status<'a>(&self, test: &'a EvalOutput) -> (EvalStatus, Option<&'a str>) {
        (test.proverstatus, test.provertime.as_deref())
    }

    fn set_status(&self, test: &mut EvalOutput, status: EvalStatus, time: Option<String>) {
        test.proverstatus = status;
        test.provertime = time;
    }

    fn get_maxtime(&self) -> &str {
        self.maxtime.as_ref()
    }
}

pub(crate) fn eval_provers(
    datajson: impl AsRef<Path> + Send + 'static,
    maxtime: &'static str,
    vampire: bool,
    myprover: bool,
) -> Result<(), EvalError> {
    const REFRESH_RATE: u64 = 15;
    let dataset = Arc::new(RwLock::new(load_results::<Arc<str>>(&datajson)?));
    let finished = Arc::new(AtomicBool::new(false));
    let handle = {
        let results = dataset.clone();
        let finished = finished.clone();
        thread::spawn(move || {
            let mut shutdown = false;
            while !shutdown {
                for _ in 0..REFRESH_RATE {
                    thread::sleep(Duration::from_secs(1));
                    shutdown |= finished.load(atomic::Ordering::Relaxed);
                    if shutdown {
                        break;
                    }
                }
                if let Err(e) = save_results(&results.read().unwrap(), &datajson) {
                    eprintln!("{e}");
                }
            }
        })
    };
    if vampire {
        run_prover(&dataset, Vampire::new("eval/tmp.p", maxtime))?;
    }
    if myprover {
        run_prover(&dataset, MyProver::new(maxtime))?;
    }
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

impl<S> DataPoint<S>
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
}

fn load_results<S: AsRef<str>>(path: impl AsRef<Path>) -> Result<Vec<DataPoint<S>>, EvalError>
where
    S: for<'d> serde::Deserialize<'d>,
{
    Ok(serde_json::from_reader(BufReader::new(File::open(path)?))?)
}

pub(crate) fn add_formulae<S: AsRef<str>>(
    dataset: &mut Vec<DataPoint<S>>,
    formulae: impl Iterator<Item = S>,
) where
    S: Eq + Hash,
{
    let mut set: HashSet<&str> = HashSet::with_capacity(dataset.len());
    for datapoint in dataset.iter() {
        set.insert(datapoint.formula.as_ref());
    }
    let new = vecfor!(f in formulae, if !set.contains(f.as_ref()) => DataPoint::new(f, None));
    dataset.extend(new);
}

pub(crate) fn save_results<S: AsRef<str>>(
    dataset: &[DataPoint<S>],
    path: impl AsRef<Path>,
) -> io::Result<()>
where
    S: Serialize,
{
    let mut outfile = File::create(path)?;
    serde_json::to_writer_pretty(&mut outfile, dataset)?;
    outfile.write_all(b"\n")
}

fn run_prover<S>(
    dataset: &Arc<RwLock<Vec<DataPoint<S>>>>,
    mut prover: impl Prover<S>,
) -> Result<(), EvalError>
where
    S: AsRef<str> + Clone,
{
    let mut queue = vec![];
    {
        let guard = dataset.read().unwrap();
        for (i, datapoint) in guard.iter().enumerate() {
            for (j, test) in datapoint.tests.iter().enumerate() {
                match prover.check_status(test) {
                    (EvalStatus::Pending | EvalStatus::Failed | EvalStatus::Timedout, None) => {
                        queue.push((i, j, datapoint.formula.clone(), test.frames))
                    }
                    (EvalStatus::Timedout, Some(time)) => {
                        let time: f64 = time
                            .trim()
                            .strip_suffix('s')
                            .ok_or(EvalError::FormatError)?
                            .trim()
                            .parse()?;
                        let maxtime = prover.get_maxtime().parse()?;
                        if time < maxtime {
                            queue.push((i, j, datapoint.formula.clone(), test.frames))
                        }
                    }
                    _ => {}
                }
            }
        }
    };
    prover.init()?;
    for (i, j, formula, frames) in queue {
        let (out, time) = prover.run(&formula, frames);
        {
            let mut guard = dataset.write().unwrap();
            if let Some(datapoint) = guard.get_mut(i) {
                if let Some(test) = datapoint.tests.get_mut(j)
                    && datapoint.formula.as_ref() == formula.as_ref()
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
            EvalError::FloatParseErr(e) => write!(f, "{e}"),
            EvalError::FmtWriteError(e) => write!(f, "{e}"),
            EvalError::TimeParseErr(e) => write!(f, "Input Time Format Error: {e}"),
            EvalError::FormulaParseErr(e) => write!(f, "{e}"),
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
        EvalError::FloatParseErr(value)
    }
}

impl From<num::ParseIntError> for EvalError {
    fn from(value: num::ParseIntError) -> Self {
        Self::TimeParseErr(value)
    }
}

impl From<fmt::Error> for EvalError {
    fn from(value: fmt::Error) -> Self {
        Self::FmtWriteError(value)
    }
}

mod test {
    use crate::util::{self, run_on_exts};

    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_output() {
        let folders = ["eval/results/"];
        let exts = ["json"];
        run_on_exts(&exts, folders, template_test_output).unwrap();
    }

    fn template_test_output(file: &Path) {
        let results = load_results::<Rc<str>>(file).unwrap();
        for datapoint in results {
            for test in datapoint.tests {
                match (test.proverstatus, test.vampirestatus) {
                    (EvalStatus::Theorem, EvalStatus::Theorem)
                    | (EvalStatus::Theorem, EvalStatus::CounterSatisfiable)
                    | (EvalStatus::CounterSatisfiable, EvalStatus::Theorem)
                    | (EvalStatus::CounterSatisfiable, EvalStatus::CounterSatisfiable) => {
                        assert_eq!(
                            test.proverstatus, test.vampirestatus,
                            "Formula: {}; Frames: {}",
                            datapoint.formula, test.frames
                        )
                    }
                    _ => {}
                }
            }
        }
    }
}
