use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fmt,
    fs::File,
    hash::Hash,
    io::{self, BufRead, BufReader, Write},
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
pub(crate) struct DataPoint<F, T, K: Eq + Hash> {
    pub(crate) formula: F,
    pub(crate) setting: EvalSetting,
    pub(crate) tests: Vec<EvalOutput<T, K>>,
}

#[derive(Serialize, Deserialize)]
pub(crate) enum EvalSetting {
    RandDNF(Setting),
    RandTheorem(FrameCondition),
    Crafted,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum EvalStatus {
    Pending,
    Theorem,
    CounterSatisfiable,
    Skip,
    Timedout,
    Failed,
}

#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct EvalOutput<T, K: Eq + Hash> {
    frames: FrameCondition,
    tests: HashMap<K, EvalTest<T>>,
}

#[derive(Clone, Serialize, Deserialize)]
struct EvalTest<T> {
    status: EvalStatus,
    time: Option<T>,
}

pub(crate) trait Prover<F, T, M, Q> {
    fn run(&self, formula: &F, frames: FrameCondition) -> (EvalStatus, Option<T>);
    fn init(&mut self) -> Result<(), EvalError>;
    fn get_maxtime(&self) -> &M;
    fn get_key(&self) -> &Q;
}

pub(crate) struct Vampire<P, M, Q> {
    tmptptpfile: P,
    maxtime: M,
    key: Q,
}

impl<P, M, Q> Vampire<P, M, Q> {
    pub(crate) fn new(tmptptpfile: P, maxtime: M, key: Q) -> Self {
        Self {
            tmptptpfile,
            maxtime,
            key,
        }
    }

    fn run_one<F>(
        &self,
        formula: &F,
        frames: FrameCondition,
    ) -> Result<(EvalStatus, Option<String>), EvalError>
    where
        P: AsRef<Path>,
        F: AsRef<str>,
        M: AsRef<OsStr>,
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
            .arg(&self.tmptptpfile.as_ref())
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

impl<P, F, M, Q> Prover<F, String, M, Q> for Vampire<P, M, Q>
where
    P: AsRef<Path>,
    F: AsRef<str>,
    M: AsRef<str> + AsRef<OsStr>,
    Q: Eq + Hash,
{
    fn run(&self, formula: &F, frames: FrameCondition) -> (EvalStatus, Option<String>) {
        match self.run_one(formula, frames) {
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

    fn get_maxtime(&self) -> &M {
        &self.maxtime
    }

    fn get_key(&self) -> &Q {
        &self.key
    }
}

pub(crate) struct MyProver<M, Q> {
    maxtime: M,
    key: Q,
    duration: Option<Duration>,
    reaper: Option<ThreadReaper>,
}

impl<T, K> MyProver<T, K> {
    pub(crate) fn new(maxtime: T, key: K) -> Self {
        Self {
            maxtime,
            key,
            duration: None,
            reaper: None,
        }
    }
}

impl<F, M, Q> Prover<F, String, M, Q> for MyProver<M, Q>
where
    F: AsRef<str>,
    M: AsRef<str>,
    Q: Eq + Hash,
{
    fn run(&self, formula: &F, frames: FrameCondition) -> (EvalStatus, Option<String>) {
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

    fn get_maxtime(&self) -> &M {
        &self.maxtime
    }

    fn get_key(&self) -> &Q {
        &self.key
    }
}

pub(crate) fn eval_prover<M, Q>(
    datajson: impl AsRef<Path> + Send + 'static,
    prover: impl Prover<Arc<str>, String, M, Q>,
) -> Result<(), EvalError>
where
    M: AsRef<str> + Into<String>,
    Q: Into<String> + Clone,
{
    const REFRESH_RATE: u64 = 15;
    let dataset = Arc::new(RwLock::new(load_results::<Arc<str>, String, String>(
        &datajson,
    )?));
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
    run_prover(&dataset, prover)?;
    finished.store(true, atomic::Ordering::Relaxed);
    handle.join().unwrap();
    Ok(())
}

pub(crate) fn load_formulae<F>(path: impl AsRef<Path>, formulae: &mut Vec<F>) -> io::Result<()>
where
    F: From<String>,
{
    for line in BufReader::new(File::open(path)?).lines() {
        formulae.push(F::from(line?));
    }
    Ok(())
}

impl<F, T, K> DataPoint<F, T, K>
where
    K: Eq + Hash,
{
    pub(crate) fn new<Q>(
        formula: F,
        setting: EvalSetting,
        initkeys: impl Iterator<Item = Q> + Clone,
    ) -> Self
    where
        Q: Into<K>,
    {
        let tests = vecfor!(
            f in FrameCondition::array()
            => EvalOutput {
                frames: f,
                tests: HashMap::from_iter(
                    initkeys.clone().map(
                        |k| (k.into(), EvalTest::<T> { status: EvalStatus::Pending, time: None })
                    )
                ),
            }
        );
        Self {
            formula,
            setting,
            tests,
        }
    }
}

fn load_results<F, T, K>(path: impl AsRef<Path>) -> Result<Vec<DataPoint<F, T, K>>, EvalError>
where
    F: for<'d> serde::Deserialize<'d>,
    T: for<'d> serde::Deserialize<'d>,
    K: Eq + Hash + for<'d> serde::Deserialize<'d>,
{
    Ok(serde_json::from_reader(BufReader::new(File::open(path)?))?)
}

pub(crate) fn add_formulae<F, T, K, Q>(
    dataset: &mut Vec<DataPoint<F, T, K>>,
    formulae: impl Iterator<Item = F>,
    initkeys: impl Iterator<Item = Q> + Clone,
) where
    F: Eq + Hash + AsRef<str>,
    K: Eq + Hash,
    Q: Into<K>,
{
    let mut set: HashSet<&str> = HashSet::with_capacity(dataset.len());
    for datapoint in dataset.iter() {
        set.insert(datapoint.formula.as_ref());
    }
    let new = vecfor!(
        f in formulae,
        if !set.contains(f.as_ref())
        => DataPoint::new(f, EvalSetting::Crafted, initkeys.clone())
    );
    dataset.extend(new);
}

pub(crate) fn save_results<F, T, K>(
    dataset: &[DataPoint<F, T, K>],
    path: impl AsRef<Path>,
) -> io::Result<()>
where
    F: Serialize,
    T: Serialize,
    K: Eq + Hash + Serialize,
{
    let mut outfile = File::create(path)?;
    serde_json::to_writer_pretty(&mut outfile, dataset)?;
    outfile.write_all(b"\n")
}

fn run_prover<F, T, M, K, Q>(
    dataset: &Arc<RwLock<Vec<DataPoint<F, T, K>>>>,
    mut prover: impl Prover<F, T, M, Q>,
) -> Result<(), EvalError>
where
    F: AsRef<str> + Clone,
    T: AsRef<str>,
    M: AsRef<str>,
    Q: Clone + Into<K>,
    K: Eq + Hash,
{
    let mut queue = vec![];
    {
        let mut guard = dataset.write().unwrap();
        for (i, datapoint) in guard.iter_mut().enumerate() {
            for (j, test) in datapoint.tests.iter_mut().enumerate() {
                let testdata =
                    test.tests
                        .entry(prover.get_key().clone().into())
                        .or_insert(EvalTest {
                            status: EvalStatus::Pending,
                            time: None,
                        });
                match (testdata.status, &testdata.time, test.frames) {
                    (_, _, FrameCondition::K4 | FrameCondition::D4 | FrameCondition::S4) => {}
                    (EvalStatus::Pending | EvalStatus::Failed | EvalStatus::Timedout, None, _) => {
                        queue.push((i, j, datapoint.formula.clone(), test.frames))
                    }
                    (EvalStatus::Timedout, Some(time), _) => {
                        let time: f64 = time
                            .as_ref()
                            .trim()
                            .strip_suffix('s')
                            .ok_or(EvalError::FormatError)?
                            .trim()
                            .parse()?;
                        let maxtime = prover.get_maxtime().as_ref().parse()?;
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
        let (status, time) = prover.run(&formula, frames);
        {
            let mut guard = dataset.write().unwrap();
            if let Some(datapoint) = guard.get_mut(i) {
                if let Some(test) = datapoint.tests.get_mut(j)
                    && datapoint.formula.as_ref() == formula.as_ref()
                    && test.frames == frames
                {
                    test.tests
                        .insert(prover.get_key().clone().into(), EvalTest { status, time });
                }
            }
        }
        // if status == EvalStatus::Timedout {
        //     thread::sleep(Duration::from_secs(3));
        // }
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
        run_on_exts(&exts, folders, |p| {
            template_test_output(p, "vampire", "prover")
        })
        .unwrap();
    }

    fn template_test_output(file: &Path, vampirekey: impl AsRef<str>, proverkey: impl AsRef<str>) {
        let results = load_results::<String, String, String>(file).unwrap();
        for (i, datapoint) in results.iter().enumerate() {
            for test in &datapoint.tests {
                let statuses = (
                    test.tests.get(vampirekey.as_ref()),
                    test.tests.get(proverkey.as_ref()),
                );
                match statuses {
                    (None, _) => panic!("Vampire output missing: {i} {}", test.frames),
                    (_, None) => panic!("Prover output missing: {i} {}", test.frames),
                    (Some(status1), Some(status2)) => match (status1.status, status2.status) {
                        (EvalStatus::Theorem, EvalStatus::Theorem)
                        | (EvalStatus::Theorem, EvalStatus::CounterSatisfiable)
                        | (EvalStatus::CounterSatisfiable, EvalStatus::Theorem)
                        | (EvalStatus::CounterSatisfiable, EvalStatus::CounterSatisfiable) => {
                            assert_eq!(
                                status1.status, status2.status,
                                "Index: {i}; Frames: {}",
                                test.frames
                            )
                        }
                        _ => {}
                    },
                }
            }
        }
    }
}
