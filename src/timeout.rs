use std::{
    collections::VecDeque,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
    thread::{self, JoinHandle},
    time::Duration,
};

pub(crate) struct Timedout;
pub(crate) type MayTimeout<T> = Result<T, Timedout>;

pub(crate) trait TimeoutHandler {
    fn timedout(&self) -> MayTimeout<()>;
}

pub(crate) struct NoopHandler;

impl TimeoutHandler for NoopHandler {
    fn timedout(&self) -> MayTimeout<()> {
        Ok(())
    }
}

pub(crate) struct StopHandler {
    stop: Arc<AtomicBool>,
}

impl StopHandler {
    pub(crate) fn new(dur: Duration) -> (Self, thread::JoinHandle<()>) {

        let stop = Arc::new(AtomicBool::new(false));
        let stopclone = stop.clone();
        let thread = thread::spawn(move || {
            cancellable_sleep(dur, 10, stopclone);
        });
        (StopHandler { stop }, thread)
    }
}

impl Drop for StopHandler {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
    }
}

impl TimeoutHandler for StopHandler {
    fn timedout(&self) -> MayTimeout<()> {
        if self.stop.load(Ordering::Relaxed) {
            Err(Timedout)
        } else {
            Ok(())
        }
    }
}

fn cancellable_sleep(dur: Duration, freq: u64, stop: Arc<AtomicBool>) {
    let total_ms = dur.as_millis() as u64 + 1;
    let mut sys = sysinfo::System::new_all();
    for _ in 0..total_ms / freq {
        thread::sleep(Duration::from_millis(freq));
        sys.refresh_memory();
        if let Some(proc) = sys.process(sysinfo::get_current_pid().unwrap()) {
            let mem_kb = proc.memory();
            if mem_kb > 9 * 1024 * 1024 {
                stop.store(true, Ordering::Relaxed);
            }
        }
        if stop.load(Ordering::Relaxed) {
            return;
        }
    }
    thread::sleep(Duration::from_millis(total_ms % freq));
    stop.store(true, Ordering::Relaxed);
}

pub struct ThreadReaper {
    queue: Arc<Mutex<VecDeque<JoinHandle<()>>>>,
    meta_handle: Option<JoinHandle<()>>,
    stop: Arc<AtomicBool>,
}

impl ThreadReaper {
    pub(crate) fn new(stop: Arc<AtomicBool>) -> Self {
        let queue = Arc::new(Mutex::new(VecDeque::<JoinHandle<()>>::new()));
        let queue_clone = queue.clone();
        let stop_clone = stop.clone();
        let meta_handle = thread::spawn(move || {
            let mut finished = false;
            while !finished {
                thread::sleep(Duration::from_millis(10));
                if stop_clone.load(Ordering::Relaxed) {
                    finished = true;
                }
                loop {
                    let handle = queue_clone.lock().unwrap().pop_front();
                    if let Some(handle) = handle {
                        if let Err(e) = handle.join() {
                            eprintln!("Thread panicked: {e:?}");
                        }
                    } else {
                        break;
                    }
                }
            }
        });
        ThreadReaper {
            queue,
            meta_handle: Some(meta_handle),
            stop,
        }
    }

    pub(crate) fn add_thread(&self, handle: JoinHandle<()>) {
        self.queue.lock().unwrap().push_back(handle);
    }
}

impl Drop for ThreadReaper {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        if let Some(Err(e)) = self.meta_handle.take().map(|h| h.join()) {
            eprintln!("Meta thread panicked: {e:?}");
        }
    }
}
