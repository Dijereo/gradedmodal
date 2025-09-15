use std::{fmt, fs, io, ops::Deref, path::Path, rc::Rc};

pub(crate) enum Few<T> {
    None,
    One(T),
    Two(T, T),
    Three(T, T, T),
}

#[derive(Clone, Debug)]
pub(crate) enum Rx<T> {
    Box(Box<T>),
    Rc(Rc<T>),
}

impl<T> AsRef<T> for Rx<T> {
    fn as_ref(&self) -> &T {
        match self {
            Rx::Box(b) => b.as_ref(),
            Rx::Rc(r) => r.as_ref(),
        }
    }
}

impl<T> Deref for Rx<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Rx::Box(b) => b.deref(),
            Rx::Rc(r) => r.deref(),
        }
    }
}

impl<T> From<Box<T>> for Rx<T> {
    fn from(b: Box<T>) -> Self {
        Rx::Box(b)
    }
}

impl<T> From<Rc<T>> for Rx<T> {
    fn from(r: Rc<T>) -> Self {
        Rx::Rc(r)
    }
}

impl<T> Rx<T> {
    pub(crate) fn rc(item: T) -> Self {
        Rc::new(item).into()
    }

    pub(crate) fn bxx(item: T) -> Self {
        Box::new(item).into()
    }

    pub(crate) fn try_deref_mut(&mut self) -> Option<&mut T> {
        match self {
            Rx::Box(b) => Some(b.as_mut()),
            Rx::Rc(_) => None,
        }
    }

    pub(crate) fn try_into_inner(self) -> Result<T, Rc<T>> {
        match self {
            Rx::Box(b) => Ok(*b),
            Rx::Rc(r) => Err(r),
        }
    }

    pub(crate) fn try_as_box(self) -> Result<Box<T>, Rc<T>> {
        match self {
            Rx::Box(b) => Ok(b),
            Rx::Rc(r) => Err(r),
        }
    }

    pub(crate) fn try_quick_clone(&self) -> Option<Self> {
        match self {
            Rx::Box(_) => None,
            Rx::Rc(r) => Some(r.clone().into()),
        }
    }

    pub(crate) fn into_rc(self) -> Self {
        match self {
            Rx::Box(b) => Self::Rc(Rc::new(*b)),
            Rx::Rc(_) => self,
        }
    }
}

pub(crate) fn write_subscript(f: &mut fmt::Formatter, mut n: u8) -> fmt::Result {
    const DIGITS: [char; 10] = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'];
    let mut out = String::new();
    loop {
        write!(f, "{}", DIGITS[(n % 10) as usize])?;
        n /= 10;
        if n == 0 {
            break write!(f, "");
        }
    }
}

pub(crate) struct EnqueueProdIter<O, I, T, U> {
    outer: O,
    inner: I,
    queue: Vec<(T, U)>,
}

impl<O, I, T, U> EnqueueProdIter<O, I, T, U>
where
    O: Iterator<Item = T>,
    I: Iterator<Item = U>,
{
    fn new(outer: O, inner: I) -> Self {
        Self {
            outer,
            inner,
            queue: vec![],
        }
    }

    pub(crate) fn enqueue(&mut self, outeritem: T, inneritem: U) {
        self.queue.push((outeritem, inneritem));
    }
}

impl<O, I, T, U> Iterator for EnqueueProdIter<O, I, T, U> {
    type Item = (T, U);

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub fn run_on_exts<E, D>(
    exts: &[E],
    folders: impl Iterator<Item = D>,
    mut f: impl FnMut(&Path),
) -> io::Result<()>
where
    E: AsRef<str>,
    D: AsRef<Path>,
{
    for folder in folders {
        if !folder.as_ref().exists() {
            continue;
        }
        for entry in fs::read_dir(folder)? {
            let path = entry?.path();
            if path.is_file() {
                if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                    if exts
                        .iter()
                        .any(|wanted| wanted.as_ref().eq_ignore_ascii_case(ext))
                    {
                        f(&path);
                    }
                }
            }
        }
    }
    Ok(())
}

#[macro_export]
macro_rules! vecfor {
    [
        $var:ident in $iter:expr
        $(, cap=$cap:expr)?
        , flat
        $(, into $store:expr)?
        $(, if $pred:expr)?
        $(=> $body:expr)?
    ] => {{
        vecfor!(@inner; $var in $iter $(, cap=$cap)?, flat=true $(, into $store)? $(, if $pred)? $(=> $body)? )
    }};
    [
        $var:ident in $iter:expr
        $(, cap=$cap:expr)?
        $(, into $store:expr)?
        $(, if $pred:expr)?
        $(=> $body:expr)?
    ] => {{
        vecfor!(@inner; $var in $iter $(, cap=$cap)? $(, into $store)? $(, if $pred)? $(=> $body)? )
    }};
    [
        @inner;
        $var:ident in $iter:expr
        $(, cap=$cap:expr)?
        $(, flat=$flat:literal)?
        $(, into $store:expr)?
        $(, if $pred:expr)?
        $(=> $body:expr)?
    ] => {{
        let mut v = Vec::new();
        let _p = &mut v;
        $(
            let _p = &mut $store;
            v.push(());
            let v = ();
        )?
        $(
            _p.reserve($cap);
        )?
        for $var in $iter {
            $(
                if !$pred {
                    continue;
                }
            )?
            $(
                let $var = $body;
            )?
            let iter = [$var].into_iter();
            $(
                $flat;
                let iter = iter.flatten();
            )?
            _p.extend(iter);
        }
        v
    }};
}
