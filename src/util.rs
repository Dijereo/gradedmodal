use std::fmt;

pub(crate) enum Few<T> {
    None,
    One(T),
    Two(T, T),
    Three(T, T, T),
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
