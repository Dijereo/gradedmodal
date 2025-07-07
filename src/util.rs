pub(crate) enum Few<T> {
    None,
    One(T),
    Two(T, T),
    Three(T, T, T),
}
