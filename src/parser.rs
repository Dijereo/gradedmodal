pub(crate) type DynParser<'a, I, O, S> = dyn FnOnce(S) -> Result<(O, S), Option<(usize, I)>> + 'a;

pub(crate) fn parse_unit<I, O, S>(
    mut stream: S,
    process: impl FnOnce(I) -> Result<O, I>,
) -> Result<(O, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)>,
{
    match stream.next() {
        Some((i, t)) => match process(t) {
            Ok(out) => Ok((out, stream)),
            Err(t) => Err(Some((i, t))),
        },
        None => Err(None),
    }
}

pub(crate) fn parse_eq<I, O, S>(
    stream: S,
    target: &I,
    output: O,
) -> Result<(O, S), Option<(usize, I)>>
where
    I: PartialEq,
    S: Iterator<Item = (usize, I)>,
{
    parse_unit(
        stream,
        move |t| if &t == target { Ok(output) } else { Err(t) },
    )
}

pub(crate) fn parse_tup<I, O1, O2, O, S>(
    stream: S,
    p1: impl FnOnce(S) -> Result<(O1, S), Option<(usize, I)>>,
    p2: impl FnOnce(S) -> Result<(O2, S), Option<(usize, I)>>,
    process: impl FnOnce(O1, O2) -> O,
) -> Result<(O, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)>,
{
    let (out1, stream) = p1(stream)?;
    let (out2, stream) = p2(stream)?;
    Ok((process(out1, out2), stream))
}

pub(crate) fn parse_fst<I, O, O2, S>(
    stream: S,
    p1: impl FnOnce(S) -> Result<(O, S), Option<(usize, I)>>,
    p2: impl FnOnce(S) -> Result<(O2, S), Option<(usize, I)>>,
) -> Result<(O, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)>,
{
    parse_tup(stream, p1, p2, |out1, _| out1)
}

pub(crate) fn parse_snd<I, O1, O, S>(
    stream: S,
    p1: impl FnOnce(S) -> Result<(O1, S), Option<(usize, I)>>,
    p2: impl FnOnce(S) -> Result<(O, S), Option<(usize, I)>>,
) -> Result<(O, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)>,
{
    parse_tup(stream, p1, p2, |_, out2| out2)
}

pub(crate) fn parse_any<'a, I, O, S>(
    stream: S,
    parsers: impl Iterator<Item = Box<DynParser<'a, I, O, S>>>,
) -> Result<(O, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)> + Clone,
{
    let mut best_attempt = None;
    for parser in parsers {
        let stream = stream.clone();
        match parser(stream) {
            out @ Ok(_) => return out,
            Err(None) => {}
            Err(attempt @ Some((i, _))) => {
                match best_attempt {
                    Some((j, _)) if i > j => best_attempt = attempt,
                    None => best_attempt = attempt,
                    _ => {}
                };
            }
        }
    }
    Err(best_attempt)
}

pub(crate) fn parse_option<I, O, S>(
    stream: S,
    parser: impl FnOnce(S) -> Result<(O, S), Option<(usize, I)>>,
) -> Result<(Option<O>, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)> + Clone,
{
    match parser(stream.clone()) {
        Ok((out, updated_stream)) => Ok((Some(out), updated_stream)),
        Err(_) => Ok((None, stream)),
    }
}

pub(crate) fn parse_repeat<I, O, S>(
    mut stream: S,
    mut parser: impl FnMut(S) -> Result<(O, S), Option<(usize, I)>>,
) -> Result<(Vec<O>, S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)> + Clone,
{
    let mut outs = vec![];
    loop {
        match parser(stream.clone()) {
            Ok((out, updated_stream)) => {
                stream = updated_stream;
                outs.push(out);
            }
            Err(_) => {
                return Ok((outs, stream));
            }
        }
    }
}

pub(crate) fn parse_end<I, S>(mut stream: S) -> Result<((), S), Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)>,
{
    match stream.next() {
        val @ Some(_) => Err(val),
        None => Ok(((), stream)),
    }
}

pub(crate) fn parse_entire<I, O, S>(
    stream: S,
    parser: impl FnOnce(S) -> Result<(O, S), Option<(usize, I)>>,
) -> Result<O, Option<(usize, I)>>
where
    S: Iterator<Item = (usize, I)>,
{
    let (out, stream) = parser(stream)?;
    let _ = parse_end(stream)?;
    Ok(out)
}
