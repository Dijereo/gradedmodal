use std::{
    fmt::{self, Debug, Display},
    rc::Rc,
};

use crate::{
    parser::{
        DynParser, parse_any, parse_entire, parse_eq, parse_fst, parse_option, parse_snd,
        parse_tup, parse_unit,
    },
    token::Token,
};

#[derive(Debug, PartialEq)]
pub enum Formula {
    PropVar(Option<u8>),
    Not(Rc<Formula>),
    And(Rc<Formula>, Rc<Formula>),
    Or(Rc<Formula>, Rc<Formula>),
    Imply(Rc<Formula>, Rc<Formula>),
    Iff(Rc<Formula>, Rc<Formula>),
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Formula::PropVar(Some(n)) => write!(f, "p{n}"),
            Formula::PropVar(None) => write!(f, "p"),
            Formula::Not(subformula) => match subformula.as_ref() {
                Formula::PropVar(_) | Formula::Not(_) => write!(f, "~{subformula}"),
                _ => write!(f, "~({subformula})"),
            },
            Formula::And(leftsubf, rightsubf) => match leftsubf.as_ref() {
                Formula::PropVar(_) | Formula::Not(_) | Formula::And(_, _) => {
                    write!(f, "{leftsubf} & ")
                }
                _ => write!(f, "({leftsubf}) & "),
            }
            .and(match rightsubf.as_ref() {
                Formula::PropVar(_) | Formula::Not(_) | Formula::And(_, _) => {
                    write!(f, "{rightsubf}")
                }
                _ => write!(f, "({rightsubf})"),
            }),
            Formula::Or(leftsubf, rightsubf) => {
                let out = match leftsubf.as_ref() {
                    Formula::PropVar(_)
                    | Formula::Not(_)
                    | Formula::And(_, _)
                    | Formula::Or(_, _) => {
                        write!(f, "{leftsubf} | ")
                    }
                    _ => write!(f, "({leftsubf}) | "),
                }
                .and(match rightsubf.as_ref() {
                    Formula::PropVar(_)
                    | Formula::Not(_)
                    | Formula::And(_, _)
                    | Formula::Or(_, _) => {
                        write!(f, "{rightsubf}")
                    }
                    _ => write!(f, "({rightsubf})"),
                });
                out
            }
            Formula::Imply(leftsubf, rightsubf) => match leftsubf.as_ref() {
                Formula::Imply(_, _) => write!(f, "({leftsubf}) -> {rightsubf}"),
                _ => write!(f, "{leftsubf} -> {rightsubf}"),
            },
            Formula::Iff(leftsubf, rightsubf) => {
                let out = match leftsubf.as_ref() {
                    Formula::Imply(_, _) => write!(f, "({leftsubf}) <-> "),
                    _ => write!(f, "{leftsubf} <-> "),
                }
                .and(match rightsubf.as_ref() {
                    Formula::Imply(_, _) => write!(f, "({rightsubf})"),
                    _ => write!(f, "{rightsubf}"),
                });
                out
            }
        }
    }
}

pub(crate) fn full_parser<S>(stream: S) -> Result<Formula, Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_entire(stream, formula_parser)
}

fn var_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)>,
{
    let process = |token| match token {
        Token::PROPVAR(n) => Ok(Formula::PropVar(n)),
        _ => Err(token),
    };
    parse_unit(stream, process)
}

fn paren_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_snd(
        stream,
        |s| parse_eq(s, &Token::LPAREN, ()),
        |s| {
            parse_fst(
                s,
                formula_parser,
                |s2| parse_eq(s2, &Token::RPAREN, ()),
            )
        },
    )
}

fn atom_parser<'a, S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone + 'a,
{
    parse_any(
        stream,
        ([
            Box::new(paren_parser),
            Box::new(not_parser),
            Box::new(var_parser),
        ] as [Box<DynParser<'a, Token, Formula, S>>; 3])
            .into_iter(),
    )
}

fn not_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        |s| parse_eq(s, &Token::NOT, ()),
        atom_parser,
        |_, formula| Formula::Not(Rc::new(formula)),
    )
}

fn imply_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_snd(stream, |s| parse_eq(s, &Token::IMPLY, ()), formula_parser)
}

fn iff_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_snd(stream, |s| parse_eq(s, &Token::IFF, ()), nimply_parser)
}

fn or_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_snd(stream, |s| parse_eq(s, &Token::OR, ()), ncond_parser)
}

fn and_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_snd(stream, |s| parse_eq(s, &Token::AND, ()), conj_parser)
}

fn conj_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        atom_parser,
        |s| parse_option(s, and_parser),
        |f1, f2| match f2 {
            Some(f2) => Formula::And(Rc::new(f1), Rc::new(f2)),
            None => f1,
        },
    )
}

fn ncond_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        conj_parser,
        |s| parse_option(s, or_parser),
        |f1, f2| match f2 {
            Some(f2) => Formula::Or(Rc::new(f1), Rc::new(f2)),
            None => f1,
        },
    )
}

fn nimply_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        ncond_parser,
        |s| parse_option(s, iff_parser),
        |f1, f2| match f2 {
            Some(f2) => Formula::Iff(Rc::new(f1), Rc::new(f2)),
            None => f1,
        },
    )
}

pub(crate) fn formula_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        nimply_parser,
        |s| parse_option(s, imply_parser),
        |f1, f2| match f2 {
            Some(f2) => Formula::Imply(Rc::new(f1), Rc::new(f2)),
            None => f1,
        },
    )
}

// src/lib.rs

#[cfg(test)]
mod tests {
    use crate::token::tokenize;

    use super::*;

    fn prop(n: Option<u8>) -> Formula {
        Formula::PropVar(n)
    }

    fn not(f: Formula) -> Formula {
        Formula::Not(Rc::new(f))
    }

    fn and(l: Formula, r: Formula) -> Formula {
        Formula::And(Rc::new(l), Rc::new(r))
    }

    fn or(l: Formula, r: Formula) -> Formula {
        Formula::Or(Rc::new(l), Rc::new(r))
    }

    fn imply(l: Formula, r: Formula) -> Formula {
        Formula::Imply(Rc::new(l), Rc::new(r))
    }

    fn iff(l: Formula, r: Formula) -> Formula {
        Formula::Iff(Rc::new(l), Rc::new(r))
    }

    fn parse_str(input: &str) -> Formula {
        let tokens = tokenize(input).unwrap();
        full_parser(tokens.into_iter().enumerate()).unwrap()
    }

    #[test]
    fn test_parse_propvar() {
        assert_eq!(parse_str("p"), prop(None));
        assert_eq!(parse_str("p42"), prop(Some(42)));
    }

    #[test]
    fn test_parse_not() {
        assert_eq!(parse_str("~p1"), not(prop(Some(1))));
        assert_eq!(parse_str("~~p2"), not(not(prop(Some(2)))));
    }

    #[test]
    fn test_parse_and_or() {
        assert_eq!(parse_str("p1 & p2"), and(prop(Some(1)), prop(Some(2))));
        assert_eq!(parse_str("p1 | p"), or(prop(Some(1)), prop(None)));
        assert_eq!(
            parse_str("p1 & p2 | p"),
            or(and(prop(Some(1)), prop(Some(2))), prop(None))
        );
        assert_eq!(
            parse_str("p1 | p2 & p3"),
            or(prop(Some(1)), and(prop(Some(2)), prop(Some(3))))
        );
    }

    #[test]
    fn test_parse_imply_iff() {
        assert_eq!(parse_str("p1 -> p2"), imply(prop(Some(1)), prop(Some(2))));
        assert_eq!(parse_str("p1 <-> p2"), iff(prop(Some(1)), prop(Some(2))));
        assert_eq!(
            parse_str("p1 -> p2 -> p3"),
            imply(prop(Some(1)), imply(prop(Some(2)), prop(Some(3))))
        );
        assert_eq!(
            parse_str("p <-> p2 <-> p3"),
            iff(prop(None), iff(prop(Some(2)), prop(Some(3))))
        );
    }

    #[test]
    fn test_parse_grouping() {
        assert_eq!(
            parse_str("~(p1 & p2)"),
            not(and(prop(Some(1)), prop(Some(2))))
        );
        assert_eq!(
            parse_str("(p1 -> p2) & p3"),
            and(imply(prop(Some(1)), prop(Some(2))), prop(Some(3)))
        );
        assert_eq!(
            parse_str("p1 -> (p2 & p3)"),
            imply(prop(Some(1)), and(prop(Some(2)), prop(Some(3))))
        );
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let tokens = vec![Token::AND, Token::PROPVAR(Some(1))]
            .into_iter()
            .enumerate();
        let err = full_parser(tokens).unwrap_err();
        assert_eq!(err, Some((0, Token::AND)));
    }

    // #[test]
    fn test_parse_error_unclosed_paren() {
        let tokens = tokenize("((((p1 & p2))").unwrap().into_iter().enumerate();
        let err = full_parser(tokens).unwrap_err();
        assert_eq!(err, Some((2, Token::LPAREN)));
    }

    #[test]
    fn test_parse_error_extra_tokens() {
        let tokens = tokenize("p1 p").unwrap();
        assert_eq!(tokens, vec![Token::PROPVAR(Some(1)), Token::PROPVAR(None)]);
        let tokens = tokens.into_iter().enumerate();
        let err = full_parser(tokens).unwrap_err();
        assert_eq!(err, Some((1, Token::PROPVAR(None))));
    }
}
