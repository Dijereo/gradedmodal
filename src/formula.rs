use std::{fmt, rc::Rc};

use crate::{
    parser::{
        DynParser, parse_any, parse_entire, parse_eq, parse_fst, parse_option, parse_snd,
        parse_tup, parse_unit,
    },
    token::Token,
};

#[derive(Debug, PartialEq)]
pub(crate) enum Formula {
    Bottom,
    Top,
    PropVar(char, Option<u32>),
    Not(Rc<Formula>),
    Box(Rc<Formula>),
    Diamond(Rc<Formula>),
    DiamondGe(u32, Rc<Formula>), // Invariant: .0: u32 cannot be zero
    DiamondLe(u32, Rc<Formula>),
    And(Rc<Formula>, Rc<Formula>),
    Or(Rc<Formula>, Rc<Formula>),
    Imply(Rc<Formula>, Rc<Formula>),
    Iff(Rc<Formula>, Rc<Formula>),
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Formula::Bottom => write!(f, "⊥"),
            Formula::Top => write!(f, "⊤"),
            Formula::PropVar(c, Some(n)) => write!(f, "{c}{n}"),
            Formula::PropVar(c, None) => write!(f, "{c}"),
            Formula::Not(subformula) => match subformula.as_ref() {
                Formula::Bottom
                | Formula::Top
                | Formula::PropVar(..)
                | Formula::Not(_)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(..)
                | Formula::DiamondLe(..) => {
                    write!(f, "¬{subformula}")
                }
                _ => write!(f, "¬({subformula})"),
            },
            Formula::Box(subformula) => match subformula.as_ref() {
                Formula::Bottom
                | Formula::Top
                | Formula::PropVar(..)
                | Formula::Not(_)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(..)
                | Formula::DiamondLe(..) => {
                    write!(f, "□{subformula}")
                }
                _ => write!(f, "□({subformula})"),
            },
            Formula::Diamond(subformula) => match subformula.as_ref() {
                Formula::Bottom
                | Formula::Top
                | Formula::PropVar(..)
                | Formula::Not(_)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(..)
                | Formula::DiamondLe(..) => {
                    write!(f, "◇{subformula}")
                }
                _ => write!(f, "◇({subformula})"),
            },
            Formula::DiamondGe(count, subformula) => match subformula.as_ref() {
                Formula::Bottom | Formula::Top | Formula::PropVar(..) => {
                    write!(f, "◇≥{count} {subformula}")
                }
                Formula::Not(_)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(..)
                | Formula::DiamondLe(..) => {
                    write!(f, "◇≥{count}{subformula}")
                }
                _ => write!(f, "◇≥{count}({subformula})"),
            },
            Formula::DiamondLe(count, subformula) => match subformula.as_ref() {
                Formula::Bottom | Formula::Top | Formula::PropVar(..) => {
                    write!(f, "◇≤{count} {subformula}")
                }
                Formula::Not(_)
                | Formula::Box(_)
                | Formula::Diamond(_)
                | Formula::DiamondGe(..)
                | Formula::DiamondLe(..) => {
                    write!(f, "◇≤{count}{subformula}")
                }
                _ => write!(f, "◇≤{count}({subformula})"),
            },
            Formula::And(leftsubf, rightsubf) => {
                match leftsubf.as_ref() {
                    Formula::Bottom
                    | Formula::Top
                    | Formula::PropVar(..)
                    | Formula::Not(_)
                    | Formula::Box(_)
                    | Formula::Diamond(_)
                    | Formula::DiamondGe(..)
                    | Formula::DiamondLe(..)
                    | Formula::And(_, _) => write!(f, "{leftsubf} ∧ ")?,
                    _ => write!(f, "({leftsubf}) ∧ ")?,
                }
                match rightsubf.as_ref() {
                    Formula::Bottom
                    | Formula::Top
                    | Formula::PropVar(..)
                    | Formula::Not(_)
                    | Formula::Box(_)
                    | Formula::Diamond(_)
                    | Formula::DiamondGe(..)
                    | Formula::DiamondLe(..)
                    | Formula::And(_, _) => {
                        write!(f, "{rightsubf}")
                    }
                    _ => write!(f, "({rightsubf})"),
                }
            }
            Formula::Or(leftsubf, rightsubf) => {
                match leftsubf.as_ref() {
                    Formula::Bottom
                    | Formula::Top
                    | Formula::PropVar(..)
                    | Formula::Not(_)
                    | Formula::Box(_)
                    | Formula::Diamond(_)
                    | Formula::DiamondGe(..)
                    | Formula::DiamondLe(..)
                    | Formula::And(_, _)
                    | Formula::Or(_, _) => write!(f, "{leftsubf} ∨ ")?,
                    _ => write!(f, "({leftsubf}) ∨ ")?,
                }
                match rightsubf.as_ref() {
                    Formula::Bottom
                    | Formula::Top
                    | Formula::PropVar(..)
                    | Formula::Not(_)
                    | Formula::Box(_)
                    | Formula::Diamond(_)
                    | Formula::DiamondGe(..)
                    | Formula::DiamondLe(..)
                    | Formula::And(_, _)
                    | Formula::Or(_, _) => {
                        write!(f, "{rightsubf}")
                    }
                    _ => write!(f, "({rightsubf})"),
                }
            }
            Formula::Imply(leftsubf, rightsubf) => match leftsubf.as_ref() {
                Formula::Imply(_, _) => write!(f, "({leftsubf}) → {rightsubf}"),
                _ => write!(f, "{leftsubf} → {rightsubf}"),
            },
            Formula::Iff(leftsubf, rightsubf) => {
                match leftsubf.as_ref() {
                    Formula::Imply(_, _) => write!(f, "({leftsubf}) ↔ ")?,
                    _ => write!(f, "{leftsubf} ↔ ")?,
                }
                match rightsubf.as_ref() {
                    Formula::Imply(_, _) => write!(f, "({rightsubf})"),
                    _ => write!(f, "{rightsubf}"),
                }
            }
        }
    }
}

impl Formula {
    pub(crate) fn is_bottom(&self) -> bool {
        if let Formula::Bottom = self {
            true
        } else {
            false
        }
    }

    pub(crate) fn top() -> Rc<Formula> {
        Rc::new(Formula::Top)
    }

    pub(crate) fn bottom() -> Rc<Formula> {
        Rc::new(Formula::Bottom)
    }

    pub(crate) fn not(self: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::Not(self.clone()))
    }

    pub(crate) fn and(self: &Rc<Formula>, other: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::And(self.clone(), other.clone()))
    }

    pub(crate) fn or(self: &Rc<Formula>, other: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::Or(self.clone(), other.clone()))
    }

    pub(crate) fn imply(self: &Rc<Formula>, other: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::Imply(self.clone(), other.clone()))
    }

    pub(crate) fn iff(self: &Rc<Formula>, other: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::Iff(self.clone(), other.clone()))
    }

    pub(crate) fn diamond(self: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::Diamond(self.clone()))
    }

    pub(crate) fn dmge(self: &Rc<Formula>, count: u32) -> Rc<Formula> {
        if count == 0 {
            Formula::top()
        } else {
            Rc::new(Formula::DiamondGe(count, self.clone()))
        }
    }

    pub(crate) fn dmle(self: &Rc<Formula>, count: u32) -> Rc<Formula> {
        if count == 0 {
            self.not().box_()
        } else {
            Rc::new(Formula::DiamondLe(count, self.clone()))
        }
    }

    pub(crate) fn box_(self: &Rc<Formula>) -> Rc<Formula> {
        Rc::new(Formula::Box(self.clone()))
    }

    pub(crate) fn is_negation(&self, other: &Formula) -> bool {
        match (self, other) {
            (Formula::Not(phi1), phi2) => phi1.as_ref() == phi2,
            (phi1, Formula::Not(phi2)) => phi1 == phi2.as_ref(),
            _ => false,
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
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        |s| {
            parse_unit(s, |token| match token {
                Token::PROPVAR(c) => Ok(c),
                _ => Err(token),
            })
        },
        |s| {
            parse_option(s, |s2| {
                parse_unit(s2, |token| match token {
                    Token::NUM(n) => Ok(n),
                    _ => Err(token),
                })
            })
        },
        |c, n| Formula::PropVar(c, n),
    )
}

fn paren_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_snd(
        stream,
        |s| parse_eq(s, &Token::LPAREN, ()),
        |s| parse_fst(s, formula_parser, |s2| parse_eq(s2, &Token::RPAREN, ())),
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
            Box::new(box_parser),
            Box::new(diamond_parser),
            Box::new(dmge_parser),
            Box::new(dmle_parser),
            Box::new(var_parser),
            Box::new(bottom_parser),
        ] as [Box<DynParser<'a, Token, Formula, S>>; 8])
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

fn box_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        |s| parse_eq(s, &Token::BOX, ()),
        atom_parser,
        |_, formula| Formula::Box(Rc::new(formula)),
    )
}

fn diamond_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        |s| parse_eq(s, &Token::DIAMOND, ()),
        atom_parser,
        |_, formula| Formula::Diamond(Rc::new(formula)),
    )
}

fn dmge_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        |s| {
            parse_snd(
                s,
                |s2| parse_eq(s2, &Token::DIAMOND, ()),
                |s2| {
                    parse_snd(
                        s2,
                        |s3| parse_eq(s3, &Token::GTE, ()),
                        |s3| {
                            parse_unit(s3, |token| match token {
                                Token::NUM(n) => Ok(n),
                                _ => Err(token),
                            })
                        },
                    )
                },
            )
        },
        atom_parser,
        |n, formula| Formula::DiamondGe(n, Rc::new(formula)),
    )
}

fn dmle_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)> + Clone,
{
    parse_tup(
        stream,
        |s| {
            parse_snd(
                s,
                |s2| parse_eq(s2, &Token::DIAMOND, ()),
                |s2| {
                    parse_snd(
                        s2,
                        |s3| parse_eq(s3, &Token::LTE, ()),
                        |s3| {
                            parse_unit(s3, |token| match token {
                                Token::NUM(n) => Ok(n),
                                _ => Err(token),
                            })
                        },
                    )
                },
            )
        },
        atom_parser,
        |n, formula| Formula::DiamondLe(n, Rc::new(formula)),
    )
}

fn bottom_parser<S>(stream: S) -> Result<(Formula, S), Option<(usize, Token)>>
where
    S: Iterator<Item = (usize, Token)>,
{
    parse_eq(stream, &Token::BOTTOM, Formula::Bottom)
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

#[cfg(test)]
mod tests {
    use crate::token::tokenize;

    use super::*;

    fn prop(c: char) -> Formula {
        Formula::PropVar(c, None)
    }

    fn prop_n(c: char, n: u32) -> Formula {
        Formula::PropVar(c, Some(n))
    }

    fn not(f: Formula) -> Formula {
        Formula::Not(Rc::new(f))
    }

    fn boxm(f: Formula) -> Formula {
        Formula::Box(Rc::new(f))
    }

    fn diamond(f: Formula) -> Formula {
        Formula::Diamond(Rc::new(f))
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
        assert_eq!(parse_str("_|_"), Formula::Bottom);
        assert_eq!(parse_str("p"), prop('p'));
        assert_eq!(parse_str("p42"), prop_n('p', 42));
    }

    #[test]
    fn test_parse_not() {
        assert_eq!(parse_str("~q1"), not(prop_n('q', 1)));
        assert_eq!(parse_str("~~q2"), not(not(prop_n('q', 2))));
    }

    #[test]
    fn test_parse_modals() {
        assert_eq!(parse_str("<>p"), diamond(prop('p')));
        assert_eq!(parse_str("[]p0"), boxm(prop_n('p', 0)));
        assert_eq!(
            parse_str("<>~[]<>x2"),
            diamond(not(boxm(diamond(prop_n('x', 2)))))
        );
    }

    #[test]
    fn test_parse_and_or() {
        assert_eq!(parse_str("x1 & y2"), and(prop_n('x', 1), prop_n('y', 2)));
        assert_eq!(parse_str("x1 | y"), or(prop_n('x', 1), prop('y')));
        assert_eq!(
            parse_str("x1 & y2 | z"),
            or(and(prop_n('x', 1), prop_n('y', 2)), prop('z'))
        );
        assert_eq!(
            parse_str("x1 | y2 & z3"),
            or(prop_n('x', 1), and(prop_n('y', 2), prop_n('z', 3)))
        );
    }

    #[test]
    fn test_parse_imply_iff() {
        assert_eq!(parse_str("p1 -> p2"), imply(prop_n('p', 1), prop_n('p', 2)));
        assert_eq!(parse_str("p1 <-> p2"), iff(prop_n('p', 1), prop_n('p', 2)));
        assert_eq!(
            parse_str("p1 -> p2 -> p3"),
            imply(prop_n('p', 1), imply(prop_n('p', 2), prop_n('p', 3)))
        );
        assert_eq!(
            parse_str("p <-> p2 <-> _|_"),
            iff(prop('p',), iff(prop_n('p', 2), Formula::Bottom))
        );
    }

    #[test]
    fn test_parse_grouping() {
        assert_eq!(
            parse_str("~(p1 & p2)"),
            not(and(prop_n('p', 1), prop_n('p', 2)))
        );
        assert_eq!(
            parse_str("(p1 -> _|_) & p3"),
            and(imply(prop_n('p', 1), Formula::Bottom), prop_n('p', 3))
        );
        assert_eq!(
            parse_str("p1 -> (p2 & p3)"),
            imply(prop_n('p', 1), and(prop_n('p', 2), prop_n('p', 3)))
        );
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let tokens = vec![Token::AND, Token::PROPVAR('p'), Token::NUM(1)]
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
        assert_eq!(
            tokens,
            vec![Token::PROPVAR('p'), Token::NUM(1), Token::PROPVAR('p')]
        );
        let tokens = tokens.into_iter().enumerate();
        let err = full_parser(tokens).unwrap_err();
        assert_eq!(err, Some((2, Token::PROPVAR('p'))));
    }

    #[test]
    fn test_parse_error_wrong_diamond() {
        let tokens = tokenize("p1 <> p2").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::PROPVAR('p'),
                Token::NUM(1),
                Token::DIAMOND,
                Token::PROPVAR('p'),
                Token::NUM(2)
            ]
        );
        let tokens = tokens.into_iter().enumerate();
        let err = full_parser(tokens).unwrap_err();
        assert_eq!(err, Some((2, Token::DIAMOND)));
    }

    #[test]
    fn test_parse_error_wrong_box() {
        let tokens = tokenize("p1 [] p2").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::PROPVAR('p'),
                Token::NUM(1),
                Token::BOX,
                Token::PROPVAR('p'),
                Token::NUM(2)
            ]
        );
        let tokens = tokens.into_iter().enumerate();
        let err = full_parser(tokens).unwrap_err();
        assert_eq!(err, Some((2, Token::BOX)));
    }
}
