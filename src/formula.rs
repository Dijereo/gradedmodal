use std::{fmt, rc::Rc};

use crate::{
    parser::{
        DynParser, parse_any, parse_entire, parse_eq, parse_fst, parse_option, parse_snd,
        parse_tup, parse_unit,
    },
    token::Token,
};

#[derive(Debug)]
pub(crate) enum Formula {
    Bottom,
    Top,
    PropVar(char, Option<usize>),
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

    pub(crate) fn propi(p: char, i: usize) -> Rc<Formula> {
        Rc::new(Formula::PropVar(p, Some(i)))
    }

    pub(crate) fn prop(p: char) -> Rc<Formula> {
        Rc::new(Formula::PropVar(p, None))
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

    pub(crate) fn directly_equivalent(self: &Rc<Self>, other: &Rc<Self>) -> bool {
        Rc::as_ptr(self) == Rc::as_ptr(other)
            || match (self.as_ref(), other.as_ref()) {
                (Formula::Not(phi1), _) => phi1.directly_contradicts(other),
                (_, Formula::Not(phi2)) => self.directly_contradicts(phi2),
                (Formula::Bottom, Formula::Bottom) => true,
                (Formula::Top, Formula::Top) => true,
                (Formula::Top, Formula::DiamondGe(0, _)) => true,
                (Formula::DiamondGe(0, _), Formula::Top) => true,
                (Formula::PropVar(c1, i1), Formula::PropVar(c2, i2)) => c1 == c2 && i1 == i2,
                (Formula::Box(phi1), Formula::Box(phi2)) => phi1.directly_equivalent(phi2),
                (Formula::Box(phi1), Formula::DiamondLe(0, phi2)) => {
                    phi1.directly_contradicts(phi2)
                }
                (Formula::DiamondLe(0, phi1), Formula::Box(phi2)) => {
                    phi1.directly_contradicts(phi2)
                }
                (Formula::Diamond(phi1), Formula::Diamond(phi2)) => phi1.directly_equivalent(phi2),
                (Formula::Diamond(phi1), Formula::DiamondGe(1, phi2)) => {
                    phi1.directly_equivalent(phi2)
                }
                (Formula::DiamondGe(1, phi1), Formula::Diamond(phi2)) => {
                    phi1.directly_equivalent(phi2)
                }
                (Formula::DiamondGe(c1, phi1), Formula::DiamondGe(c2, phi2)) => {
                    c1 == c2 && phi1.directly_equivalent(phi2)
                }
                (Formula::And(phi0, phi1), Formula::And(phi2, phi3)) => {
                    phi0.directly_equivalent(phi2) && phi1.directly_equivalent(phi3)
                }
                (Formula::Or(phi0, phi1), Formula::Or(phi2, phi3)) => {
                    phi0.directly_equivalent(phi2) && phi1.directly_equivalent(phi3)
                }
                (Formula::Imply(phi0, phi1), Formula::Imply(phi2, phi3)) => {
                    phi0.directly_equivalent(phi2) && phi1.directly_equivalent(phi3)
                }
                (Formula::Iff(phi0, phi1), Formula::Iff(phi2, phi3)) => {
                    phi0.directly_equivalent(phi2) && phi1.directly_equivalent(phi3)
                }
                _ => false,
            }
    }

    pub(crate) fn directly_contradicts(self: &Rc<Formula>, other: &Rc<Formula>) -> bool {
        Rc::as_ptr(self) != Rc::as_ptr(other)
            && match (self.as_ref(), other.as_ref()) {
                (Formula::Not(phi1), _) => phi1.directly_equivalent(other),
                (_, Formula::Not(phi2)) => self.directly_equivalent(phi2),
                (Formula::DiamondGe(c1, phi1), Formula::DiamondLe(c2, phi2)) => {
                    c1 > c2 && phi1.directly_equivalent(phi2)
                }
                (Formula::Diamond(phi1), Formula::DiamondLe(c2, phi2)) => {
                    *c2 < 1 && phi1.directly_equivalent(phi2)
                }
                (Formula::DiamondGe(c1, phi1), Formula::Box(phi2)) => {
                    *c1 > 0 && phi1.directly_contradicts(phi2)
                }
                (Formula::Diamond(phi1), Formula::Box(phi2)) => phi1.directly_contradicts(phi2),
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
                    Token::NUM(n) => Ok(n as usize),
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
    use std::rc::Rc;

    use crate::token::tokenize;

    use super::*;

    fn parse_str(input: &str) -> Rc<Formula> {
        let tokens = tokenize(input).unwrap();
        Rc::new(full_parser(tokens.into_iter().enumerate()).unwrap())
    }

    #[test]
    fn test_parse_propvar() {
        assert!(parse_str("_|_").directly_equivalent(&Formula::bottom()));
        assert!(parse_str("p").directly_equivalent(&Formula::prop('p')));
        assert!(parse_str("p42").directly_equivalent(&Formula::propi('p', 42)));
    }

    #[test]
    fn test_parse_not() {
        assert!(parse_str("~q1").directly_equivalent(&Formula::propi('q', 1).not()));
        assert!(parse_str("~~q2").directly_equivalent(&Formula::propi('q', 2).not().not()));
    }

    #[test]
    fn test_parse_modals() {
        assert!(parse_str("<>p").directly_equivalent(&Formula::prop('p').diamond()));
        assert!(parse_str("[]p0").directly_equivalent(&Formula::propi('p', 0).box_()));
        assert!(
            parse_str("<>~[]<>x2")
                .directly_equivalent(&Formula::propi('x', 2).diamond().box_().not().diamond())
        );
    }

    #[test]
    fn test_parse_and_or() {
        println!("{}", parse_str("x1 & y2"));
        println!("{}", &Formula::propi('x', 1).and(&Formula::propi('y', 2)));
        assert!(
            parse_str("x1 & y2")
                .directly_equivalent(&Formula::propi('x', 1).and(&Formula::propi('y', 2)))
        );
        assert!(
            parse_str("x1 | y")
                .directly_equivalent(&Formula::propi('x', 1).or(&Formula::prop('y')))
        );
        assert!(
            parse_str("x1 & y2 | z").directly_equivalent(
                &Formula::propi('x', 1)
                    .and(&Formula::propi('y', 2))
                    .or(&Formula::prop('z'))
            )
        );
        assert!(parse_str("x1 | y2 & z3").directly_equivalent(
            &Formula::propi('x', 1).or(&Formula::propi('y', 2).and(&Formula::propi('z', 3)))
        ));
    }

    #[test]
    fn test_parse_imply_iff() {
        assert!(
            parse_str("p1 -> p2")
                .directly_equivalent(&Formula::propi('p', 1).imply(&Formula::propi('p', 2)))
        );
        assert!(
            parse_str("p1 <-> p2")
                .directly_equivalent(&Formula::propi('p', 1).iff(&Formula::propi('p', 2)))
        );
        assert!(parse_str("p1 -> p2 -> p3").directly_equivalent(
            &Formula::propi('p', 1).imply(&Formula::propi('p', 2).imply(&Formula::propi('p', 3)))
        ));
        assert!(parse_str("p <-> p2 <-> _|_").directly_equivalent(
            &Formula::prop('p',).iff(&Formula::propi('p', 2).iff(&Formula::bottom()))
        ));
    }

    #[test]
    fn test_parse_grouping() {
        assert!(
            parse_str("~(p1 & p2)")
                .directly_equivalent(&Formula::propi('p', 1).and(&Formula::propi('p', 2)).not())
        );
        assert!(
            parse_str("(p1 -> _|_) & p3").directly_equivalent(
                &Formula::propi('p', 1)
                    .imply(&Formula::bottom())
                    .and(&Formula::propi('p', 3))
            )
        );
        assert!(parse_str("p1 -> (p2 & p3)").directly_equivalent(
            &Formula::propi('p', 1).imply(&Formula::propi('p', 2).and(&Formula::propi('p', 3)))
        ));
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
