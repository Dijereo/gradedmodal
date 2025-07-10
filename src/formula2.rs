// use std::{fmt, rc::Rc};

// use crate::{
//     parser::{parse_eq, parse_fst, parse_snd},
//     token::Token,
// };

// #[derive(Clone, Debug, PartialEq)]
// enum Unit {
//     Bottom,
//     Var(char),
//     VarIdx(char, u8),
//     Paren(Rc<SubFormulae>),
// }

// impl fmt::Display for Unit {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Unit::Bottom => write!(f, "⊥"),
//             Unit::Var(c) => write!(f, "{c}"),
//             Unit::VarIdx(c, i) => write!(f, "{c}{i}"),
//             Unit::Paren(phi) => write!(f, "({phi})"),
//         }
//     }
// }

// impl Unit {
//     fn unit_parser<'a, S>(stream: S) -> Self
//     where
//         S: Iterator<Item = (usize, &'a Token)>,
//     {
//         parse_any(
//             stream,
//             ([
//                 Box::new(paren_parser),
//                 Box::new(not_parser),
//                 Box::new(box_parser),
//                 Box::new(diamond_parser),
//                 Box::new(var_parser),
//                 Box::new(bottom_parser),
//             ] as [Box<DynParser<'a, Token, Formula, S>>; 6])
//                 .into_iter(),
//         )
//     }

//     fn paren_parser<'a, S>(stream: S) -> Result<(Unit, S), Option<(usize, &'a Token)>>
//     where
//         S: Iterator<Item = (usize, &'a Token)>,
//     {
//         parse_snd(
//             stream,
//             |s| parse_eq(s, &Token::LPAREN, ()),
//             |s| parse_fst(s, subformulae_parser, |s2| parse_eq(s2, &Token::RPAREN, ())),
//         )
//     }

//     fn atom_parser<'a, S>(mut stream: S) -> Result<(Self, S), Option<(usize, &'a Token)>>
//     where
//         S: Iterator<Item = (usize, &'a Token)>,
//     {
//         if let Some((i, tok)) = stream.next() {
//             match tok {
//                 Token::BOTTOM => Ok((Self::Bottom, stream)),
//                 Token::PROPVAR(Some(n)) => Ok((Self::VarIdx('p', *n), stream)),
//                 Token::PROPVAR(None) => Ok((Self::Var('p'), stream)),
//                 _ => Err(Some((i, tok))),
//             }
//         } else {
//             Err(None)
//         }
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// enum Unary {
//     Not,
//     Box,
//     Diamond,
// }

// impl fmt::Display for Unary {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Unary::Not => write!(f, "¬"),
//             Unary::Box => write!(f, "□"),
//             Unary::Diamond => write!(f, "◇"),
//         }
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// enum Connective {
//     And,
//     Or,
//     Imply,
//     Iff,
// }

// impl fmt::Display for Connective {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Connective::And => write!(f, "∧"),
//             Connective::Or => write!(f, "∨"),
//             Connective::Imply => write!(f, "→"),
//             Connective::Iff => write!(f, "↔"),
//         }
//     }
// }

// impl Connective {
//     fn should_paren(&self, conn2: &Connective) -> bool {
//         match (self, conn2) {
//             (_, Connective::And) => false,
//             (Connective::And, _) => true,
//             (_, Connective::Or) => false,
//             (Connective::Or, _) => true,
//             (_, Connective::Iff) => false,
//             (_, Connective::Imply) => true,
//         }
//     }

//     fn write_subformula(&self, phi: &Formula2, f: &mut fmt::Formatter) -> fmt::Result {
//         match phi {
//             Formula2::SubFormulae(SubFormulae(conn2, ..)) => {
//                 if self.should_paren(conn2) {
//                     write!(f, "({phi})")
//                 } else {
//                     write!(f, "{phi}")
//                 }
//             }
//             Formula2::Atom(..) => write!(f, "{phi}"),
//         }
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// struct SubFormulae(Connective, Rc<Formula2>, Rc<Formula2>, Vec<Formula2>);

// impl fmt::Display for SubFormulae {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let SubFormulae(conn, phi0, phi1, phis) = self;
//         write!(f, "{phi0} {conn} {phi1}")?;
//         for phi in phis {
//             write!(f, " {conn} {phi}")?;
//         }
//         Ok(())
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub(crate) enum Formula2 {
//     SubFormulae(SubFormulae),
//     Atom(Vec<Unary>, Unit),
// }

// impl fmt::Display for Formula2 {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Formula2::SubFormulae(phi) => {
//                 write!(f, "{phi}")
//             }
//             Formula2::Atom(unaries, unit) => {
//                 for unary in unaries {
//                     write!(f, "{unary}")?;
//                 }
//                 write!(f, "{unit}")
//             }
//         }
//     }
// }
