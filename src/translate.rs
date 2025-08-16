use std::{cell::Cell, fmt, rc::Rc};

use crate::{formula::Formula, frame::FrameCondition};

struct STFrames<'a> {
    formula: &'a Rc<Formula>,
    frames: FrameCondition,
}

struct ST<'a>(&'a Rc<Formula>, &'a Cell<usize>, usize);

impl fmt::Display for STFrames<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.frames.reflexive() {
            writeln!(f, "fof(t,axiom,(![X]: rel(X, X))).")?;
        } else if self.frames.ray() {
            writeln!(f, "fof(d,axiom,(![X]: (?[Y]: rel(X, Y)))).")?;
        }
        if self.frames.symmetric() {
            writeln!(f, "fof(b,axiom,(![X,Y]: (rel(X, Y) => rel(Y, X)))).")?;
        }
        if self.frames.transitive() {
            writeln!(
                f,
                "fof(ax4,axiom,(![X,Y,Z]: ((rel(X, Y) & rel(Y, Z)) => rel(X, Z))))."
            )?;
        }
        if self.frames.euclidean() {
            writeln!(
                f,
                "fof(ax5,axiom,(![X,Y,Z]: ((rel(X, Y) & rel(X, Z)) => rel(Y, Z))))."
            )?;
        }
        let cell = Cell::new(0);
        writeln!(
            f,
            "fof(phi,conjecture,(![X{}]: ({})))",
            cell.get(),
            ST(self.formula, &cell, cell.get())
        )
    }
}

impl fmt::Display for ST<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.as_ref() {
            Formula::Bottom => write!(f, "$false"),
            Formula::Top => write!(f, "$true"),
            Formula::PropVar(p, None) => write!(f, "{p}(X{})", self.2),
            Formula::PropVar(p, Some(i)) => write!(f, "{p}{i}(X{})", self.2),
            Formula::Not(phi) => write!(f, "~({})", ST(phi, self.1, self.2)),
            Formula::Box(phi) => {
                let j = self.1.get() + 1;
                self.1.set(j);
                write!(
                    f,
                    "![X{}]: (rel(X{}, X{}) => ({}))",
                    j,
                    self.2,
                    j,
                    ST(phi, self.1, j)
                )
            }
            Formula::Diamond(phi) => {
                let j = self.1.get() + 1;
                self.1.set(j);
                write!(
                    f,
                    "?[X{}]: (rel(X{}, X{}) & ({}))",
                    j,
                    self.2,
                    j,
                    ST(phi, self.1, j)
                )
            }
            Formula::DiamondGe(c, phi) => {
                if *c == 0 {
                    return write!(f, "$true");
                }
                let i = self.1.get();
                let k = i + (*c as usize);
                print!("{i} {k} {c}");
                self.1.set(k);
                write!(f, "?[X{}", i + 1)?;
                for j in i + 2..=k {
                    write!(f, ", X{j}")?;
                }
                write!(f, "]: (")?;
                write!(f, "rel(X{}, X{})", self.2, i + 1)?;
                for j in i + 2..=k {
                    write!(f, " & rel(X{}, X{})", self.2, j)?;
                }
                for j in i + 1..=k {
                    for j2 in j + 1..=k {
                        write!(f, " & (X{} != X{})", j, j2)?;
                    }
                }
                for j in i + 1..=k {
                    write!(f, " & ({})", ST(phi, self.1, j))?;
                }
                write!(f, ")")
            }
            Formula::DiamondLe(c, phi) => {
                let i = self.1.get();
                let k = i + (*c as usize) + 1;
                self.1.set(k);
                write!(f, "![X{}", i + 1)?;
                for j in i + 2..=k {
                    write!(f, ", X{j}")?;
                }
                write!(f, "]: (")?;
                write!(f, "~rel(X{}, X{})", self.2, i + 1)?;
                for j in i + 2..=k {
                    write!(f, " | ~rel(X{}, X{})", self.2, j)?;
                }
                for j in i + 1..=k {
                    for j2 in j + 1..=k {
                        write!(f, " | (X{} = X{})", j, j2)?;
                    }
                }
                for j in i + 1..=k {
                    write!(f, " | ~({})", ST(phi, self.1, j))?;
                }
                write!(f, ")")
            }
            Formula::And(phi1, phi2) => write!(
                f,
                "({}) & ({})",
                ST(phi1, self.1, self.2),
                ST(phi2, self.1, self.2)
            ),
            Formula::Or(phi1, phi2) => write!(
                f,
                "({}) | ({})",
                ST(phi1, self.1, self.2),
                ST(phi2, self.1, self.2)
            ),
            Formula::Imply(phi1, phi2) => write!(
                f,
                "({}) => ({})",
                ST(phi1, self.1, self.2),
                ST(phi2, self.1, self.2)
            ),
            Formula::Iff(phi1, phi2) => write!(
                f,
                "({}) <=> ({})",
                ST(phi1, self.1, self.2),
                ST(phi2, self.1, self.2)
            ),
        }
    }
}

mod test {
    use super::*;
    use crate::{formula::full_parser, frame::FrameCondition, token::tokenize};
    use std::{fmt::Write, path::Path};

    fn assert_str_eq_file(actual: &str, path: impl AsRef<Path>) {
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.as_ref().display(), e));
        assert_eq!(content, actual);
    }

    fn template_test_st(formula: &str, path: impl AsRef<Path>, frames: FrameCondition) {
        let mut tptp = String::new();
        let tokens = match tokenize(formula) {
            Ok(tokens) => tokens,
            Err((i, c)) => panic!("Tokenization failed at {i}: {c}"),
        };
        let formula = match full_parser(tokens.into_iter().enumerate()) {
            Ok(formula) => formula,
            Err(None) => panic!("Parsing not terminated"),
            Err(Some((i, t))) => panic!("Parsing failed at {i}: {t:?}"),
        };
        write!(
            &mut tptp,
            "{}",
            STFrames {
                formula: &formula,
                frames
            }
        )
        .expect("TPTP write should not fail");
        assert_str_eq_file(&tptp, path);
    }

    #[test]
    fn test_fof_st() {
        template_test_st(
            "[](p->q)->[]p->[]q",
            "test/test_st_k_s5.txt",
            FrameCondition::S5,
        );
        template_test_st(
            "<>>=5 p0 & <><><=4 p0",
            "test/test_st_0_d.txt",
            FrameCondition::D,
        );
    }
}
