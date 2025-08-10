#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    BOTTOM,
    TOP,
    NOT,
    AND,
    OR,
    IMPLY,
    IFF,
    LPAREN,
    RPAREN,
    BOX,
    DIAMOND,
    LTE,
    GTE,
    LT,
    GT,
    EQ,
    NEQ,
    PROPVAR(char),
    NUM(u32),
}

pub(crate) fn tokenize(input: &str) -> Result<Vec<Token>, (usize, char)> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();

    while let Some(&(i, ch)) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' => {
                chars.next();
            }
            d if d.is_ascii_digit() => {
                chars.next();
                let mut num = String::new();
                num.push(d);
                while let Some((_, d)) = chars.peek() {
                    if d.is_ascii_digit() {
                        num.push(*d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let n = num
                    .parse::<u32>()
                    .expect("Should contain only and at least one digit");
                tokens.push(Token::NUM(n));
            }
            p if p.is_ascii_lowercase() => {
                chars.next();
                tokens.push(Token::PROPVAR(p));
            }
            '(' => {
                tokens.push(Token::LPAREN);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RPAREN);
                chars.next();
            }
            '~' | '¬' => {
                tokens.push(Token::NOT);
                chars.next();
            }
            '^' | '∧' => {
                tokens.push(Token::AND);
                chars.next();
            }
            '&' => {
                chars.next();
                while let Some((_, '&')) = chars.peek() {
                    chars.next();
                }
                tokens.push(Token::AND);
            }
            'V' | '∨' => {
                tokens.push(Token::OR);
                chars.next();
            }
            '|' => {
                chars.next();
                while let Some((_, '|')) = chars.peek() {
                    chars.next();
                }
                tokens.push(Token::OR);
            }
            '<' => {
                chars.next();
                match chars.peek() {
                    Some((_, '-')) => {
                        chars.next();
                        match chars.next() {
                            Some((_, '>')) => tokens.push(Token::IFF),
                            _ => return Err((i, ch)),
                        }
                    }
                    Some((_, '>')) => {
                        chars.next();
                        tokens.push(Token::DIAMOND)
                    }
                    Some((_, '=')) => {
                        chars.next();
                        tokens.push(Token::LTE)
                    }
                    _ => tokens.push(Token::LT),
                }
            }
            '!' => {
                chars.next();
                match chars.peek() {
                    Some((_, '=')) => {
                        while let Some((_, '=')) = chars.peek() {
                            chars.next();
                        }
                        tokens.push(Token::NEQ);
                    }
                    Some((_, p)) if p.is_ascii_lowercase() => tokens.push(Token::NOT),
                    _ => tokens.push(Token::BOTTOM),
                }
            }
            '≠' => {
                tokens.push(Token::NEQ);
                chars.next();
            }
            '=' => {
                chars.next();
                while let Some((_, '=')) = chars.peek() {
                    chars.next();
                }
                tokens.push(Token::EQ);
            }
            '≤' => {
                tokens.push(Token::LTE);
                chars.next();
            }
            '≥' => {
                tokens.push(Token::GTE);
                chars.next();
            }
            '◇' => {
                tokens.push(Token::DIAMOND);
                chars.next();
            }
            '□' => {
                tokens.push(Token::BOX);
                chars.next();
            }
            '[' => {
                chars.next();
                match chars.peek() {
                    Some((_, ']')) => {
                        chars.next();
                        tokens.push(Token::BOX);
                    }
                    _ => return Err((i, ch)),
                }
            }
            '-' => {
                chars.next();
                match chars.peek() {
                    Some((_, '>')) => {
                        chars.next();
                        tokens.push(Token::IMPLY);
                    }
                    _ => {
                        tokens.push(Token::NOT);
                    }
                }
            }
            '→' => {
                tokens.push(Token::IMPLY);
                chars.next();
            }
            '↔' => {
                tokens.push(Token::IFF);
                chars.next();
            }
            '>' => {
                chars.next();
                match chars.peek() {
                    Some(&(_, '=')) => {
                        chars.next();
                        tokens.push(Token::GTE);
                    }
                    _ => tokens.push(Token::GT),
                }
            }
            '_' => {
                chars.next();
                match (chars.next(), chars.next()) {
                    (Some((_, '|')), Some((_, '_'))) => tokens.push(Token::BOTTOM),
                    _ => return Err((i, ch)),
                }
            }
            'T' | '⊤' => {
                tokens.push(Token::TOP);
                chars.next();
            }
            '⊥' => {
                tokens.push(Token::BOTTOM);
                chars.next();
            }
            _ => return Err((i, ch)),
        };
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_tokens() {
        assert_eq!(tokenize("⊤").unwrap(), vec![Token::TOP]);
        assert_eq!(tokenize("T").unwrap(), vec![Token::TOP]);
        assert_eq!(tokenize("⊥").unwrap(), vec![Token::BOTTOM]);
        assert_eq!(tokenize("!").unwrap(), vec![Token::BOTTOM]);
        assert_eq!(tokenize("_|_").unwrap(), vec![Token::BOTTOM]);
        assert_eq!(tokenize("p").unwrap(), vec![Token::PROPVAR('p')]);
        assert_eq!(
            tokenize("q0").unwrap(),
            vec![Token::PROPVAR('q'), Token::NUM(0)]
        );
        assert_eq!(
            tokenize("x123").unwrap(),
            vec![Token::PROPVAR('x'), Token::NUM(123)]
        );
        assert_eq!(tokenize("¬").unwrap(), vec![Token::NOT]);
        assert_eq!(tokenize("~").unwrap(), vec![Token::NOT]);
        assert_eq!(
            tokenize("!p").unwrap(),
            vec![Token::NOT, Token::PROPVAR('p')]
        );
        assert_eq!(tokenize("∧").unwrap(), vec![Token::AND]);
        assert_eq!(tokenize("^").unwrap(), vec![Token::AND]);
        assert_eq!(tokenize("&").unwrap(), vec![Token::AND]);
        assert_eq!(tokenize("&&").unwrap(), vec![Token::AND]);
        assert_eq!(tokenize("∨").unwrap(), vec![Token::OR]);
        assert_eq!(tokenize("V").unwrap(), vec![Token::OR]);
        assert_eq!(tokenize("|").unwrap(), vec![Token::OR]);
        assert_eq!(tokenize("||").unwrap(), vec![Token::OR]);
        assert_eq!(tokenize("=").unwrap(), vec![Token::EQ]);
        assert_eq!(tokenize("==").unwrap(), vec![Token::EQ]);
        assert_eq!(tokenize("===").unwrap(), vec![Token::EQ]);
        assert_eq!(tokenize("≠").unwrap(), vec![Token::NEQ]);
        assert_eq!(tokenize("!=").unwrap(), vec![Token::NEQ]);
        assert_eq!(tokenize("!==").unwrap(), vec![Token::NEQ]);
        assert_eq!(tokenize("≤").unwrap(), vec![Token::LTE]);
        assert_eq!(tokenize("<").unwrap(), vec![Token::LT]);
        assert_eq!(tokenize("≥").unwrap(), vec![Token::GTE]);
        assert_eq!(tokenize(">").unwrap(), vec![Token::GT]);
        assert_eq!(tokenize("◇").unwrap(), vec![Token::DIAMOND]);
        assert_eq!(tokenize("<>").unwrap(), vec![Token::DIAMOND]);
        assert_eq!(tokenize("□").unwrap(), vec![Token::BOX]);
        assert_eq!(tokenize("[]").unwrap(), vec![Token::BOX]);
        assert_eq!(tokenize("→").unwrap(), vec![Token::IMPLY]);
        assert_eq!(tokenize("->").unwrap(), vec![Token::IMPLY]);
        assert_eq!(tokenize("↔").unwrap(), vec![Token::IFF]);
        assert_eq!(tokenize("<->").unwrap(), vec![Token::IFF]);
        assert_eq!(tokenize("(").unwrap(), vec![Token::LPAREN]);
        assert_eq!(tokenize(")").unwrap(), vec![Token::RPAREN]);
    }

    #[test]
    fn test_multiple_tokens() {
        let input = "~ p1 & []~( l23 | i |_|_ ) -> <>≥6q4 <-> -o5 && !v1 ^ □¬( t23 V z ∨ ! ) → ◇≤7a4 ↔ T";
        let expected = vec![
            Token::NOT,
            Token::PROPVAR('p'),
            Token::NUM(1),
            Token::AND,
            Token::BOX,
            Token::NOT,
            Token::LPAREN,
            Token::PROPVAR('l'),
            Token::NUM(23),
            Token::OR,
            Token::PROPVAR('i'),
            Token::OR,
            Token::BOTTOM,
            Token::RPAREN,
            Token::IMPLY,
            Token::DIAMOND,
            Token::GTE,
            Token::NUM(6),
            Token::PROPVAR('q'),
            Token::NUM(4),
            Token::IFF,
            Token::NOT,
            Token::PROPVAR('o'),
            Token::NUM(5),
            Token::AND,
            Token::NOT,
            Token::PROPVAR('v'),
            Token::NUM(1),
            Token::AND,
            Token::BOX,
            Token::NOT,
            Token::LPAREN,
            Token::PROPVAR('t'),
            Token::NUM(23),
            Token::OR,
            Token::PROPVAR('z'),
            Token::OR,
            Token::BOTTOM,
            Token::RPAREN,
            Token::IMPLY,
            Token::DIAMOND,
            Token::LTE,
            Token::NUM(7),
            Token::PROPVAR('a'),
            Token::NUM(4),
            Token::IFF,
            Token::TOP,
        ];
        assert_eq!(tokenize(input).unwrap(), expected);
    }

    #[test]
    fn test_incomplete_bottom() {
        let input = "|_";
        let result = tokenize(input);
        assert_eq!(result, Err((1, '_')));
        let input = "_| p";
        let result = tokenize(input);
        assert_eq!(result, Err((0, '_')));
    }

    #[test]
    fn test_invalid_propvar() {
        let input = "pX";
        let result = tokenize(input);
        assert_eq!(result, Err((1, 'X')));
    }
}
