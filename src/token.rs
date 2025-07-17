#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    BOTTOM,
    NOT,
    AND,
    OR,
    IMPLY,
    IFF,
    LPAREN,
    RPAREN,
    BOX,
    DIAMOND,
    PROPVAR(char, Option<u8>),
}

pub(crate) fn tokenize(input: &str) -> Result<Vec<Token>, (usize, char)> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();

    while let Some(&(i, ch)) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' => {
                chars.next();
            }
            '~' => {
                tokens.push(Token::NOT);
                chars.next();
            }
            '&' => {
                tokens.push(Token::AND);
                chars.next();
            }
            '|' => {
                tokens.push(Token::OR);
                chars.next();
            }
            '-' => {
                chars.next();
                match chars.peek() {
                    Some(&(_, '>')) => {
                        chars.next();
                        tokens.push(Token::IMPLY);
                    }
                    _ => return Err((i, ch)),
                }
            }
            '<' => {
                chars.next();
                match chars.next() {
                    Some((_, '-')) => match chars.next() {
                        Some((_, '>')) => tokens.push(Token::IFF),
                        _ => return Err((i, ch)),
                    },
                    Some((_, '>')) => tokens.push(Token::DIAMOND),
                    _ => return Err((i, ch)),
                }
            }
            '_' => {
                chars.next();
                match (chars.next(), chars.next()) {
                    (Some((_, '|')), Some((_, '_'))) => tokens.push(Token::BOTTOM),
                    _ => return Err((i, ch)),
                }
            }
            '(' => {
                tokens.push(Token::LPAREN);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RPAREN);
                chars.next();
            }
            p if p.is_ascii_lowercase() => {
                chars.next();
                let mut num = String::new();
                while let Some((_, c)) = chars.peek() {
                    if c.is_ascii_digit() {
                        num.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let n = num.parse::<u8>().ok();
                tokens.push(Token::PROPVAR(p, n));
            }
            '[' => {
                chars.next();
                match chars.peek() {
                    Some(&(_, ']')) => {
                        chars.next();
                        tokens.push(Token::BOX);
                    }
                    _ => return Err((i, ch)),
                }
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
        assert_eq!(tokenize("_|_").unwrap(), vec![Token::BOTTOM]);
        assert_eq!(tokenize("~").unwrap(), vec![Token::NOT]);
        assert_eq!(tokenize("&").unwrap(), vec![Token::AND]);
        assert_eq!(tokenize("|").unwrap(), vec![Token::OR]);
        assert_eq!(tokenize("->").unwrap(), vec![Token::IMPLY]);
        assert_eq!(tokenize("<->").unwrap(), vec![Token::IFF]);
        assert_eq!(tokenize("(").unwrap(), vec![Token::LPAREN]);
        assert_eq!(tokenize(")").unwrap(), vec![Token::RPAREN]);
        assert_eq!(tokenize("<>").unwrap(), vec![Token::DIAMOND]);
        assert_eq!(tokenize("[]").unwrap(), vec![Token::BOX]);
        assert_eq!(tokenize("p").unwrap(), vec![Token::PROPVAR('p', None)]);
        assert_eq!(tokenize("q0").unwrap(), vec![Token::PROPVAR('q', Some(0))]);
        assert_eq!(tokenize("x123").unwrap(), vec![Token::PROPVAR('x', Some(123))]);
    }

    #[test]
    fn test_multiple_tokens() {
        let input = "~ p1 & []~( p23 | z |_|_ ) -> <>p4 <-> o5";
        let expected = vec![
            Token::NOT,
            Token::PROPVAR('p', Some(1)),
            Token::AND,
            Token::BOX,
            Token::NOT,
            Token::LPAREN,
            Token::PROPVAR('p', Some(23)),
            Token::OR,
            Token::PROPVAR('z', None),
            Token::OR,
            Token::BOTTOM,
            Token::RPAREN,
            Token::IMPLY,
            Token::DIAMOND,
            Token::PROPVAR('p', Some(4)),
            Token::IFF,
            Token::PROPVAR('o', Some(5)),
        ];
        assert_eq!(tokenize(input).unwrap(), expected);
    }

    #[test]
    fn test_invalid_character() {
        let input = "p1 ^ p2";
        let result = tokenize(input);
        assert_eq!(result, Err((3, '^')));
    }

    #[test]
    fn test_incomplete_arrow() {
        let input = "p1 - p2";
        let result = tokenize(input);
        assert_eq!(result, Err((3, '-')));
    }

    #[test]
    fn test_incomplete_iff() {
        let input = "p1 < - > p2";
        let result = tokenize(input);
        assert_eq!(result, Err((3, '<')));
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
