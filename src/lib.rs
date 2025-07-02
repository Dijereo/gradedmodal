use std::io::{self, Write};

use crate::{formula::full_parser, token::tokenize};

mod formula;
mod parser;
mod token;

pub fn run() {
    print_tokens();
}

fn print_tokens() {
    print!("Enter a formula: ");
    io::stdout().flush().unwrap();

    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        eprintln!("Failed to read input");
        return;
    }

    match tokenize(input.trim()) {
        Ok(tokens) => {
            for token in &tokens {
                print!("{:?} ", token);
            }
            println!();
            println!();

            let stream = tokens.into_iter().enumerate();
            match full_parser(stream) {
                Ok(f) => print!("{:#?}\n", f),
                Err(Some((i, tok))) => {
                    eprintln!("Error: bad token sequence '{:#?}' at index {}", tok, i)
                }
                Err(None) => eprintln!("Error: unterminated token sequence"),
            }
        }
        Err((idx, ch)) => {
            eprintln!(
                "Error: bad character sequence '{}' at byte index {}",
                ch, idx
            );
        }
    }
}
