use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{
    formula::full_parser,
    rules::{RuleCalc, T_CALCULUS},
    token::tokenize,
};

mod formula;
mod formula2;
mod parser;
mod parser2;
mod rules;
mod signed;
mod tableau;
mod token;
mod util;

pub fn run() {
    loop {
        print_tokens();
    }
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
                Ok(f) => {
                    let f = Rc::new(f);
                    let tab = T_CALCULUS.sat(vec![f]);
                    println!("{}", tab);
                }
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
