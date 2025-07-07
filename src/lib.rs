use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{formula::full_parser, signed::SignedTableau, token::tokenize};

mod formula;
mod parser;
mod signed;
mod tableau;
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
                Ok(f) => {
                    // println!("{:#?}", f);
                    // println!();
                    // println!();
                    // println!("{}", f);
                    let tab = SignedTableau::create((false, Rc::new(f)));
                    println!();
                    println!();
                    // println!("{:#?}", tab);
                    println!("{}", tab.to_tree_string());
                    // println!("{}", Formula::PropVar(Some(0)) == Formula::PropVar(None))
                    // println!();
                    // println!();
                    // tab.expand(vec![]);
                    // println!("{:#?}", tab);
                    // println!("{}", tab.to_tree_string());
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
