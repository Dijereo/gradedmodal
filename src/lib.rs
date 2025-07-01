use std::io::{self, Write};

use crate::token::tokenize;

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
            for token in tokens {
                print!("{:?} ", token);
            }
            println!();
        }
        Err((idx, ch)) => {
            eprintln!("Error: bad character sequence '{}' at byte index {}", ch, idx);
        }
    }
}
