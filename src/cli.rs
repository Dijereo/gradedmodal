use std::io::{self, Write};

use crate::{formula::full_parser, frame::FrameCondition, token::tokenize};

use std::env;

pub fn run() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return interactive_mode();
    }

    match args[1].as_str() {
        "-f" => {
            if args.len() != 4 {
                eprintln!("Usage: {} -f <input.json> <output.json>", args[0]);
                std::process::exit(1);
            }
            file_mode(&args[2], &args[3]);
        }
        _ => {
            eprintln!("Unknown option: {}", args[1]);
            eprintln!("Usage: {} [-f input.json output.json]", args[0]);
            std::process::exit(1);
        }
    }
}

fn file_mode(input: &str, output: &str) {
    println!("Processing {} -> {}", input, output);
    todo!();
}

fn interactive_mode() {
    let mut framecond = FrameCondition::K;
    loop {
        print!("Choose Frame Class: ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("Failed to read input");
            return;
        }
        framecond = input.parse().unwrap_or(framecond);
        println!("Chosen Frame Class: {:?}", framecond);
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
                        println!("{}", f);
                        println!();
                        framecond.print_sat(f);
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
}
