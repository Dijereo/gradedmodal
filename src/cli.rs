use std::{
    env,
    io::{self, Write},
    mem,
};

use crate::{
    eval::eval_provers, formula::full_parser, frame::FrameCondition, randgen::gen_formulae,
    token::tokenize,
};

pub fn run() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return interactive_mode();
    }

    match args[1].as_str() {
        "-f" => {
            if args.len() != 4 {
                eprintln!("Usage: {} -f <input.txt> <output.json>", args[0]);
                std::process::exit(1);
            }
            file_mode(&args[2], &args[3]);
        }
        "-e" => {
            if args.len() != 3 {
                eprintln!("Usage: {} -e <time_in_seconds>", args[0]);
                std::process::exit(1);
            }
            if let Err(e) = eval_provers("eval/output.json", mem::take(&mut args[2]).leak()) {
                eprintln!("{e}");
            }
        }
        "-d" => {
            if args.len() != 6 {
                eprintln!(
                    "Usage: {} -d <num_rand_formulae> <seed_int> <crafted_formulae_txt_file> <output_json_file>",
                    args[0]
                );
                std::process::exit(1);
            }
            let n: usize = match args[2].parse() {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("Invalid num: {e}");
                    std::process::exit(1);
                }
            };
            let seed: u64 = match args[3].parse() {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("Invalid num: {e}");
                    std::process::exit(1);
                }
            };
            if let Err(e) = gen_formulae(n, seed, &args[4], &args[5]) {
                eprintln!("{e}");
            }
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
