use std::{
    env,
    fmt::Write,
    io::{self, Write as _},
    mem,
    sync::Arc,
};

use crate::{
    eval::{MyProver, Vampire, eval_prover},
    formula::full_parser,
    frame::FrameCondition,
    randgen::gen_formulae,
    token::tokenize,
};

pub fn run() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return help_mode(&args[0]);
    }

    match args[1].as_str() {
        "-i" => interactive_mode(),
        "-h" => help_mode(&args[0]),
        "-v" => {
            if args.len() != 4 {
                eprintln!("Usage: {} -v <data_file.json> <time_in_seconds>", args[0]);
                std::process::exit(1);
            }
            let datajson: &'static str = mem::take(&mut args[2]).leak();
            let vampire = Vampire::new("eval/temp.p", &args[3], "vampire");
            if let Err(e) = eval_prover(datajson, vampire) {
                eprintln!("{e}");
            }
        }
        "-p" => {
            if args.len() < 4 || args.len() > 5 {
                eprintln!(
                    "Usage: {} -p <data_file.json> <time_in_seconds> [<testid>]",
                    args[0]
                );
                std::process::exit(1);
            }
            let datajson: &'static str = mem::take(&mut args[2]).leak();
            let result = if args.len() == 5 {
                let mut key = String::with_capacity("prover".len() + args[4].len());
                write!(&mut key, "prover{}", args[4]).unwrap();
                let key: &'static str = key.leak();
                let prover = MyProver::new(&args[3], key);
                eval_prover(datajson, prover)
            } else {
                let prover = MyProver::new(&args[3], "prover");
                eval_prover(datajson, prover)
            };
            if let Err(e) = result {
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
            help_mode(&args[0]);
            std::process::exit(1);
        }
    }
}

fn help_mode(args0: &str) {
    println!(
        "Usage: {args0} -d <num_rand_formulae> <seed_int> <crafted_formulae_txt_file> <output_json_file> # Generate dataset",
    );
    println!("Usage: {args0} -v <time_in_seconds> # Evaluate vampire on dataset");
    eprintln!(
        "Usage: {args0} -p <data_file.json> <time_in_seconds> [<testid>] # Evaluate prover on dataset",
    );
    eprintln!("Usage: {args0} -i # Interactive Mode");
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
