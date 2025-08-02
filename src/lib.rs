use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{
    formula::full_parser, frame::FrameCondition, rules3::GradedKCalc, tableau2::DisplayTableau,
    token::tokenize,
};

mod dnf;
mod flatformula;
mod formula;
mod frame;
mod ilp;
mod parser;
mod rules;
mod rules3;
mod rules4;
mod signed;
mod tableau;
mod tableau2;
mod token;
mod util;

pub fn run() {
    let mut framecond = FrameCondition::K;
    loop {
        print!("Choose Frame Class: ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("Failed to read input");
            return;
        }

        framecond = match input.trim().to_uppercase().as_str() {
            "K" => FrameCondition::K,
            "D" => FrameCondition::D,
            "K4" => FrameCondition::K4,
            "D4" => FrameCondition::D4,
            "K5" => FrameCondition::K5,
            "D5" => FrameCondition::D5,
            "K45" => FrameCondition::K45,
            "D45" => FrameCondition::D45,
            "KB5" => FrameCondition::KB5,
            "S5" => FrameCondition::S5,
            _ => framecond,
        };
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
                        let f = Rc::new(f);
                        // println!("{}", f);
                        // let f = Rc::<Formula>::from(Depth1F::from(f));
                        println!("{}", f);
                        println!();
                        // let tab = S4_CALCULUS.sat(vec![f]);
                        let tab = GradedKCalc::sat(vec![f], framecond);
                        let tab = DisplayTableau(tab);
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
}
