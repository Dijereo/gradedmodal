use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{
    formula::full_parser,
    frame::{FrameCondition, Frames, Frames4, Frames5, FramesB5, FramesKOr45, FramesT},
    tableau2::DisplayTableau,
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
mod signed;
mod tableau;
mod tableau2;
mod token;
mod transit;
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
            "T" => FrameCondition::T,
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
                        match framecond {
                            FrameCondition::K => println!(
                                "{}",
                                DisplayTableau(FramesKOr45::<false, false>.sat(vec![f]))
                            ),
                            FrameCondition::D => println!(
                                "{}",
                                DisplayTableau(FramesKOr45::<true, false>.sat(vec![f]))
                            ),
                            FrameCondition::T => {
                                println!("{}", DisplayTableau(FramesT.sat(vec![f])))
                            }
                            FrameCondition::K4 => {
                                println!("{}", DisplayTableau(Frames4::<false>.sat(vec![f])))
                            }
                            FrameCondition::D4 => {
                                println!("{}", DisplayTableau(Frames4::<true>.sat(vec![f])))
                            }
                            FrameCondition::K5 => {
                                println!("{}", DisplayTableau(Frames5::<false>.sat(vec![f])))
                            }
                            FrameCondition::D5 => {
                                println!("{}", DisplayTableau(Frames5::<true>.sat(vec![f])))
                            }
                            FrameCondition::K45 => println!(
                                "{}",
                                DisplayTableau(FramesKOr45::<false, true>.sat(vec![f]))
                            ),
                            FrameCondition::D45 => println!(
                                "{}",
                                DisplayTableau(FramesKOr45::<true, true>.sat(vec![f]))
                            ),
                            FrameCondition::KB5 => {
                                println!("{}", DisplayTableau(FramesB5::<false>.sat(vec![f])))
                            }
                            FrameCondition::S5 => {
                                println!("{}", DisplayTableau(FramesB5::<true>.sat(vec![f])))
                            }
                        };
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
