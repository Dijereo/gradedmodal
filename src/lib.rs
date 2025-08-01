use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{
    depth1::Depth1F,
    formula::{Formula, full_parser},
    frame::FrameCondition,
    rules::{K_CALCULUS, RuleCalc, S4_CALCULUS, T_CALCULUS},
    rules3::GradedKCalc,
    tableau2::DisplayTableau,
    token::tokenize,
};

mod depth1;
mod dnf;
mod formula;
// mod formula2;
mod frame;
mod ilp;
mod parser;
// mod parser2;
mod rules;
mod rules3;
mod signed;
mod tableau;
mod tableau2;
mod token;
mod util;

pub fn run() {
    loop {
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
                        let tab = DisplayTableau(GradedKCalc::sat(vec![f], FrameCondition::D45));
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
