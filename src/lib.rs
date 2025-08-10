use axum::{
    Router,
    http::{Method, header},
    routing::post,
};
use std::{
    io::{self, Write},
    rc::Rc,
};
use tower_http::cors::{self, CorsLayer};

use crate::{formula::full_parser, frame::FrameCondition, token::tokenize};

mod api;
mod dnf;
mod flatformula;
mod formula;
mod frame;
mod ilp;
mod model;
mod parser;
mod rules;
mod rules3;
mod signed;
mod tableau;
mod tableau2;
mod token;
mod transit;
mod util;

pub fn init_router() -> Router {
    let cors = CorsLayer::new()
        .allow_origin(cors::Any)
        .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
        .allow_headers([header::CONTENT_TYPE]);

    Router::new()
        // .route("/", get(hello_world))
        .route("/api", post(api::solve_endpt))
        // .route_service("/", ServeFile::new("dist/index.html"))
        // .route_service(
        //     "/assets/index-BrQhOZ60.js",
        //     ServeFile::new("dist/assets/index-BrQhOZ60.js"),
        // )
        // .route_service(
        //     "/assets/index-C5BzwU7B.js",
        //     ServeFile::new("dist/assets/index-C5BzwU7B.js"),
        // )
        // .route_service(
        //     "/assets/index-D8b4DHJx.css",
        //     ServeFile::new("dist/assets/index-D8b4DHJx.css"),
        // )
        // .route_service(
        //     "/assets/react-CHdo91hT.svg",
        //     ServeFile::new("dist/assets/react-CHdo91hT.svg"),
        // )
        // .route_service("/vite.svg", ServeFile::new("dist/vite.svg"))
        .layer(cors)
}

pub fn run_cli() {
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
