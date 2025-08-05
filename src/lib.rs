use std::{
    io::{self, Write},
    rc::Rc,
    time::Instant,
};

use axum::{
    Json, Router,
    http::{Method, StatusCode, header},
    response::{IntoResponse, Response},
    routing::post,
};
use serde::{Deserialize, Serialize};
use tower_http::cors::{self, CorsLayer};

use crate::{
    formula::full_parser,
    frame::{FrameCondition, Frames, Frames4, Frames5, FramesB5, FramesKOr45, FramesT},
    model::{Graph, mock_graph},
    tableau2::DisplayTableau,
    token::tokenize,
    transit::{Transit4, Transit5, TransitB5, TransitKOr45, TransitT},
};

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
        .route("/api", post(world_hello))
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

async fn hello_world() -> &'static str {
    "Hello world!"
}

#[derive(Deserialize)]
pub struct UserSubmission {
    formula: String,
    frames: String,
}

#[derive(Serialize)]
pub struct ServerResponse {
    compute_time: String,
    graph_data: Graph,
    extra: String,
}

impl IntoResponse for ServerResponse {
    fn into_response(self) -> Response {
        (StatusCode::OK, Json(self)).into_response()
    }
}

async fn world_hello(Json(json): Json<UserSubmission>) -> ServerResponse {
    let start = Instant::now();
    println!("{} {}", json.formula, json.frames);
    let (graph_data, extra) = solve(&json.formula, &json.frames);
    let compute_time = format!("{:.9?}", start.elapsed());
    println!("Elapsed: {}", compute_time);
    ServerResponse {
        compute_time,
        graph_data,
        extra,
    }
}

fn solve(formula: &str, frames: &str) -> (Graph, String) {
    let framecond = match frames {
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
        _ => FrameCondition::K,
    };
    println!("Chosen Frame Class: {:?}", framecond);

    match tokenize(formula.trim()) {
        Ok(tokens) => {
            let stream = tokens.into_iter().enumerate();
            match full_parser(stream) {
                Ok(f) => {
                    let f = Rc::new(f);
                    let graph = match framecond {
                        FrameCondition::K => {
                            let tab = DisplayTableau(FramesKOr45::<false, false>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::D => {
                            let tab = DisplayTableau(FramesKOr45::<true, false>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::T => {
                            let tab = DisplayTableau(FramesT.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::K4 => {
                            let tab = DisplayTableau(Frames4::<false>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::D4 => {
                            let tab = DisplayTableau(Frames4::<true>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::K5 => {
                            let tab = DisplayTableau(Frames5::<false>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::D5 => {
                            let tab = DisplayTableau(Frames5::<true>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::K45 => {
                            let tab = DisplayTableau(FramesKOr45::<false, true>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::D45 => {
                            let tab = DisplayTableau(FramesKOr45::<true, true>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::KB5 => {
                            let tab = DisplayTableau(FramesB5::<false>.sat(vec![f]));
                            tab.model_graph()
                        }
                        FrameCondition::S5 => {
                            let tab = DisplayTableau(FramesB5::<true>.sat(vec![f]));
                            tab.model_graph()
                        }
                    };
                    match graph {
                        Ok(graph) => graph,
                        Err(_) => {
                            eprintln!("IOError");
                            mock_graph("Error".to_string())
                        }
                    }
                }
                Err(Some((i, tok))) => {
                    eprintln!("Error: bad token sequence '{:#?}' at index {}", tok, i);
                    mock_graph("Error".to_string())
                }
                Err(None) => {
                    eprintln!("Error: unterminated token sequence");
                    mock_graph("Error".to_string())
                }
            }
        }
        Err((idx, ch)) => {
            eprintln!(
                "Error: bad character sequence '{}' at byte index {}",
                ch, idx
            );
            mock_graph("Error".to_string())
        }
    }
}

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
                        // TODO: remove Frames trait
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
                        }
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
