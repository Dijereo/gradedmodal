use axum::{
    Router,
    http::{Method, header},
    routing::post,
};
use std::io::{self, Write};
use tower_http::cors::{self, CorsLayer};

use crate::{formula::full_parser, frame::FrameCondition, token::tokenize};

mod api;
mod b5;
mod cli;
mod dnf;
mod flatformula;
mod formula;
mod frame;
mod ilp;
mod k4;
mod k45;
mod k5;
mod kb;
mod model;
mod parser;
mod rules;
mod rules3;
mod signed;
mod tableau;
mod tableau2;
mod tb;
mod token;
mod translate;
mod transit;
mod tt;
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
    cli::run();
}
