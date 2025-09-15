use std::{mem, path::Path};

use axum::{
    Router,
    http::{Method, header},
    routing::post,
};
use tower_http::{
    cors::{self, CorsLayer},
    services::ServeFile,
};

mod api;
mod b5;
mod cli;
mod eval;
mod flatformula;
mod formula;
mod frame;
mod k4;
mod k45;
mod k5;
mod kb;
mod model;
mod parser;
mod randgen;
mod randthm;
mod rules3;
mod tableau2;
mod timeout;
mod token;
mod transit;
mod translate;
mod tt;
mod util;

pub fn init_router() -> Router {
    let cors = CorsLayer::new()
        .allow_origin(cors::Any)
        .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
        .allow_headers([header::CONTENT_TYPE]);

    let mut router = Some(
        Router::new()
            .route("/api", post(api::solve_endpt))
            .route_service("/", ServeFile::new("dist/index.html")),
    );
    util::run_on_exts(
        &["css", "js", "svg", "html"],
        ["dist/assets"].iter(),
        &mut |path: &Path| {
            router = Some(mem::take(&mut router).unwrap().route_service(
                &format!("/assets/{}", path.file_name().unwrap().to_str().unwrap()),
                ServeFile::new(path),
            ));
        },
    )
    .unwrap();
    router.map(|r| r.layer(cors)).unwrap()
}

pub fn run_cli() {
    cli::run();
}
