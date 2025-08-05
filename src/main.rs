use std::io;

use axum::{Json, Router, routing::get};
use serde::Deserialize;
use tower_http::services::{ServeDir, ServeFile};

#[tokio::main]
async fn main() -> Result<(), io::Error> {
    let router = init_router();
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await?;
    axum::serve(listener, router).await?;
    Ok(())
}

fn init_router() -> Router {
    Router::new()
        // .route("/", get(hello_world))
        // .route("/api", get(world_hello))
        .route_service("/", ServeFile::new("dist/index.html"))
        .route_service(
            "/assets/index-BrQhOZ60.js",
            ServeFile::new("dist/assets/index-BrQhOZ60.js"),
        )
        .route_service(
            "/assets/index-C5BzwU7B.js",
            ServeFile::new("dist/assets/index-C5BzwU7B.js"),
        )
        .route_service(
            "/assets/index-D8b4DHJx.css",
            ServeFile::new("dist/assets/index-D8b4DHJx.css"),
        )
        .route_service(
            "/assets/react-CHdo91hT.svg",
            ServeFile::new("dist/assets/react-CHdo91hT.svg"),
        )
        .route_service("/vite.svg", ServeFile::new("dist/vite.svg"))
}

async fn hello_world() -> &'static str {
    "Hello world!"
}

#[derive(Deserialize)]
pub struct Submission {
    formula: String,
    frames: String,
}

async fn world_hello(Json(json): Json<Submission>) -> Result<(), ()> {
    println!("{} {}", json.formula, json.frames);
    Ok(())
}
