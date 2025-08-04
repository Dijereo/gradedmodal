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
        .route("/app", get(hello_world))
        .route("/api", get(world_hello))
        .merge(Router::new().route_service(
            "/",
            ServeDir::new("dist").not_found_service(ServeFile::new("dist/index.html")),
        ))
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
