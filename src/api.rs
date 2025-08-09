use std::{rc::Rc, time::Instant};

use axum::{
    Json,
    http::StatusCode,
    response::{IntoResponse, Response},
};
use serde::{Deserialize, Serialize};

use crate::{formula::full_parser, frame::FrameCondition, model::Graph, token::tokenize};

#[derive(Deserialize)]
pub(crate) struct UserSubmission {
    formula: String,
    frames: String,
}

#[derive(Serialize)]
pub(crate) struct ServerTimes {
    pub(crate) server_time: String,
    pub(crate) parse_time: String,
    pub(crate) solve_time: String,
    pub(crate) tabwrite_time: String,
    pub(crate) graph_time: String,
}

pub(crate) enum ServerResponse {
    Ok(ServerOutput),
    FrameErr(String),
    ParseErr(String),
    ServerErr,
}

#[derive(Serialize)]
pub(crate) struct ServerOutput {
    pub(crate) formula: String,
    pub(crate) times: ServerTimes,
    pub(crate) graph: Graph,
    pub(crate) tableau: String,
    pub(crate) symmetric: bool,
    pub(crate) success: bool,
}

impl IntoResponse for ServerResponse {
    fn into_response(self) -> Response {
        match self {
            ServerResponse::Ok(output) => (StatusCode::OK, Json(output)).into_response(),
            ServerResponse::FrameErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerResponse::ParseErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerResponse::ServerErr => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
        }
    }
}

pub(crate) async fn satisfy(Json(json): Json<UserSubmission>) -> ServerResponse {
    let start = Instant::now();
    println!("{} {}", json.formula, json.frames);
    let mut response = solve(&json.formula, &json.frames);
    if let ServerResponse::Ok(output) = &mut response {
        output.times.server_time = format!("{:.3?}", start.elapsed());
    }
    response
}

fn solve(formula: &str, frames: &str) -> ServerResponse {
    let parse_start = Instant::now();
    let framecond: FrameCondition = {
        match frames.parse() {
            Ok(framecond) => framecond,
            Err(err) => {
                return ServerResponse::FrameErr(format!(
                    "Error: Bad frame class selection: {err}."
                ));
            }
        }
    };
    println!("Chosen Frame Class: {:?}", framecond);
    let tokens = match tokenize(formula.trim()) {
        Ok(tokens) => tokens,
        Err((i, c)) => {
            return ServerResponse::ParseErr(format!(
                "Error: bad character '{c}' at byte index {i}."
            ));
        }
    };
    let stream = tokens.into_iter().enumerate();
    let formula = match full_parser(stream) {
        Ok(f) => Rc::new(f),
        Err(Some((i, tok))) => {
            return ServerResponse::ParseErr(format!(
                "Error: bad token sequence '{:#?}' at index {}.",
                tok, i
            ));
        }
        Err(None) => {
            return ServerResponse::ParseErr(format!("Error: unterminated token sequence."));
        }
    };
    let parse_time = format!("{:.3?}", parse_start.elapsed());
    let resp = framecond.graph_tab_sat(vec![formula], parse_time);
    resp
}
