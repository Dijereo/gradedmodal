use std::time::Instant;

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
    action: String,
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
    ActionErr(String),
    FrameErr(String),
    ParseErr(String),
    ServerErr,
    NotImplemented(&'static str),
}

#[derive(Serialize)]
pub(crate) struct ServerOutput {
    pub(crate) formula: String,
    pub(crate) times: ServerTimes,
    pub(crate) graph: Graph,
    pub(crate) tableau: String,
    pub(crate) symmetric: bool,
    pub(crate) satisfiable: bool,
}

impl IntoResponse for ServerResponse {
    fn into_response(self) -> Response {
        match self {
            ServerResponse::Ok(output) => (StatusCode::OK, Json(output)).into_response(),
            ServerResponse::FrameErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerResponse::ParseErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerResponse::ActionErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerResponse::ServerErr => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
            ServerResponse::NotImplemented(err) => {
                (StatusCode::NOT_IMPLEMENTED, err).into_response()
            }
        }
    }
}

pub(crate) async fn solve_endpt(Json(json): Json<UserSubmission>) -> ServerResponse {
    println!("{} {}", json.formula, json.frames);
    let validate = match json.action.to_lowercase().as_str() {
        "sat" => false,
        "val" => true,
        _ => return ServerResponse::ActionErr(json.action),
    };
    solve(&json.formula, &json.frames, validate)
}

pub(crate) fn solve(formula: &str, frames: &str, validate: bool) -> ServerResponse {
    let start = Instant::now();
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
        Ok(f) => f,
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
    let parse_time = format!("{:.3?}", start.elapsed());
    let mut resp = framecond.graph_tab(formula, validate, parse_time);
    if let ServerResponse::Ok(output) = &mut resp {
        output.times.server_time = format!("{:.3?}", start.elapsed());
    }
    resp
}
