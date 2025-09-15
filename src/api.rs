use std::time::{Duration, Instant};

use axum::{
    Json,
    http::StatusCode,
    response::{IntoResponse, Response},
};
use serde::{Deserialize, Serialize};

use crate::{
    formula::full_parser,
    frame::FrameCondition,
    model::GraphView,
    timeout::{NoopHandler, StopHandler, Timedout, TimeoutHandler},
    token::tokenize,
};

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

pub(crate) type ServerResult = Result<ServerOutput, ServerError>;

pub(crate) enum ServerError {
    ActionErr(String),
    FrameErr(String),
    ParseErr(String),
    ServerErr,
    NotImplemented(&'static str),
    Timedout,
}

#[derive(Serialize)]
pub(crate) struct ServerOutput {
    pub(crate) formula: String,
    pub(crate) times: ServerTimes,
    pub(crate) graph: Option<GraphView>,
    pub(crate) tableau: String,
    pub(crate) success: bool,
}

impl IntoResponse for ServerOutput {
    fn into_response(self) -> Response {
        (StatusCode::OK, Json(self)).into_response()
    }
}

impl IntoResponse for ServerError {
    fn into_response(self) -> Response {
        match self {
            ServerError::FrameErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerError::ParseErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerError::ActionErr(err) => (StatusCode::BAD_REQUEST, err).into_response(),
            ServerError::ServerErr => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
            ServerError::NotImplemented(err) => (StatusCode::NOT_IMPLEMENTED, err).into_response(),
            ServerError::Timedout => (
                StatusCode::OK,
                Json(ServerOutput {
                    formula: String::new(),
                    times: ServerTimes {
                        server_time: String::new(),
                        parse_time: String::new(),
                        solve_time: String::new(),
                        tabwrite_time: String::new(),
                        graph_time: String::new(),
                    },
                    graph: None,
                    tableau: "Timedout or Out of Memory".to_string(),
                    success: false,
                }),
            )
                .into_response(),
        }
    }
}

pub(crate) async fn solve_endpt(Json(json): Json<UserSubmission>) -> ServerResult {
    // println!("{} {}", json.formula, json.frames);
    let validate = match json.action.to_lowercase().as_str() {
        "sat" => false,
        "val" => true,
        _ => return Err(ServerError::ActionErr(json.action)),
    };
    let (toh, handle) = StopHandler::new(Duration::from_secs(5), None);
    solve(&json.formula, &json.frames, validate, toh)
}

pub(crate) fn solve(
    formula: &str,
    frames: &str,
    validate: bool,
    toh: impl TimeoutHandler,
) -> ServerResult {
    let start = Instant::now();
    let framecond: FrameCondition = {
        match frames.parse() {
            Ok(framecond) => framecond,
            Err(err) => {
                return Err(ServerError::FrameErr(format!(
                    "Error: Bad frame class selection: {err}."
                )));
            }
        }
    };
    // println!("Chosen Frame Class: {:?}", framecond);
    let tokens = match tokenize(formula.trim()) {
        Ok(tokens) => tokens,
        Err((i, c)) => {
            return Err(ServerError::ParseErr(format!(
                "Error: bad character '{c}' at byte index {i}."
            )));
        }
    };
    let stream = tokens.into_iter().enumerate();
    let formula = match full_parser(stream) {
        Ok(f) => f,
        Err(Some((i, tok))) => {
            return Err(ServerError::ParseErr(format!(
                "Error: bad token sequence '{:#?}' at index {}.",
                tok, i
            )));
        }
        Err(None) => {
            return Err(ServerError::ParseErr(format!(
                "Error: unterminated token sequence."
            )));
        }
    };
    let parse_time = format!("{:.3?}", start.elapsed());
    toh.timedout()?;
    let mut resp = framecond.graph_tab(formula, validate, parse_time, toh);
    if let Ok(output) = &mut resp {
        output.times.server_time = format!("{:.3?}", start.elapsed());
    }
    resp
}

impl From<Timedout> for ServerError {
    fn from(_value: Timedout) -> Self {
        Self::Timedout
    }
}
