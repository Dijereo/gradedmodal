use std::io;

#[tokio::main]
async fn main() -> Result<(), io::Error> {
    let router = gradedmodal::init_router();
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await?;
    axum::serve(listener, router).await?;
    Ok(())
}
