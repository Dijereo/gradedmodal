FROM rust:1.88 AS builder

RUN cargo new --bin golem
WORKDIR /golem
COPY Cargo.toml Cargo.lock ./
RUN apt-get update \
    && apt-get install -y libclang-dev libgfortran5 \
    && rm -rf /var/lib/apt/lists/* \
    && export LIBCLANG_PATH=$(dirname $(find /usr/lib -name libclang.so | head -n1))
RUN cargo build --release || true

COPY src ./src
# RUN mkdir ./dist
# COPY dist/index.html ./dist
# COPY dist/assets ./dist/assets

RUN cargo build --release

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /golem
COPY --from=builder /golem/target/release/golem .
# COPY --from=builder /golem/dist ./dist

EXPOSE 3000
CMD ["./golem"]
