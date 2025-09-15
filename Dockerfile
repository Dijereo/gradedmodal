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
RUN cargo build --release --bin server


FROM node:22 AS vite-builder

WORKDIR /app

COPY app/package.json app/package-lock.json ./
RUN npm ci

COPY app/ ./
RUN npm run build


FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /golem
COPY --from=builder /golem/target/release/server .
RUN mkdir ./dist && mkdir ./dist/assets && mkdir ./lib
COPY --from=builder /golem/target/release/build/scip-sys-*/out/scip_install/lib/libscip.so* \
    /usr/lib/x86_64-linux-gnu/libgfortran.so* \
    /usr/lib/x86_64-linux-gnu/libquadmath.so* \
    ./lib
ENV LD_LIBRARY_PATH=/golem/lib
COPY --from=vite-builder /app/dist/index.html ./dist
COPY --from=vite-builder /app/dist/assets/ ./dist/assets/

EXPOSE 3000
CMD ["/golem/server"]
