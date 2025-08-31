#!/usr/bin/env bash
set -euo pipefail

cd app
npm run build
cd ..

mkdir -p dist/static
cp app/dist/index.html dist/static/
cp -r app/dist/assets dist/static/

RUSTFLAGS="-Awarnings" cargo run --release --bin server
