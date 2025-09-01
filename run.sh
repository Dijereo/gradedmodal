#!/usr/bin/env bash
set -euo pipefail

cd app
npm run build
cd ..

mkdir -p dist/assets
cp app/dist/index.html dist/
cp -r app/dist/assets dist/

RUSTFLAGS="-Awarnings" cargo run --release --bin server
