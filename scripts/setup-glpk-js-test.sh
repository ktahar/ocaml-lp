#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$ROOT/test/lp-glpk-js"
MODULE_DIR="$TEST_DIR/node_modules/glpk.js"
TARGET_VERSION="5.0.0"

mkdir -p "$TEST_DIR"
mkdir -p "$TEST_DIR/node_modules"

if
  [ -f "$MODULE_DIR/package.json" ] &&
  [ -f "$MODULE_DIR/dist/glpk.js" ] &&
  [ -f "$MODULE_DIR/dist/glpk.wasm" ] &&
  grep -Eq "\"name\"[[:space:]]*:[[:space:]]*\"glpk\\.js\"" \
    "$MODULE_DIR/package.json" &&
  grep -Eq "\"version\"[[:space:]]*:[[:space:]]*\"$TARGET_VERSION\"" \
    "$MODULE_DIR/package.json"
then
  echo "glpk.js@$TARGET_VERSION is already installed for tests; skipping setup."
  exit 0
fi

TMP_DIR="$(mktemp -d)"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cd "$TMP_DIR"
npm pack "glpk.js@$TARGET_VERSION" >/dev/null
tar -xzf "glpk.js-$TARGET_VERSION.tgz"

rm -rf "$MODULE_DIR"
cp -R "$TMP_DIR/package" "$MODULE_DIR"
