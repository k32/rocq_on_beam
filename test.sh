#!/bin/sh
# Build the plugin, extract the tests into test_core/, and run the eunit suite.
set -e
cd "$(dirname "$0")"

PKG=${PKG:-/tmp/opencode/pkg/coreerlang}
OUT=${OUT:-test_core}
mkdir -p "$PKG" "$OUT"
cp src/META.coreerlang_plugin "$PKG/META"
for f in cmxs cma cmxa a; do
  ln -sf "$PWD/_build/default/src/coreerlang_plugin.$f" "$PKG/"
done

dune build

# Rocq -> Core Erlang (into $OUT)
rocq c -I "$(dirname "$PKG")" test/test.v
rocq c -I "$(dirname "$PKG")" -R test "" test/rich.v
rocq c -I "$(dirname "$PKG")" -R test "" test/poly.v
rocq c -I "$(dirname "$PKG")" -R test "" test/record.v
rocq c -I "$(dirname "$PKG")" -R test "" test/inductive.v

# Core Erlang -> BEAM (extracted modules + eunit test modules, into $OUT)
erlc -o "$OUT" "$OUT"/*.core
erlc -o "$OUT" test/*_tests.erl

erl -pa "$OUT" -noshell -eval 'eunit:test([adder_tests, rich_tests, poly_tests, record_tests, inductive_tests], [verbose])' -s init stop
