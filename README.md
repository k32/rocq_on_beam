# coreerlang — minimal Rocq extraction to Core Erlang

A minimal-viable Rocq plugin that extracts Rocq definitions to
[Core Erlang](https://erlang.org/doc/apps/compiler/core_erlang.html),
the intermediate language of the Erlang compiler. It reuses Rocq's own
extraction machinery (MiniML) and only adds a Core Erlang printer, so
everything the standard `Extraction` command supports (logical erasure,
inlining, opacity handling, …) carries over.

## Usage

```coq
Declare ML Module "coreerlang_plugin".

(* print to stdout *)
CoreErlang Extraction add.

(* write a monolithic .core file (dependencies included) *)
CoreErlang Extraction "adder" export(add).
```

References marked as `export(ref)` are listed in the module's exports.
Unmarked references (and all pulled-in dependencies) are still
extracted and callable from within the module, but stay internal —
invisible from other Erlang modules.

The generated file can be compiled with `erlc` and runs on BEAM:

```sh
erlc -o test_core test_core/rocq_adder.core
erl -pa test_core -noshell -eval 'io:format("~p~n",[rocq_adder:rocq_add({\'rocq_S\',{\'rocq_S\',\'rocq_O\'}},\'rocq_O\')])' -s init stop
```

## Representation

Every generated atom carries a `rocq_` prefix (module names, function
names, constructor tags, markers), so extracted code cannot clash with
hand-written Erlang names. The output file is named after the module,
which is what `erlc` expects.

| Rocq             | Core Erlang                                        |
|------------------|----------------------------------------------------|
| function         | `'rocq_name'/n = fun (X,Y) -> ...`                 |
| inductive        | constructors become tagged tuples or atoms         |
| `nat`            | `'rocq_O'` / `{'rocq_S', N}`                       |
| `list`           | `'rocq_nil'` / `{'rocq_cons', H, T}`               |
| `match`          | `case E of <'Tag',X> when 'true' -> ... end`       |
| record           | one-constructor inductive: `{'rocq_mkPoint', X, Y}`, projections are matches; single-function-field records eta-collapse to the function |
| global call      | `apply 'rocq_name'/n (A, B)` (local, recursive ok) |
| local `fix`      | `letrec 'rocq_f'/n = fun ... in ...`               |
| partial application | eta-expansion (Core Erlang is uncurried)        |

(Quoted atoms keep Rocq's original casing after the prefix, e.g.
`'rocq_S'`, `'rocq_Cons'`.)

Type parameters are erased: a definition `f (A B : Type) (p : A * B)`
extracts to a one-argument untyped function `'rocq_f'/1 = fun (_p) -> ...`
operating on the tagged tuple `{'rocq_pair', X, Y}` — see `test/poly.v`.

Standard-library dependencies (e.g. `Nat.add`) are extracted into the
same monolithic module, as with the usual `Extraction "file" ...`.

## Building & testing

```sh
make check   # build plugin + extract + compile + run the eunit suite
make build   # build and stage the plugin only
make clean   # remove test_core/
```

The Makefile rules are abstract: a new test `test/foo.v` (containing
`CoreErlang Extraction "test_core/foo" ...`) plus `test/foo_tests.erl`
is picked up automatically.

Manual usage (equivalent steps):

```sh
dune build

# stage the plugin so `Declare ML Module` finds it
mkdir -p /tmp/pkg/coreerlang_plugin
cp src/META.coreerlang_plugin /tmp/pkg/coreerlang_plugin/META
for f in cmxs cma cmxa a; do
  ln -sf $PWD/_build/default/src/coreerlang_plugin.$f /tmp/pkg/coreerlang_plugin/
done

rocq c -I /tmp/pkg -R test "" test/rich.v   # writes test_core/rocq_rich.core
erlc -o test_core test_core/*.core test/*_tests.erl
erl -pa test_core -noshell -eval 'eunit:test([rich_tests], [verbose])' -s init stop
```

## Layout

- `src/cerlang.ml` — the Core Erlang AST (`expr`, `pattern`, `fundef`, `module_`)
- `src/cetrans.ml` — phase 1: MiniML → Core Erlang AST (name resolution, de Bruijn → variables, eta-expansion, constructor tagging)
- `src/ceprint.ml` — phase 2: Core Erlang AST → concrete syntax (pure pretty-printing)
- `src/coreerlang.ml` — glue: drives Rocq's extraction, then the two phases; vernac entry points
- `src/g_coreerlang.mlg` — the `CoreErlang Extraction` vernacular (via `rocq pp-mlg`)
- `test/*.v` — Rocq inputs; `test/*_tests.erl` — eunit tests over the extracted modules

## Limitations (it's minimal!)

- Single monolithic module; no separate/multipart extraction.
- Inductives are untyped tagged tuples; no interop with native Erlang
  types beyond atoms/tuples chosen by Rocq's extraction settings
  (e.g. `Extract Inductive` customizations are not translated).
- `MLmagic` is erased unsafely; axioms/dummies become marker atoms.
