# coreerlang — minimal Rocq extraction to Core Erlang

A minimal-viable Rocq plugin that extracts Rocq definitions to
[Core Erlang](https://erlang.org/doc/apps/compiler/core_erlang.html),
the intermediate language of the Erlang compiler. It reuses Rocq's own
extraction machinery (MiniML) and only adds a Core Erlang printer, so
everything the standard `Extraction` command supports (logical erasure,
inlining, opacity handling, …) carries over.

## Usage

```coq
Declare ML Module "rocq-core-erlang".

(* print to stdout *)
CoreErlang Extraction add.

(* write a monolithic .core file (dependencies included) *)
CoreErlang Extraction "adder" add.

(* separate extraction: no output-file argument; the module (and
   file) name comes from the Rocq module of the references *)
CoreErlang Separate Extraction add.
```

By default every listed reference is exported (its dependencies are
pulled in but stay internal). A reference marked
`noexport(ref)` is extracted and callable from within the module, but
kept out of the module's exports.

The generated file can be compiled with `erlc` and runs on BEAM:

```sh
erlc -o test_core test_core/adder.core
erl -pa test_core -noshell -eval 'io:format("~p~n",[adder:add({\'S\',{\'S\',\'O\'}},\'O\')])' -s init stop
```

## Representation

The output file is named after the module, which is what `erlc`
expects.

| Rocq             | Core Erlang                                        |
|------------------|----------------------------------------------------|
| function         | `'name'/n = fun (X,Y) -> ...`                 |
| inductive        | constructors become tagged tuples or atoms         |
| `nat`            | native integers: `0`, `N`; `S x` → `x+1`; matches use `<0>` / `when N > 0` + predecessor `let`; literal `S`-chains fold |
| `Z` (ZArith)     | native integers: literals fold (exact, arbitrary precision); `Z.add/sub/mul/max/min/opp/pred/succ/abs/eqb/leb/ltb/of_nat/to_nat` map to Erlang BIFs (`opp`/`pred`/`succ` are unary in Rocq but binary BIFs, so the missing operand is supplied synthetically: `0 - X`, `X - 1`, `X + 1`); user matches on `Z0/Zpos/Zneg` rewrite to guarded clauses |
| prim. `float`    | native float literals                              |
| `list`           | native lists: `[]` / `[H\|T]`                      |
| `prod`           | native untagged tuple `{X, Y}`                     |
| prim. `string`   | native binary (one byte segment per character)     |
| `Erlang.BIF.HMap`| native maps (`maps:from_list/put/find/remove/keys/fold`); the EqDec dictionary argument is dropped |
| `match`          | `case E of <'Tag',X> when 'true' -> ... end`       |
| record           | one-constructor inductive: `{'mkPoint', X, Y}`, projections are matches; single-function-field records eta-collapse to the function |
| global call      | `apply 'name'/n (A, B)` (local, recursive ok) |
| local `fix`      | `letrec 'f'/n = fun ... in ...`               |
| partial application | eta-expansion (Core Erlang is uncurried)        |

(Quoted atoms keep Rocq's original casing, e.g. `'S'`, `'Cons'`; every
match carries a `match_fail` fallback raising `{'badmatch', ...}`.)

Type parameters are erased: a definition `f (A B : Type) (p : A * B)`
extracts to a one-argument untyped function `'f'/1 = fun (_p) -> ...`
operating on the tagged tuple `{'pair', X, Y}` — see `test/poly.v`.

Standard-library dependencies (e.g. `Nat.add`) are extracted into the
same monolithic module, as with the usual `Extraction "file" ...`.

### Classes and instances

Typeclasses need no special handling: Rocq elaborates them away before
extraction, so they arrive as plain records. In practical terms:

- A `Class` is a one-constructor record. It extracts to a tagged
  tuple, e.g. `{'Build_Show', Show_fn, Shown_fn}` — field order is the
  declaration order, minus erased `Prop` fields (see below).
- An `Instance` is a nullary constant of record type, extracted as a
  function `'show_Z'/0` returning the tuple.
- A **method call resolves at elaboration time**: `show z` in Rocq
  becomes "build the instance's tuple, project the field, apply". You
  never get runtime typeclass search in the generated code.
- A **generic function** `Definition showTwice {A} (s : Show A) (x : A)`
  keeps the dictionary as its first argument, exactly like in the
  OCaml backend. From Erlang, pass the dictionary by hand — either the
  extracted instance or a literal tuple:

```erlang
%% dictionary from the extracted instance
0 = typeclass:describe(-42),
%% ...or hand-built: field order follows the class declaration
Dict = {'Build_Show', fun(_) -> 9 end, fun(N) -> N + 1 end},
18 = typeclass:showTwice(Dict, 5).
```

- **Superclasses** (`DS_show :> Show A`) are nested record fields and
  work through the ordinary tuple encoding; projection is a match.
- **`Prop` fields are erased**: a class with proof-valued members
  produces a *shorter* tuple than its field list suggests, since Rocq
  extraction keeps only the computationally relevant fields. Check the
  generated `.core` (or rely on the constructor name prefix
  `'Build_...`') when constructing dictionaries from Erlang.
- There is no runtime instance resolution on the Erlang side: pick (or
  build) the dictionary explicitly. If you want the Erlang caller to
  avoid dictionaries entirely, export instance-specialized wrappers
  from Rocq (e.g. `Definition describe z := show z`), which embed the
  choice of instance at extraction time.

One performance note: each method call rebuilds the instance tuple and
projects from it (that is Rocq's extracted term). Correct, but if a
hot path bothers you, wrap rather than optimize the extractor.

A full worked example lives in `test/typeclass.v` and
`test/src/typeclass_tests.erl`.

A small support library ships with the plugin under `theories/`
(logical name `Erlang`): `Runtime` (markers), `BIF` (the `HMap`
interface), `Types` (`MapFindResult`), `EqDecNat` (an EqDec instance
for `nat`), `GenServer` (a gen_server module type: `init`/
`handle_cast`/`handle_call`/`handle_info` with tagged result
values). `From Erlang Require Import BIF.` brings the native map
operations in. The Erlang side of the runtime lives in `runtime/`
(the `erlang_runtime` app): `coq_gen_server` adapts extracted
gen_server callbacks to the OTP behaviour, translating the result
constructors (`'i_ok'` → `{ok, State}`, `'c_reply'` → `{reply,
Reply, State}`, …) — see `test/gen_server.v` and
`test/src/gen_server_suite.erl` for a live example that starts the
extracted Counter via the shim.

## Building & testing

The OCaml plugin and the Rocq theories are built with dune; the test
pipeline (Rocq extraction → Core Erlang compilation → eunit) is driven
by ANVL:

```sh
dune build     # plugin + theories (also stages the plugin into test/plugin)
anvl all       # extract, compile, run the eunit suites
```

Run `anvl` from the repository root. The single ANVL project declares
two OTP apps: `extracted` (`test/extracted/src/*.core`, generated by a
pre-compile hook running `rocq c` — one parallel task per `test/*.v`;
`from_core` is added automatically for `.core` sources) and
`rocq_core_erlang_tests` (`test/src/*.erl`). The `all` condition
discovers every `*_tests` module in the compiled test app and runs
them in one eunit call (a non-empty glob is asserted, so a broken
discovery cannot silently pass).

A new test is just two files, picked up automatically:

- `test/foo.v` containing `CoreErlang Extraction "foo" ...`
- `test/src/foo_tests.erl`, an eunit module calling `foo:...`

Manual usage (equivalent steps):

```sh
dune build

# test/plugin now holds META + promoted plugin artifacts; rocq writes
# the .core file to its cwd
(cd test/extracted/src &&
 rocq c -q -I ../../../test/plugin -Q ../../../_build/default/theories Erlang ../../../test/rich.v)

erlc -o test_core test/extracted/src/*.core test/src/*_tests.erl
erl -pa test_core -noshell -eval 'eunit:test([rich_tests], [verbose])' -s init stop
```

## Layout

- `src/cerlang.ml` — the Core Erlang AST (`expr`, `pattern`, `fundef`, `module_`)
- `src/cenative.ml` — native type lowering: constructor registry (list/pair/nat/Z/positive), literal folding, Z op → BIF table, constructor/pattern/match-branch rewrites
- `src/cetrans.ml` — term translation: MiniML → Core Erlang AST (de Bruijn resolution, eta-expansion, fixpoints); queries `Cenative` for all native decisions
- `src/cemodule.ml` — structure/module layer: walks the MiniML structure, fun maps, module assembly; drives `Cetrans`
- `src/ceprint.ml` — phase 2: Core Erlang AST → concrete syntax (pure pretty-printing, including integer/float literal rendering)
- `src/coreerlang.ml` — glue: drives Rocq's extraction, then the two phases; vernac entry points
- `src/coreerlang_vernac.mlg` — the `CoreErlang Extraction` vernaculars
- `theories/` — Rocq support library (logical name `Erlang`)
- `anvl.erl` — the ANVL test pipeline (root project)
- `test/plugin/` — META + dune-promoted plugin artifacts for `Declare ML Module`
- `test/*.v` — Rocq inputs; `test/src/*_tests.erl` — eunit tests over the extracted modules; `test/extracted/` — generated `.core` files (gitignored)

## Limitations (it's minimal!)

- Single monolithic module; no separate/multipart extraction.
- Inductives are untyped tagged tuples; no interop with native Erlang
  types beyond atoms/tuples chosen by Rocq's extraction settings
  (e.g. `Extract Inductive` customizations are not translated).
- `MLmagic` is erased unsafely; axioms/dummies become marker atoms.
- `Z.compare` and `Nat.eq_dec` have no native mapping and surface as
  unextracted-constant errors.
