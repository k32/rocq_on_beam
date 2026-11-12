# Classes and instances plan

## Verdict (verified, not assumed)

Typeclasses and instances already extract correctly with **no special
lowering**. In Rocq both are elaborated away before extraction reaches
MiniML:

- a `Class` is a one-constructor record — extracted as the usual
  tagged tuple `{'Build_Show', F1, F2}`;
- an `Instance` is a nullary constant of record type — extracted as
  `'show_Z'/0 = fun() -> {'Build_Show', fun(_1) -> 0, fun(_2) ->
  call 'erlang':'abs'(_2)}`;
- method calls have their dictionary argument **resolved at
  elaboration time**: `show z` becomes "build the `show_Z` record,
  project the field, apply" — no runtime typeclass search, ever;
- superclasses are just nested record fields (`'Build_DoubleShow'`
  holding the `Show` dictionary) and work through the ordinary
  tagged-tuple encoding;
- a generic function `showTwice {A} (s : Show A) x` keeps the
  dictionary as an explicit leading argument, as in the OCaml backend.

This is now locked in by `test/typeclass.v` + `test/src/typeclass_tests.erl`
(3 eunit tests, auto-discovered).

## Verified output (excerpt)

```erlang
'show_Z'/0 = fun() -> {'Build_Show', fun(_1) -> 0,
                                fun(_2) -> call 'erlang':'abs'(_2)}

'describe'/1 = fun(_1) ->
  let _2 = apply 'show_Z'/0() in
  case _2 of <{'Build_Show',_3,_4}> when 'true' -> apply _3(_1) ...
```

## Remaining work: none required, two optional items

### 1. Dictionary-passing overhead (optimization, not correctness)

Every method call rebuilds the dictionary record and immediately
matches it apart:

```
apply _3 (let Dict = build record in match Dict ...)
```

Rocq's extractor keeps the instance as an opaque constant, so the
record is constructed at each call site. Correct, but allocates a
tuple + closure per method call. If it ever matters:

- detect the shape `let d = apply 'inst'/0() in match d with
  <{'Ctor', f1, ..., fn}> ...` (dictionary built only to be
  destructured once) and beta-reduce it to the projected fields —
  a small peephole in `cetrans`, or
- mark instances inline-able at extraction time so `optimize_struct`
  substitutes them (what OCaml extraction achieves via
  `Extraction Inline`).

Estimate: 0.5 d. Defer until profiling shows it matters.

### 2. Propositional fields shrink records (document, don't code)

Rocq extraction erases proof fields (minimal projections), so a class
with `Prop` members produces a *shorter* tuple than its field list
suggests. Nothing to implement — the tagged encoding carries whatever
fields survive — but worth a test with a `Prop` field to pin the
arity. Estimate: 0.25 d (one test).

## Non-goals

- No runtime typeclass resolution: Erlang callers pass dictionaries
  explicitly (or call instance-specialized functions); resolution is a
  Rocq-compile-time affair.
- No `Canonical Structure`/unification magic — same argument: all
  resolved before MiniML.
