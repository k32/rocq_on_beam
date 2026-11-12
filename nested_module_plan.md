# Nested module extraction plan

Map Rocq modules exactly onto Erlang modules: a Rocq module `M`
nested in file `foo.v` (logical path `foo.M`) becomes the Erlang
module `'foo.M'` — beam `foo.M.beam`, remote calls
`call 'foo.M':'f'(...)`. Module nesting in the generated Core Erlang
is flat (Erlang has no nested modules); the *names* carry the
hierarchy, dotted.

## Status quo

- MiniML keeps module nesting: submodules appear as
  `SEmodule { ml_mod_expr = MEstruct (mp, elems) }`, recursively
  (`mono_environment` with `~modular:false`; `optimize_struct` does
  not flatten).
- Module types are signature-only in MiniML (`ml_mod_type`,
  `Sval/Sind/Stype/Smodule/Smodtype` specs): no runtime content.
  Skipping them loses nothing.
- `cetrans.ml:translate_structure` currently drops `SEmodule` /
  `SEmodtype` on the floor (`match elem with SEdecl d -> ... `).
- Cross-module calls are half-supported by accident: an unextracted
  global already prints as `call 'foo.M':'f'` (dotted `ModPath`).

## Example mappings (v -> core)

### 1. Nested module, extracted wholesale

`test/nested_mod.v` (logical path `nested_mod`, compiled with
`-Q test ""`):

```coq
Module Inner.
  Definition twice (x : nat) : nat := x + x.
End Inner.

Module Outer.
  Definition four : nat := Inner.twice 2.
  Module Deep.
    Definition six : nat := Inner.twice 3.
  End Deep.
End Outer.

CoreErlang Separate Extraction Outer.four Outer.Deep.six.
```

writes three Core Erlang files:

`nested_mod.core`

```erlang
module 'nested_mod.Inner' ['twice'/1] attributes []
'twice'/1 = fun(_1) -> call 'erlang':'+'(_1,_1)
end
```

`nested_mod.Outer.core`

```erlang
module 'nested_mod.Outer' ['four'/0] attributes []
'four'/0 = fun() -> call 'nested_mod.Inner':'twice'(2)
end
```

`nested_mod.Outer.Deep.core`

```erlang
module 'nested_mod.Outer.Deep' ['six'/0] attributes []
'six'/0 = fun() -> call 'nested_mod.Inner':'twice'(3)
end
```

(`nested_mod` itself holds no extracted decl, so no `nested_mod.core`
— empty modules are not emitted.)

### 2. Module alias (`Module M := N`) — one beam, no copy

```coq
Module Base.
  Definition a : nat := 1.
End Base.
Module Alias := Base.
Definition use : nat := Alias.a + Base.a.
```

`MEident` alias submodules emit nothing; both refs resolve to
`'nested_alias.Base':'a'`:

```erlang
module 'nested_alias' ['use'/0] attributes []
'use'/0 = fun() -> call 'erlang':'+'
              (call 'nested_alias.Base':'a'(),
               call 'nested_alias.Base':'a'())
end
```

### 3. Functor — loud error

```coq
Module Type T. Parameter t : nat. End T.
Module F (X : T). Definition d : nat := X.t. End F.
Module G := F (struct Definition t := 5 end).
```

`optimize_struct` beta-reduces the concrete application, so `G.d`
extracts normally (as `5`). A *residual* functor (e.g. extracting
`F.d` directly, or a functor parameter kept abstract) has no Erlang
counterpart and fails:

```
CoreErlang extraction: functor F is not extractable to Core Erlang
(module-level functions do not exist)
```

### 4. Extraction-library idiom (dedicated extraction module)

`test/bar_extract.v`:

```coq
Require Import foo.          (* the extractee *)
CoreErlang Extraction Library foo.
```

emits `foo.core` (+ `foo.Inner.core`, ...) with every extractable
declaration of `foo` — the ML-backend `Extraction Library` analogue.
The extraction module itself never appears in the output.

## Vernacular commands

- `CoreErlang Separate Extraction ref...` — each ref (and every
  pulled-in dependency) lands in the Erlang module of its own Rocq
  module; `export_spec` (`noexport(...)`) reused for exports.
  Class `SIDEFF STATE opaque_access`.
- `CoreErlang Extraction Library mod.` — whole-library extraction as
  above. Same classification.
- The existing monolithic `CoreErlang Extraction "file" ref...`
  stays unchanged for the single-module use case.

## Steps

| # | Piece | Estimate |
|---|---|---|
| 0 | Split structure/module handling out of `cetrans.ml` into new `cemodule.ml` (`fundefs_of_decl`, `translate_structure`, fun-map bookkeeping move; `cetrans` keeps terms/patterns/fixpoints). Pure refactor, all tests green | 0.5 d |
| 1 | `cetrans`: term-level remote refs — same-module refs stay local `EFunRef`, cross-module refs become remote calls; unify with the existing unknown-global path | 1 d |
| 2 | `cemodule`: recurse `SEmodule`/`MEstruct`, per-module + global fun-maps (`KerName -> module atom * name atom * arity`), `MEident` alias skip, `SEmodtype`/specs skip, residual functor error | 0.5 d |
| 3 | Vernac: `CoreErlang Separate Extraction` + `CoreErlang Extraction Library` in `coreerlang_vernac.mlg`, entry points in `coreerlang.ml` | 1 d |
| 4 | `coreerlang.ml`: multi-module output keyed by ModPath (`foo.core`, `foo.M.core`, ...); skip empty roots | 0.5 d |
| 5 | Spike dotted beams through erlc `from_core`/eunit/anvl (expect zero pipeline change); tests for examples 1-4; README | 0.5 d |

Base total: ~4 days. Contingency: static functor expansion (instead of
erroring), +1-2 d.

## Risks / open questions

- Residual `MEfunctor`/`MEapply` after `optimize_struct` — error in
  v1 (example 3); static expansion is the contingency.
- Dotted module names through the toolchain: legal (Elixir-style
  modules), but erlc `from_core` + eunit + anvl beam globbing get a
  dedicated spike before committing to the naming.
- `ModPath.to_string` of a file-qualified nested module under the
  test `-Q test ""` gives `foo.M` directly; under other mappings the
  prefix follows the logical name — document that the Erlang module
  name is the Rocq *logical* path, not the filename.
