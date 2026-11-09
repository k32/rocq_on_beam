# Plan: native type lowering for the Core Erlang extraction plugin

Status: Phase 1 (1a `list`, 1b `prod`, 1c primitive `string` → binary)
**implemented**; Phases 2–3 pending.

## Goal

Map Rocq datatypes with direct Erlang counterparts onto native Erlang
representations instead of tagged-tuple encodings, so extracted code is
directly interoperable with hand-written Erlang.

**Key framing decision**: we do **not** reuse Rocq's
`Extract Inductive` / `Extract Constant` machinery. Its "custom" slots
hold target-language *source strings*, which we would have to parse
back into an AST. Since Erlang's native set is fixed (no user-definable
types), we hardwire the mapping by kernel name (`KerName`) in the
translator instead. User-issued `Extract Inductive` / `Extract
Constant` commands are ignored — `cetrans` should raise a clear error
when `Table.is_custom` fires, so the mismatch is never silent.

## Target mapping

| Rocq type            | Native representation        | Mechanism                        |
|----------------------|------------------------------|----------------------------------|
| `list`               | `[]` / `[H\|T]`              | constructor/pattern rewrite (pure) |
| `prod`, `tupleN`     | untagged `{A,B}`             | constructor/pattern rewrite (pure) |
| `bool`               | `'true'` / `'false'`         | already free (atoms) — done      |
| `nat`                | integers `0`, `N`            | guarded-match rewrite (the hard one) |
| primitive `Int63`    | integer                      | `MLuint` → `EInt` — already done |
| primitive `Float64`  | float                        | `MLfloat` → `EFloat` — already done |
| primitive `string`   | **binary** `<<"...">>`       | literal rewrite (pure)           |
| primitive `Array`    | Erlang tuple                 | `MLparray` → `ETuple` — already adequate; relabel from "degraded" to "native" in docs |
| `option`             | `{'Some',X}` / `'None'`      | unchanged (idiomatic Erlang anyway) |

## Verified Core Erlang facts (OTP 29, probed with `erlc`)

Binaries:

```
fun () -> #{#<104>(8,1,'integer',['unsigned'|['big']]),
            #<101>(8,1,'integer',['unsigned'|['big']])}#
        %%  evaluates to  <<"he">>
```

- the classic `#B<[...]>>` literal no longer parses; only the
  annotated `#{ seg, ... }#` form is accepted;
- segments must carry **all four** arguments
  `(size, unit, type, flags)` — a short form crashes `erlc`'s parser
  (internal error in `core_parse`), so we always emit the full form;
- string-valued segments typed `'integer'` parse but raise `badarg` at
  runtime — per-byte integer segments are the safe *and* faithful
  encoding (Rocq primitive strings are byte strings).

Guards (for `nat`): `when` accepts calls such as
`call 'erlang':'>'(X, 0)` (to be re-validated before Phase 2, see gate
below). Literal integer patterns (`<0>`) appear parseable; verify.

## Phase 1 — pure representation lowerings

No semantic rewriting; constructor and pattern sides just change shape.

### 1a. `list` → native lists

- AST (`cerlang.ml`): `ECons of expr * expr` (rendered `[H|T]`);
  pattern `PCons of pattern * pattern`.
- Translator (`cetrans.ml`): `trans_constructor` / `trans_pattern`
  consult the registry first; fold consecutive conses into
  `[A,B,C|T]` sugar (printer concern).
- Tests: `rich_tests` / `poly_tests` switch from building
  `{'cons',H,T}` to real Erlang lists — a feature: extracted `len`
  becomes callable on `[1,2]` directly.
- Size: ~60–90 lines.

### 1b. `prod` / `tupleN` → untagged tuples

- Drop the `'pair'` tag: constructor → `ETuple`, pattern → `PTuple`.
- Tests: `poly_tests` data becomes `{X,Y}`.
- Size: ~20–30 lines.

### 1c. primitive `string` → binary

- AST: `EBinary of string` (raw bytes; literal-only for now, since
  `MLstring` carries constants).
- Printer: one
  `#<b>(8,1,'integer',['unsigned'|['big']])` segment per byte.
- Translator: `MLstring s` → `EBinary (Pstring.to_string s)`
  (one-line change replacing the current `EString`).
- Interop bonus: `<<"he">>` is the idiomatic Erlang string
  representation.
- Caveat: string *literals* become native; string *operations* still
  extract as ordinary constants (custom-constant mapping is out of
  scope, consistent with the framing decision).
- Size: ~20–30 lines.

### Registry (shared by 1a/1b and Phase 2)

A set of constructor `KerName`s (`nil`, `cons`, `pair`, `O`, `S`)
resolved once at plugin init by locating the stdlib inductives via
`Nametab` / `Global`. Must tolerate both `Stdlib.*` (Rocq 9.2) and
legacy `Coq.Init.*` paths.

## Phase 2 — `nat` → integer

The structural mismatch: `S p` patterns cannot map to arithmetic
patterns, so matches need **guards + predecessor binding**:

```
case X of
  <_N> when call 'erlang':'>'(_N, 0) ->      (* was: S p *)
      let _P = call 'erlang':'-'(_N, 1) in body[p := _P]
  <_N> when 'true' -> ...                    (* was: O; or literal <0> *)
```

- AST: extend `ECase` branches with an optional guard expression
  (currently hardwired `'true'`); arithmetic via existing `ECall`.
- Translator (`trans_case`): when all branch patterns are `O`/`S` of
  stdlib `nat`, rewrite branches as above; the `match_fail` /
  `badmatch` fallback stays as the safety net.
- `trans_constructor`: `O` → `EInt "0"`, `S x` → `call 'erlang':'+'(x,1)`.
- **Literal folding**: `S (S (... O))` chains from `Compute`-style
  constants fold to a single `EInt` (with a depth cap). ~30 lines.
- Caveat: structural recursion over `nat` pays an arithmetic call per
  step — acceptable.
- Size: ~100–150 lines; riskiest phase.

## Phase 3 — cross-type re-tests and docs

- New eunit tests exercising mixed native data: `sumlist [1,2,3]`,
  `fst {1,x}` (untagged), `add 2 3` returning integer `5`, string
  literals as binaries.
- README representation-table rewrite.
- The "raise on `Table.is_custom`" guard.

## Order of work and gates

1. Re-validate guard / literal-pattern syntax against `erlc` with
   hand-written `.core` probes (gates Phase 2's design).
2. Phase 1a (`list`) → tests green with updated expectations.
3. Phase 1b (`prod`) + 1c (`string` → binary) → tests green.
4. Phase 2 (`nat` + folding).
5. Phase 3 wrap-up.

Each phase lands separately with its tests; the `badmatch` fallback
means any wrong native rewrite degrades to a runtime error, never a
silently wrong answer.

## Open question

Should `nat` lowering be always-on or behind a vernac toggle
(`CoreErlang Native nat.`)? Always-on matches "Erlang has one integer
type", but breaks interop with any external code using the tagged
`{'S',...}` encoding. Recommendation: always-on, keep the toggle as an
escape hatch.
