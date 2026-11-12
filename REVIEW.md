# Surface-level review of the rocq-to-erlang plugin OCaml code

Scope: typos, code style, and misleading comments in `src/*.ml{,g}`.
Not a semantic audit.

## `src/ceprint.ml`

1. **Line 99 — extra spaces in map closer (likely typo):**
   ```ocaml
   join ctx "~{" ", "  "}~" emit assocs
   ```
   The third argument is `"  "}~"` with two leading spaces, so maps print as
   `~{'a' => 'b'  }~`. Compare `join ctx "{" ", " "}"` on line 92.

2. **`cerlang.ml` line 32 — wrong comment for `EBinary`:** the AST
   comment documents the binary as `#<...># binary of raw bytes`, but the
   printer is correct: it emits one `#<n>(8,1,'integer',['unsigned'|['big']])`
   segment per byte, wrapped in `#{` … `}#`. The comment should describe
   the per-segment form (or the raw-bytes intent without the wrong
   syntax); `emit_seg` also pushes `#<`/`>` per segment, so the comment's
   `#<...>#` spelling doubly doesn't match reality.

3. **Lines 45–54, 130, 136, 139, 147, 182 — `List.map` used for side
   effects**, with results discarded via `let _ =`. Idiomatic OCaml is
   `List.iter`; the `let _ = List.map ...` pattern is noisy and hides
   intent (e.g. line 139 `let _ = List.map emit_binding bindings`).

4. ~~Newlines pushed before every sub-expression~~ — intentional per
   author; not a finding.

5. **Line 175 vs 86–89 — inconsistent spacing:** patterns print `[h|t]`
   while expressions print `[h | t]`. Cosmetic, but consistency helps when
   reading generated output.

6. **Line 17 comment — slight overclaim:** "Only the quote character
   needs escaping" — quoted atoms can also contain newlines/control
   characters that erlc may reject; "only the quote character is escaped"
   would be accurate.

## `src/cenative.ml`

7. **Line 366 — suspicious mapping with terse comment:**
   ```ocaml
   | ("opp" | "pred"), true -> Some (call "erlang" "-" 2) (* 0 - X / X - 1 *)
   ```
   `opp`/`pred` take **one** ML argument, but the target is declared arity
   **2** with no extra argument supplied. Via `apply_callee`, a saturated
   `opp x` (n=1 < k=2) goes down the eta-expansion path and becomes
   `fun Y -> x - Y` — not `0 - x`. Either the arity/`ml_drop` bookkeeping
   needs a synthetic second argument, or the comment should explain how
   this is correct; as written it looks like a bug and the one-line
   comment doesn't resolve it.

8. **Lines 314–317 vs 355 — comment/code mismatch:** the comment mentions
   canonical `Corelib.BinNums.IntDef.Z` and wrapper
   `Stdlib.ZArith.BinInt.Z`; the code matches **three** paths including
   `Stdlib.ZArith.BinIntDef.Z`, which the comment omits.

9. **Lines 55–60 vs 60 — inconsistency in Stdlib aliases:**
   `list`/`prod`/`nat` accept `"Corelib..." | "Stdlib..." | "Coq..."`, but
   `Z`/`positive` accept only `"Corelib.Numbers.BinNums"` (no
   `Coq.Numbers.BinNums` / Stdlib variant). If aliases matter for the
   datatypes, they presumably matter here too — or a comment should say
   why not.

10. **Line 22 (also `cerlang.ml` line 6, `ceprint.ml` line 7,
    `coreerlang.ml` line 11):** the header box's last dash line is
    `(*------...------ *)` with a stray space before `*)`, unlike the
    closed boxes elsewhere.

## `src/cetrans.ml`

11. **Lines 370–375 — misplaced/misleading comment on
    `trans_constructor`:**
    ```ocaml
    (* A global in value position (not applied):
         - one of our functions -> the fun reference ['f'/n]
         - otherwise             -> error. ...
    *)
    and trans_constructor ctx r args =
    ```
    This is a copy of the comment for `trans_global` (line 263).
    `trans_constructor` neither handles "value position" nor errors this
    way — it builds tagged tuples / delegates to `Cenative`. It actively
    misleads.

12. **Line 317 — stray spaces inside a user-facing message:**
    ```ocaml
    "lambda reduced to a bare function reference by extraction's        eta-contraction, re-expanded"
    ```
    Eight spaces mid-sentence; will show up verbatim in the feedback
    message.

13. **Line 586 — comment names the wrong variable:** "binder list for the
    group: `[ids.(k-1);...;ids.(0)]`" — in `fundefs_of_decl` there is no
    `ids`; it's `rv`. The notation was copied from `MLfix`.

14. **Line 487 — silent fallback:**
    `let sel = try List.nth fresh i with _ -> List.hd fresh` — an
    out-of-range fixpoint index is a compiler invariant violation;
    `assert false` (as used elsewhere) would be more honest than silently
    selecting the first member.

15. **Line 429 — style:** `ids = [] || List.length ids = 1` reads better
    as `List.length ids <= 1`.

16. **Lines 306, 425 — quadratic list building:** `params @ [ v ]` /
    `names @ [ w ]` inside `fold_left`. `List.rev_append`-based
    accumulation (as `trans_fix` already does with `v :: params` +
    `List.rev`) would be consistent and linear.

17. **Lines 158–163 — silent truncation:** `trans_args` returns `[]` when
    `drop` exceeds the argument list; combined with the `apply_callee`
    arity arithmetic this can only happen on a bug, so an `assert` would
    match the file's "fail loudly" philosophy.

## `src/coreerlang.ml`

18. **Lines 83–91 — duplicated, partially misleading comments:** "the
    file is named after the module so that erlc accepts it" is immediately
    followed by a second "the file is named after the module, with the
    atom escaped…". The first is redundant, and the second overclaims:
    `escape_atom` only escapes `'`, it does not make the name
    "filesystem-safe" (though `basename` mostly does). Merge into one
    accurate comment.

## `src/coreerlang_vernac.mlg`

19. **Line 29 — `CLASSIFIED AS QUERY STATE opaque_access`:** the second
    form (`coreerlang_extraction_file`) writes a file to disk, which is a
    side effect — QUERY classification looks wrong for it. If the
    classification must be shared, consider splitting the two forms or
    documenting why.

20. **Lines 26–27 — empty `{ }` block:** harmless, but noise unless a
    placeholder is intended.

## General code-style observations

- **Nested `let _ = e in ...` sequencing.** The codebase (especially
  `ceprint.ml`) writes effectful statements as `let _ = push "fun" in ...`
  instead of plain `push "fun"; ...`. This is noisy and slightly
  misleading (it looks like a binding when it is just sequencing), and it
  is also the root of the `List.map`-for-side-effects idiom (finding #3).
- **Silent fallbacks where the philosophy says "fail loudly".** The files
  declare an assert/user_err-on-the-impossible policy and mostly follow
  it, but a few spots quietly paper over invariant violations:
  `try List.nth fresh i with _ -> List.hd fresh` in `trans_fix`
  (finding #14) and `trans_args` returning `[]` when `drop` exceeds the
  argument list (finding #17). These should be asserts to match the
  codebase's own stated principles.

## Summary of the genuinely worth-fixing items

- #1 (map closer typo) affects printed output; #2 is a wrong AST comment
  (the printed binary syntax itself is correct).
- #7 (`opp`/`pred` arity) looks like a real bug or at minimum needs a
  justifying comment.
- #11 is the most misleading comment in the codebase; #12 is a typo users
  will see.
- #3/#16 are the main style nits.
