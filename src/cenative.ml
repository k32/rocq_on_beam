open Names
module Table = Extraction_plugin.Table
(*-----------------------------------------------------------------------*)
(*  Native type lowering.                                                 *)
(*                                                                       *)
(*  Some Rocq datatypes map onto native Erlang terms: lists, pairs,      *)
(*  integers (nat, Z), binaries (primitive strings). This module owns    *)
(*  everything about that mapping:                                        *)
(*    - the registry of the constructors involved (by GlobRef),          *)
(*    - literal folding,                                                  *)
(*    - the wholesale rewrite of Z library operations to BIFs,           *)
(*    - constructor / pattern / match-branch translation for the         *)
(*      lowered types.                                                    *)
(*                                                                       *)
(*  The mapping is fixed (Erlang has no user-definable types), so it is  *)
(*  hardwired here instead of reusing [Extract Inductive]-style          *)
(*  configuration.                                                        *)
(*                                                                       *)
(*  [cetrans] queries this module ([is_*] predicates, [constructor],     *)
(*  [pattern], [rewrite_branch], [zop_of]) and passes control; it never  *)
(*  decides native translations itself.                                   *)
(*-----------------------------------------------------------------------*)

module Miniml = Extraction_plugin.Miniml
open Miniml

(* -------------------------------------------------------------------- *)
(* Constructor classification, by plain pattern matching on the         *)
(* (label, module path); no lists, no searching, no name resolution.     *)
(*                                                                       *)
(* NB: match on (label, module path) strings, not on GlobRefs:          *)
(* constructors of one inductive share a KerName, so [Table.repr_of_r] *)
(* is not constructor-specific (O and S would be indistinguishable).    *)
(* -------------------------------------------------------------------- *)

type ctor =
  (* List *)
  | CNil
  | CCons
  (* 2-tuple *)
  | CPair
  (* nat *)
  | CO
  | CS
  (* Z *)
  | CZ0
  | CZpos
  | CZneg
  (* postitive *)
  | CxH
  | CxO
  | CxI
  (* HMap *)
  | CHMapW
  (* Map find return *)
  | COk
  | CError

let global_to_ctor (r : global) : ctor option =
  match r.glob with
  | GlobRef.ConstructRef ((mind, _), index) -> (
      (* constructors of one inductive share label and kernel name; the
       constructor index (1-based) tells them apart *)
      let ind = { glob = GlobRef.IndRef (mind, 0); inst = InfvInst.empty } in
      let name = Id.to_string (Table.label_of_r ind) in
      let m = ModPath.to_string (Table.modpath_of_r ind) in
      match (name, m, index) with
      | ( "list",
          ("Corelib.Init.Datatypes" | "Stdlib.Datatypes" | "Coq.Datatypes"),
          1 ) ->
          Some CNil
      | ( "list",
          ("Corelib.Init.Datatypes" | "Stdlib.Datatypes" | "Coq.Datatypes"),
          2 ) ->
          Some CCons
      | ( "prod",
          ("Corelib.Init.Datatypes" | "Stdlib.Datatypes" | "Coq.Datatypes"),
          1 ) ->
          Some CPair
      | ( "nat",
          ( "Corelib.Init.Datatypes" | "Stdlib.Init.Datatypes"
          | "Coq.Init.Datatypes" ),
          1 ) ->
          Some CO
      | ( "nat",
          ( "Corelib.Init.Datatypes" | "Stdlib.Init.Datatypes"
          | "Coq.Init.Datatypes" ),
          2 ) ->
          Some CS
      | ( ("Z" | "positive"),
          ( "Corelib.Numbers.BinNums" | "Stdlib.Numbers.BinNums"
          | "Coq.Numbers.BinNums" ),
          _ ) ->
          (* aliases, as the datatypes above *)
          Some
            (* constructor order in BinNums: Z: Z0, Zpos, Zneg;
          positive: xI, xO, xH *)
            (match (name, index) with
            | "Z", 1 -> CZ0
            | "Z", 2 -> CZpos
            | "Z", 3 -> CZneg
            | _, 1 -> CxI
            | _, 2 -> CxO
            | _ -> CxH)
      | "t", "Erlang.BIF.HMap", 1 -> Some CHMapW (* wrap; empty is unused *)
      | "MapFindResult", "Erlang.Types", 1 -> Some COk
      | "MapFindResult", "Erlang.Types", 2 -> Some CError
      | _ -> None)
  | _ -> None

let is_nil r = match global_to_ctor r with Some CNil -> true | _ -> false
let is_cons r = match global_to_ctor r with Some CCons -> true | _ -> false
let is_pair r = match global_to_ctor r with Some CPair -> true | _ -> false
let is_O r = match global_to_ctor r with Some CO -> true | _ -> false
let is_S r = match global_to_ctor r with Some CS -> true | _ -> false
let is_Z0 r = match global_to_ctor r with Some CZ0 -> true | _ -> false
let is_Zpos r = match global_to_ctor r with Some CZpos -> true | _ -> false
let is_Zneg r = match global_to_ctor r with Some CZneg -> true | _ -> false
let is_xH r = match global_to_ctor r with Some CxH -> true | _ -> false
let is_xO r = match global_to_ctor r with Some CxO -> true | _ -> false
let is_xI r = match global_to_ctor r with Some CxI -> true | _ -> false
let is_hmapw r = match global_to_ctor r with Some CHMapW -> true | _ -> false
let is_ok r = match global_to_ctor r with Some COk -> true | _ -> false
let is_error r = match global_to_ctor r with Some CError -> true | _ -> false

(* -------------------------------------------------------------------- *)
(* Helpers shared by the lowering rules                                  *)
(* -------------------------------------------------------------------- *)

let erlang_call f args =
  Cerlang.EApply (Cerlang.CGlobal (Cerlang.Atom "erlang", Cerlang.Atom f), args)

let guard_true = Cerlang.EAtom (Cerlang.Atom "true")

(* -------------------------------------------------------------------- *)
(* Literal folding                                                       *)
(* -------------------------------------------------------------------- *)

(* literal folding cap for nat S-chains: beyond this, unfold into
   arithmetic calls instead of one huge integer *)
let nat_fold_limit = 100000

let rec nat_literal = function
  | MLcons (_, r, []) when is_O r -> Some 0
  | MLcons (_, r, [ t ]) when is_S r -> Option.map (( + ) 1) (nat_literal t)
  | _ -> None

(* value of a literal positive (binary: xH=1, xO p=2p, xI p=2p+1),
   computed exactly with arbitrary-precision integers (zarith) *)
let rec pos_literal = function
  | MLcons (_, r, []) when is_xH r -> Some Z.one
  | MLcons (_, r, [ t ]) when is_xO r ->
      Option.map (fun p -> Z.mul (Z.of_int 2) p) (pos_literal t)
  | MLcons (_, r, [ t ]) when is_xI r ->
      Option.map (fun p -> Z.add (Z.mul (Z.of_int 2) p) Z.one) (pos_literal t)
  | _ -> None

(* signed value of a literal Z *)
let z_literal = function
  | MLcons (_, r, []) when is_Z0 r -> Some Z.zero
  | MLcons (_, r, [ t ]) when is_Zpos r -> pos_literal t
  | MLcons (_, r, [ t ]) when is_Zneg r -> Option.map Z.neg (pos_literal t)
  | _ -> None

(* -------------------------------------------------------------------- *)
(* Constructors and patterns                                             *)
(* -------------------------------------------------------------------- *)

let is_native_constructor (r : global) = global_to_ctor r <> None

(* Constructor translation for a natively lowered constructor
   (total when [is_native_constructor r] holds); [tr] translates
   argument expressions.

     nil   -> []
     cons  -> [H|T]
     pair  -> {A,B}            (untagged tuple)
     O     -> 0                (nat literals fold to one integer)
     S x   -> call 'erlang':'+'(x, 1)
     Z0    -> 0;  Zpos p -> p;  Zneg p -> 0 - p
     xH    -> 1;  xO p -> 2*p;  xI p -> 2*p + 1
     wrap l -> call 'maps':'from_list' (l)   (the Rocq map representation
                                              bridges to the native map)
     ok v  -> {'ok', v};  error -> 'error' *)
let translate_constructor (tr : ml_ast -> Cerlang.expr) (r : global)
    (args : ml_ast list) : Cerlang.expr =
  let open Cerlang in
  let bad name =
    ETuple (EAtom (Atom ("coq_bad_" ^ name)) :: List.map tr args)
  in
  match (global_to_ctor r, args) with
  | Some CNil, [] -> ENil
  | Some CCons, [ h; t ] -> ECons (tr h, tr t)
  | Some CPair, _ -> ETuple (List.map tr args)
  | Some CO, [] -> EInt Z.zero
  | Some CS, [ t ] -> (
      match nat_literal (MLcons (Tunknown, r, [ t ])) with
      | Some n when n <= nat_fold_limit -> EInt (Z.of_int n)
      | _ -> erlang_call "+" [ tr t; EInt Z.one ])
  | Some CZ0, [] -> EInt Z.zero
  | Some CZpos, [ t ] -> (
      (* a positive is lowered to its integer value: Zpos p = p *)
      match z_literal (MLcons (Tunknown, r, [ t ])) with
      | Some n -> EInt n
      | _ -> tr t)
  | Some CZneg, [ t ] -> (
      match z_literal (MLcons (Tunknown, r, [ t ])) with
      | Some n -> EInt n
      | _ -> erlang_call "-" [ EInt Z.zero; tr t ])
  | Some CxH, [] -> EInt Z.one
  | Some CxO, [ t ] -> (
      match pos_literal (MLcons (Tunknown, r, [ t ])) with
      | Some n -> EInt n
      | _ -> erlang_call "*" [ tr t; EInt (Z.of_int 2) ])
  | Some CxI, [ t ] -> (
      match pos_literal (MLcons (Tunknown, r, [ t ])) with
      | Some n -> EInt n
      | _ ->
          erlang_call "+"
            [ erlang_call "*" [ tr t; EInt (Z.of_int 2) ]; EInt Z.one ])
  | Some CHMapW, [ l ] ->
      Cerlang.EApply
        ( Cerlang.CGlobal (Cerlang.Atom "maps", Cerlang.Atom "from_list"),
          [ tr l ] )
  | Some COk, [ v ] -> ETuple [ EAtom (Atom "ok"); tr v ]
  | Some CError, [] -> EAtom (Atom "error")
  | Some c, _ ->
      bad
        (match c with
        | CNil -> "nil"
        | CCons -> "cons"
        | CPair -> "pair"
        | CO -> "O"
        | CS -> "S"
        | CZ0 -> "Z0"
        | CZpos -> "Zpos"
        | CZneg -> "Zneg"
        | CxH -> "xH"
        | CxO -> "xO"
        | CxI -> "xI"
        | CHMapW -> "HMapW"
        | COk -> "ok"
        | CError -> "error")
  | None, _ -> assert false

(* Patterns that can be expressed as patterns (lists, pairs, nullary
   integer literals).  Arithmetic constructor patterns (O/S, Z,
   positive) cannot: shallow occurrences go through [translate_branch]
   instead, and deep ones have no native pattern at all. *)
let is_native_pattern (r : global) =
  match global_to_ctor r with
  | Some (CNil | CCons | CPair | CO | CZ0 | CxH | COk | CError) -> true
  | _ -> false

(* [translate_pattern sub r ps] (total when [is_native_pattern r]);
   [sub] translates the sub-patterns. *)
let translate_pattern (sub : ml_pattern -> Cerlang.pattern) (r : global)
    (ps : ml_pattern list) : Cerlang.pattern =
  let open Cerlang in
  match (global_to_ctor r, ps) with
  | Some CNil, [] -> PNil
  | Some CCons, [ h; t ] -> PCons (sub h, sub t)
  | Some CPair, _ -> PTuple (List.map sub ps)
  | Some (CO | CZ0), [] -> PInt Z.zero
  | Some CxH, [] -> PInt Z.one
  | Some COk, [ p ] -> PTuple [ PAtom (Atom "ok"); sub p ]
  | Some CError, [] -> PAtom (Atom "error")
  | Some _, _ -> PTuple [ PAtom (Atom "coq_bad_cons") ] (* arity mismatch *)
  | None, _ -> assert false

(* Pusual: constructor applied to all its (named) arguments; the same
   shapes as [is_native_pattern] *)
let is_native_usual r = is_native_pattern r

let translate_pattern_usual (r : global) (names : Cerlang.var list) :
    Cerlang.pattern =
  let open Cerlang in
  match (global_to_ctor r, names) with
  | Some CNil, [] -> PNil
  | Some CCons, [ h; t ] -> PCons (PVar h, PVar t)
  | Some CPair, _ -> PTuple (List.map (fun v -> PVar v) names)
  | Some (CO | CZ0), _ -> PInt Z.zero
  | Some CxH, _ -> PInt Z.one
  | Some COk, _ -> PTuple (PAtom (Atom "ok") :: List.map (fun w -> PVar w) names)
  | Some CError, [] -> PAtom (Atom "error")
  | Some _, _ -> PTuple [ PAtom (Atom "coq_bad_cons") ]
  | None, _ -> assert false

(* -------------------------------------------------------------------- *)
(* Match branches                                                        *)
(* -------------------------------------------------------------------- *)

(* Rewrite one match branch for the natively lowered arithmetic
   types, whose constructor patterns cannot stay structural:
       O / Z0  ->  <0> when 'true'
       xH      ->  <1> when 'true'
       S p     ->  <_v> when _v > 0   with  p = _v - 1
       Zpos p  ->  <_v> when _v > 0   with  p = _v
       Zneg p  ->  <_v> when _v < 0   with  p = 0 - _v
       xO p    ->  <_v> when band(_v,1) =:= 0  with  p = bsr(_v,1)
       xI p    ->  <_v> when band(_v,1) =:= 1  with  p = bsr(_v,1)
   [v] is the (already let-bound) scrutinee variable, [name] the
   branch's single pattern variable, if any. Returns [None] when the
   pattern has no native rewrite (wildcards included as trivial). *)
let rewrite_branch (v : Cerlang.var) (pat : ml_pattern)
    (name : Cerlang.var option) :
    (Cerlang.pattern * Cerlang.expr * (Cerlang.var * Cerlang.expr) list) option
    =
  let open Cerlang in
  let no_binder pat' g = Some (pat', g, []) in
  let one_var g mk =
    Some (PVar v, g, match name with Some x -> [ (x, mk x) ] | None -> [])
  in
  let x = EVar v in
  match pat with
  | Pwild | Prel _ -> no_binder PWild guard_true
  | (Pcons (r, []) | Pusual r) when is_O r || is_Z0 r ->
      no_binder (PInt Z.zero) guard_true
  | (Pcons (r, []) | Pusual r) when is_xH r -> no_binder (PInt Z.one) guard_true
  | Pcons (r, [ Pwild ]) | Pcons (r, [ Prel 1 ]) | Pusual r -> (
      match global_to_ctor r with
      | Some CS ->
          one_var
            (erlang_call ">" [ x; EInt Z.zero ])
            (fun _ -> erlang_call "-" [ x; EInt Z.one ])
      | Some CZpos -> one_var (erlang_call ">" [ x; EInt Z.zero ]) (fun _ -> x)
      | Some CZneg ->
          one_var
            (erlang_call "<" [ x; EInt Z.zero ])
            (fun _ -> erlang_call "-" [ EInt Z.zero; x ])
      | Some CxO ->
          one_var
            (erlang_call "=:="
               [ erlang_call "band" [ x; EInt Z.one ]; EInt Z.zero ])
            (fun _ -> erlang_call "bsr" [ x; EInt Z.one ])
      | Some CxI ->
          one_var
            (erlang_call "=:="
               [ erlang_call "band" [ x; EInt Z.one ]; EInt Z.one ])
            (fun _ -> erlang_call "bsr" [ x; EInt Z.one ])
      | _ -> None)
  | _ -> None

(* Query + total entry point for the branch rewrite below *)

let translate_branch (v : Cerlang.var) (pat : ml_pattern)
    (name : Cerlang.var option) :
    Cerlang.pattern * Cerlang.expr * (Cerlang.var * Cerlang.expr) list =
  match rewrite_branch v pat name with Some b -> b | None -> assert false

let is_native_branch (pat : ml_pattern) =
  rewrite_branch (Cerlang.Var 0) pat None <> None

(* -------------------------------------------------------------------- *)
(* Z library functions -> Erlang BIFs.                                   *)
(*                                                                       *)
(* We are not interested in Z's internal representation: the standard   *)
(* operations are mapped wholesale to native Erlang calls, and literals *)
(* fold to integers. If some other Z function recurses structurally on  *)
(* the representation, its (shallow) matches are rewritten with guards; *)
(* anything left fails loudly via badmatch.                              *)
(*                                                                       *)
(* Both the canonical constants (Corelib.BinNums.IntDef.Z) and their
   Stdlib wrappers (Stdlib.ZArith.BinInt.Z) are matched: they are
   distinct constants, and which one a term references depends on
   whether the user imported Corelib or Stdlib. The internal Stdlib
   definitions (Stdlib.ZArith.BinIntDef.Z) are matched as well. *)
(* -------------------------------------------------------------------- *)

(* -------------------------------------------------------------------- *)
(* Remote calls: what a global constant becomes natively.                *)
(*                                                                       *)
(* Purely a data question: [global_target] maps a global to a native    *)
(* call target (module, function, arity, number of leading arguments    *)
(* to drop untranslated -- the EqDec dictionary) or a fixed value.      *)
(* Currying/eta handling is NOT done here; [cetrans] owns the single    *)
(* application engine and renders fully-applied calls via              *)
(* [render_call].                                                        *)
(*                                                                       *)
(* Z.compare and Nat.eq_dec have no entries (dropped for now); usage    *)
(* shows up as an unextracted-constant error in the translation.        *)
(* -------------------------------------------------------------------- *)

type target =
  | NativeCall of Cerlang.atom * Cerlang.atom * int * int
    (* module, function, arity, leading arguments to drop *)
  | NativeCall2 of Cerlang.atom * Cerlang.atom * Cerlang.expr * bool
    (* a unary ML function rendered as a binary BIF call: module,
       function, the synthetic operand, and whether it comes first
       (f(syn, x), e.g. Z.opp -> 0 - X) or last (f(x, syn), e.g.
       Z.succ -> X + 1) *)
  | NativeIdentity (* the identity: the argument itself (e.g. Z.of_nat) *)
  | NativeValue of Cerlang.expr (* a fixed value *)
  | NotNative

(* -------------------------------------------------------------------- *)
(* Per-family global classification, by plain pattern matching on the  *)
(* global's (label, module path); no lists, no searching.               *)
(* -------------------------------------------------------------------- *)

let call m f arity = NativeCall (Cerlang.Atom m, Cerlang.Atom f, arity, 0)
let call_drop m f arity = NativeCall (Cerlang.Atom m, Cerlang.Atom f, arity, 1)

(* Z library functions -> erlang BIFs *)
let global_to_z (r : global) : target option =
  let l = Id.to_string (Table.label_of_r r) in
  let m = ModPath.to_string (Table.modpath_of_r r) in
  let z_mod =
    match m with
    | "Corelib.BinNums.IntDef.Z" | "Stdlib.ZArith.BinIntDef.Z"
    | "Stdlib.ZArith.BinInt.Z" ->
        true
    | _ -> false
  in
  match (l, z_mod) with
  | ("add" | "sub" | "mul"), true ->
      Some
        (call "erlang"
           (match l with "add" -> "+" | "sub" -> "-" | _ -> "*")
           2)
  | ("max" | "min"), true -> Some (call "erlang" l 2)
  | "succ", true ->
      Some
        (NativeCall2
           (Cerlang.Atom "erlang", Cerlang.Atom "+", Cerlang.EInt Z.one, false))
      (* X + 1 *)
  | ("opp" | "pred"), true ->
      (* both are unary ML functions but the BIF '-' is binary: supply the
       missing operand synthetically (opp -> 0 - X, pred -> X - 1) *)
      Some
        (NativeCall2
           ( Cerlang.Atom "erlang",
             Cerlang.Atom "-",
             (match l with
             | "opp" -> Cerlang.EInt Z.zero
             | _ -> Cerlang.EInt Z.one),
             l = "opp" ))
  | "abs", true -> Some (call "erlang" "abs" 1)
  | "eqb", true -> Some (call "erlang" "=:=" 2)
  | "leb", true -> Some (call "erlang" "=<" 2)
  | "ltb", true -> Some (call "erlang" "<" 2)
  | ("of_nat" | "to_nat"), true -> Some NativeIdentity (* both are integers *)
  | _ -> None

(* ErlangBIF.HMap methods -> maps module calls; the EqDec dictionary
   (leading argument) is dropped untranslated *)
let global_to_map (r : global) : target option =
  match
    (Id.to_string (Table.label_of_r r), ModPath.to_string (Table.modpath_of_r r))
  with
  | "put", "Erlang.BIF.HMap" -> Some (call_drop "maps" "put" 3)
  | "find", "Erlang.BIF.HMap" -> Some (call_drop "maps" "find" 2)
  | "remove", "Erlang.BIF.HMap" -> Some (call_drop "maps" "remove" 2)
  | "keys", "Erlang.BIF.HMap" -> Some (call_drop "maps" "keys" 1)
  | "fold", "Erlang.BIF.HMap" -> Some (call_drop "maps" "fold" 3)
  | "make", "Erlang.BIF.HMap" -> Some (NativeValue (Cerlang.EMap []))
  | _ -> None

(* the combined classification *)
let global_target (r : global) : target =
  match global_to_z r with
  | Some t -> t
  | None -> ( match global_to_map r with Some t -> t | None -> NotNative)

(* arity of the native call, if this global is one *)
let call_arity r =
  match global_target r with
  | NativeCall (_, _, a, _) -> Some a
  | NativeCall2 _ -> Some 1
  | _ -> None

(* render a fully-applied native call (exactly [arity] translated args,
 * after* dropping) *)
let render_call t args =
  match (t, args) with
  | NativeCall (m, f, _, _), a -> Cerlang.EApply (Cerlang.CGlobal (m, f), a)
  | NativeCall2 (m, f, syn, true), [ a ] ->
      Cerlang.EApply (Cerlang.CGlobal (m, f), [ syn; a ])
  | NativeCall2 (m, f, syn, false), [ a ] ->
      Cerlang.EApply (Cerlang.CGlobal (m, f), [ a; syn ])
  | NativeIdentity, [ a ] -> a
  | _ -> assert false

(* is this global one of the ErlangBIF definitions (never emitted as
   fundefs)? -- exactly the map family above *)
let is_erlangbif r = match global_to_map r with Some _ -> true | None -> false

let is_erlangbif_kn kn =
  match
    (Id.to_string (KerName.label kn), ModPath.to_string (KerName.modpath kn))
  with
  | ("put" | "find" | "remove" | "keys" | "fold" | "make"), "Erlang.BIF.HMap" ->
      true
  | _ -> false
