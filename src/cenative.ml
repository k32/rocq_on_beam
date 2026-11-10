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
(*----------------------------------------------------------------------- *)

module Miniml = Extraction_plugin.Miniml
open Miniml

(* -------------------------------------------------------------------- *)
(* Registry                                                             *)
(* -------------------------------------------------------------------- *)

let locate_global names =
  List.fold_left
    (fun acc q ->
      match acc with
      | Some _ -> acc
      | None -> (
          try Some (Smartlocate.global_with_alias (Libnames.qualid_of_string q))
          with _ -> None))
    None names

(* resolved lazily: the stdlib may not be loaded when the plugin loads.
   We compare [GlobRef.t]s rather than kernel names: constructors of
   one inductive share a KerName ([Table.repr_of_r] is not
   constructor-specific), which would make O and S indistinguishable. *)
let gr_of names = lazy (locate_global names)

let native_nil =
  gr_of
    [
      "Corelib.Init.Datatypes.nil"; "Stdlib.Datatypes.nil"; "Coq.Datatypes.nil";
    ]

let native_cons =
  gr_of
    [
      "Corelib.Init.Datatypes.cons";
      "Stdlib.Datatypes.cons";
      "Coq.Datatypes.cons";
    ]

let native_pair =
  gr_of
    [
      "Corelib.Init.Datatypes.pair";
      "Stdlib.Datatypes.pair";
      "Coq.Datatypes.pair";
    ]

let native_O =
  gr_of
    [
      "Corelib.Init.Datatypes.O";
      "Stdlib.Init.Datatypes.O";
      "Coq.Init.Datatypes.O";
    ]

let native_S =
  gr_of
    [
      "Corelib.Init.Datatypes.S";
      "Stdlib.Init.Datatypes.S";
      "Coq.Init.Datatypes.S";
    ]

(* Z and positive constructors (binary integers) *)
let native_Z0 = gr_of [ "Corelib.Numbers.BinNums.Z0" ]
let native_Zpos = gr_of [ "Corelib.Numbers.BinNums.Zpos" ]
let native_Zneg = gr_of [ "Corelib.Numbers.BinNums.Zneg" ]
let native_xH = gr_of [ "Corelib.Numbers.BinNums.xH" ]
let native_xO = gr_of [ "Corelib.Numbers.BinNums.xO" ]
let native_xI = gr_of [ "Corelib.Numbers.BinNums.xI" ]

(* the HMap wrapper from our Erlang support library (present only when
   the theories are loaded): discerns a map from a regular list *)
let native_hmapw = gr_of [ "ErlangBIF.HMap.wrap" ]

(* MapFindResult from ErlangTypes: isomorphic to native maps:find's
   {ok, Value} | error *)
let native_ok = gr_of [ "ErlangTypes.ok" ]
let native_error = gr_of [ "ErlangTypes.error" ]

let is_native (r : global) = function
  | (lazy (Some g)) -> r.glob = g
  | (lazy None) -> false

let is_nil r = is_native r native_nil
let is_cons r = is_native r native_cons
let is_pair r = is_native r native_pair
let is_O r = is_native r native_O
let is_S r = is_native r native_S
let is_Z0 r = is_native r native_Z0
let is_Zpos r = is_native r native_Zpos
let is_Zneg r = is_native r native_Zneg
let is_xH r = is_native r native_xH
let is_xO r = is_native r native_xO
let is_xI r = is_native r native_xI
let is_hmapw r = is_native r native_hmapw
let is_ok r = is_native r native_ok
let is_error r = is_native r native_error

(* -------------------------------------------------------------------- *)
(* Helpers shared by the lowering rules                                  *)
(* -------------------------------------------------------------------- *)

let erlang_call f args =
  Cerlang.ECall (Cerlang.Atom "erlang", Cerlang.Atom f, args)

let guard_true = Cerlang.EAtom (Cerlang.Atom "true")

(* -------------------------------------------------------------------- *)
(* Literal folding                                                       *)
(* -------------------------------------------------------------------- *)

(* literal folding cap for nat S-chains: beyond this, unfold into
   arithmetic calls instead of one huge integer *)
let nat_fold_limit = 100000

(* number of S constructors enclosing the O of a nat literal, if [t]
   is a literal nat *)
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

let is_native_constructor (r : global) =
  is_nil r || is_cons r || is_pair r || is_O r || is_S r || is_Z0 r || is_Zpos r
  || is_Zneg r || is_xH r || is_xO r || is_xI r || is_hmapw r || is_ok r
  || is_error r

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
     HMap.Wrap l -> {'HMapW', l}   (the map wrapper: discerns a map
                                    from the regular native list inside) *)
let translate_constructor (tr : ml_ast -> Cerlang.expr) (r : global)
    (args : ml_ast list) : Cerlang.expr =
  let open Cerlang in
  let bad name =
    ETuple (EAtom (Atom ("coq_bad_" ^ name)) :: List.map tr args)
  in
  if is_nil r && args = [] then ENil
  else if is_cons r then
    match args with [ h; t ] -> ECons (tr h, tr t) | _ -> bad "cons"
  else if is_pair r then ETuple (List.map tr args)
  else if is_O r && args = [] then EInt "0"
  else if is_S r then
    match args with
    | [ t ] -> (
        match nat_literal (MLcons (Tunknown, r, [ t ])) with
        | Some n when n <= nat_fold_limit -> EInt (string_of_int n)
        | _ -> erlang_call "+" [ tr t; EInt "1" ])
    | _ -> bad "S"
  else if is_Z0 r && args = [] then EInt "0"
  else if is_Zpos r then
    (* a positive is lowered to its integer value: Zpos p = p *)
    match args with
    | [ t ] -> (
        match z_literal (MLcons (Tunknown, r, [ t ])) with
        | Some n -> EInt (Z.to_string n)
        | _ -> tr t)
    | _ -> bad "Zpos"
  else if is_Zneg r then
    match args with
    | [ t ] -> (
        match z_literal (MLcons (Tunknown, r, [ t ])) with
        | Some n -> EInt (Z.to_string n)
        | _ -> erlang_call "-" [ EInt "0"; tr t ])
    | _ -> bad "Zneg"
  else if is_xH r && args = [] then EInt "1"
  else if is_hmapw r then
    (* the Rocq representation (wrapped association list) bridges to
       the native map representation *)
    match args with
    | [ l ] -> Cerlang.ECall (Cerlang.Atom "maps", Cerlang.Atom "from_list", [ tr l ])
    | _ -> bad "HMapW"
  else if is_ok r then
    match args with
    | [ v ] -> ETuple [ EAtom (Atom "ok"); tr v ]
    | _ -> bad "ok"
  else if is_error r && args = [] then EAtom (Atom "error")
  else if is_xO r || is_xI r then
    match args with
    | [ t ] -> (
        match pos_literal (MLcons (Tunknown, r, [ t ])) with
        | Some n -> EInt (Z.to_string n)
        | _ ->
            if is_xO r then erlang_call "*" [ tr t; EInt "2" ]
            else
              erlang_call "+" [ erlang_call "*" [ tr t; EInt "2" ]; EInt "1" ])
    | _ -> bad "pos"
  else assert false

(* Patterns that can be expressed as patterns (lists, pairs, nullary
   integer literals).  Arithmetic constructor patterns (O/S, Z,
   positive) cannot: shallow occurrences go through [translate_branch]
   instead, and deep ones have no native pattern at all. *)
let is_native_pattern (r : global) =
  is_nil r || is_cons r || is_pair r || is_O r || is_Z0 r || is_xH r || is_ok r
  || is_error r

(* [translate_pattern sub r ps] (total when [is_native_pattern r]);
   [sub] translates the sub-patterns. *)
let translate_pattern (sub : ml_pattern -> Cerlang.pattern) (r : global)
    (ps : ml_pattern list) : Cerlang.pattern =
  let open Cerlang in
  if is_nil r && ps = [] then PNil
  else if is_cons r then
    match ps with
    | [ h; t ] -> PCons (sub h, sub t)
    | _ -> PTuple [ PAtom (Atom "coq_bad_cons") ]
  else if is_pair r then PTuple (List.map sub ps)
  else if (is_O r || is_Z0 r) && ps = [] then PInt "0"
  else if is_xH r && ps = [] then PInt "1"
  else if is_ok r then
    match ps with
    | [ p ] -> PTuple [ PAtom (Atom "ok"); sub p ]
    | _ -> PTuple [ PAtom (Atom "ok") ]
  else if is_error r && ps = [] then PAtom (Atom "error")
  else assert false

(* Pusual: constructor applied to all its (named) arguments *)
let is_native_usual (r : global) =
  is_nil r || is_cons r || is_pair r || is_O r || is_Z0 r || is_xH r || is_ok r
  || is_error r

let translate_pattern_usual (r : global) (names : Cerlang.var list) :
    Cerlang.pattern =
  let open Cerlang in
  if is_nil r && names = [] then PNil
  else if is_cons r then
    match names with
    | [ h; t ] -> PCons (PVar h, PVar t)
    | l -> PTuple (PAtom (Atom "coq_bad_cons") :: List.map (fun v -> PVar v) l)
  else if is_pair r then PTuple (List.map (fun v -> PVar v) names)
  else if is_O r || is_Z0 r then PInt "0"
  else if is_xH r then PInt "1"
  else if is_ok r then
    PTuple (PAtom (Atom "ok") :: List.map (fun w -> PVar w) names)
  else if is_error r && names = [] then PAtom (Atom "error")
  else assert false

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
      no_binder (PInt "0") guard_true
  | (Pcons (r, []) | Pusual r) when is_xH r -> no_binder (PInt "1") guard_true
  | Pcons (r, [ Pwild ]) | Pcons (r, [ Prel 1 ]) | Pusual r ->
      if is_S r then
        one_var
          (erlang_call ">" [ x; EInt "0" ])
          (fun _ -> erlang_call "-" [ x; EInt "1" ])
      else if is_Zpos r then
        one_var (erlang_call ">" [ x; EInt "0" ]) (fun _ -> x)
      else if is_Zneg r then
        one_var
          (erlang_call "<" [ x; EInt "0" ])
          (fun _ -> erlang_call "-" [ EInt "0"; x ])
      else if is_xO r then
        one_var
          (erlang_call "=:=" [ erlang_call "band" [ x; EInt "1" ]; EInt "0" ])
          (fun _ -> erlang_call "bsr" [ x; EInt "1" ])
      else if is_xI r then
        one_var
          (erlang_call "=:=" [ erlang_call "band" [ x; EInt "1" ]; EInt "1" ])
          (fun _ -> erlang_call "bsr" [ x; EInt "1" ])
      else None
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
   whether the user imported Corelib or Stdlib. *)
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
  | NativeIdentity  (* the identity: the argument itself (e.g. Z.of_nat) *)
  | NativeValue of Cerlang.expr  (* a fixed value *)
  | NotNative

(* fixed HMap.make: the empty map literal *)
let erlangbif_make = lazy (locate_global [ "ErlangBIF.HMap.make" ])

let erlangbif_names = [
  "ErlangBIF.HMap.make";
  "ErlangBIF.HMap.put";
  "ErlangBIF.HMap.find";
  "ErlangBIF.HMap.remove";
  "ErlangBIF.HMap.keys";
  "ErlangBIF.HMap.fold";
]

let native_targets = lazy (
  List.concat_map
    (fun (qs, m, f, arity, drop) ->
      List.filter_map
        (fun q ->
          match locate_global [ q ] with
          | Some g -> Some (g, NativeCall (Cerlang.Atom m, Cerlang.Atom f, arity, drop))
          | None -> None)
        qs)
    ([ [ "Corelib.BinNums.IntDef.Z.add"; "Stdlib.ZArith.BinIntDef.Z.add";
         "Stdlib.ZArith.BinInt.Z.add" ],
        "erlang", "+", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.sub"; "Stdlib.ZArith.BinIntDef.Z.sub";
         "Stdlib.ZArith.BinInt.Z.sub" ],
        "erlang", "-", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.mul"; "Stdlib.ZArith.BinIntDef.Z.mul";
         "Stdlib.ZArith.BinInt.Z.mul" ],
        "erlang", "*", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.max"; "Stdlib.ZArith.BinIntDef.Z.max";
         "Stdlib.ZArith.BinInt.Z.max" ],
        "erlang", "max", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.min"; "Stdlib.ZArith.BinIntDef.Z.min";
         "Stdlib.ZArith.BinInt.Z.min" ],
        "erlang", "min", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.opp"; "Stdlib.ZArith.BinIntDef.Z.opp";
         "Stdlib.ZArith.BinInt.Z.opp" ],
        "erlang", "-", 2, 0 (* 0 - X *);
       [ "Corelib.BinNums.IntDef.Z.succ"; "Stdlib.ZArith.BinIntDef.Z.succ";
         "Stdlib.ZArith.BinInt.Z.succ" ],
        "erlang", "+", 2, 0 (* X + 1 *);
       [ "Corelib.BinNums.IntDef.Z.pred"; "Stdlib.ZArith.BinIntDef.Z.pred";
         "Stdlib.ZArith.BinInt.Z.pred" ],
        "erlang", "-", 2, 0 (* X - 1 *);
       [ "Corelib.BinNums.IntDef.Z.abs"; "Stdlib.ZArith.BinIntDef.Z.abs";
         "Stdlib.ZArith.BinInt.Z.abs" ],
        "erlang", "abs", 1, 0;
       [ "Corelib.BinNums.IntDef.Z.eqb"; "Stdlib.ZArith.BinIntDef.Z.eqb";
         "Stdlib.ZArith.BinInt.Z.eqb" ],
        "erlang", "=:=", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.leb"; "Stdlib.ZArith.BinIntDef.Z.leb";
         "Stdlib.ZArith.BinInt.Z.leb" ],
        "erlang", "=<", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.ltb"; "Stdlib.ZArith.BinIntDef.Z.ltb";
         "Stdlib.ZArith.BinInt.Z.ltb" ],
        "erlang", "<", 2, 0;
       [ "Corelib.BinNums.IntDef.Z.of_nat"; "Stdlib.ZArith.BinIntDef.Z.of_nat";
         "Stdlib.ZArith.BinInt.Z.of_nat" ],
        "erlang", "", 1, 0; (* identity, see NativeIdentity *)
       [ "Corelib.BinNums.IntDef.Z.to_nat"; "Stdlib.ZArith.BinIntDef.Z.to_nat";
         "Stdlib.ZArith.BinInt.Z.to_nat" ],
        "erlang", "", 1, 0;
       (* ErlangBIF.HMap: the EqDec dictionary is dropped untranslated *)
       [ "ErlangBIF.HMap.put" ], "maps", "put", 3, 1;
       [ "ErlangBIF.HMap.find" ], "maps", "find", 2, 1;
       [ "ErlangBIF.HMap.remove" ], "maps", "remove", 2, 1;
       [ "ErlangBIF.HMap.keys" ], "maps", "keys", 1, 1;
       [ "ErlangBIF.HMap.fold" ], "maps", "fold", 3, 1 ]))

let global_target (r : global) : target =
  match List.find_opt (fun (g, _) -> r.glob = g) (Lazy.force native_targets) with
  | Some (_, NativeCall (_, f, 1, 0)) when (match f with Cerlang.Atom n -> n = "") -> NativeIdentity
  | Some (_, t) -> t
  | None ->
    (match Lazy.force erlangbif_make with
     | Some g when r.glob = g -> NativeValue (Cerlang.EMap [])
     | _ -> NotNative)

(* arity of the native call, if this global is one *)
let call_arity r =
  match global_target r with
  | NativeCall (_, _, a, _) -> Some a
  | _ -> None

(* render a fully-applied native call (exactly [arity] translated args,
 * after* dropping) *)
let render_call t args =
  match (t, args) with
  | NativeCall (m, f, _, _), a -> Cerlang.ECall (m, f, a)
  | NativeIdentity, [ a ] -> a
  | _ -> assert false

(* is the kernel name one of the ErlangBIF definitions (never emitted
   as fundefs)? *)
let is_erlangbif_kn kn =
  let name =
    ModPath.to_string (KerName.modpath kn) ^ "." ^ Id.to_string (KerName.label kn)
  in
  List.mem name erlangbif_names

let is_erlangbif r =
  let name =
    ModPath.to_string (Table.modpath_of_r r) ^ "." ^ Id.to_string (Table.label_of_r r)
  in
  List.mem name erlangbif_names
