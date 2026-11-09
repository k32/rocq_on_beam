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
      | None ->
        (try Some (Smartlocate.global_with_alias (Libnames.qualid_of_string q))
         with _ -> None))
    None names

(* resolved lazily: the stdlib may not be loaded when the plugin loads.
   We compare [GlobRef.t]s rather than kernel names: constructors of
   one inductive share a KerName ([Table.repr_of_r] is not
   constructor-specific), which would make O and S indistinguishable. *)
let gr_of names = lazy (locate_global names)

let native_nil  = gr_of ["Corelib.Init.Datatypes.nil";  "Stdlib.Datatypes.nil";  "Coq.Datatypes.nil"]
let native_cons = gr_of ["Corelib.Init.Datatypes.cons"; "Stdlib.Datatypes.cons"; "Coq.Datatypes.cons"]
let native_pair = gr_of ["Corelib.Init.Datatypes.pair"; "Stdlib.Datatypes.pair"; "Coq.Datatypes.pair"]
let native_O    = gr_of ["Corelib.Init.Datatypes.O";    "Stdlib.Init.Datatypes.O"; "Coq.Init.Datatypes.O"]
let native_S    = gr_of ["Corelib.Init.Datatypes.S";    "Stdlib.Init.Datatypes.S"; "Coq.Init.Datatypes.S"]
(* Z and positive constructors (binary integers) *)
let native_Z0   = gr_of ["Corelib.Numbers.BinNums.Z0"]
let native_Zpos = gr_of ["Corelib.Numbers.BinNums.Zpos"]
let native_Zneg = gr_of ["Corelib.Numbers.BinNums.Zneg"]
let native_xH   = gr_of ["Corelib.Numbers.BinNums.xH"]
let native_xO   = gr_of ["Corelib.Numbers.BinNums.xO"]
let native_xI   = gr_of ["Corelib.Numbers.BinNums.xI"]

let is_native (r : global) = function
  | lazy (Some g) -> r.glob = g
  | lazy None -> false

let is_nil  r = is_native r native_nil
let is_cons r = is_native r native_cons
let is_pair r = is_native r native_pair
let is_O    r = is_native r native_O
let is_S    r = is_native r native_S
let is_Z0   r = is_native r native_Z0
let is_Zpos r = is_native r native_Zpos
let is_Zneg r = is_native r native_Zneg
let is_xH   r = is_native r native_xH
let is_xO   r = is_native r native_xO
let is_xI   r = is_native r native_xI

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
  | MLcons (_, r, [t]) when is_S r ->
    Option.map (( + ) 1) (nat_literal t)
  | _ -> None

(* value of a literal positive (binary: xH=1, xO p=2p, xI p=2p+1),
   computed exactly with arbitrary-precision integers (zarith) *)
let rec pos_literal = function
  | MLcons (_, r, []) when is_xH r -> Some Z.one
  | MLcons (_, r, [t]) when is_xO r ->
    Option.map (fun p -> Z.mul (Z.of_int 2) p) (pos_literal t)
  | MLcons (_, r, [t]) when is_xI r ->
    Option.map (fun p -> Z.add (Z.mul (Z.of_int 2) p) Z.one) (pos_literal t)
  | _ -> None

(* signed value of a literal Z *)
let z_literal = function
  | MLcons (_, r, []) when is_Z0 r -> Some Z.zero
  | MLcons (_, r, [t]) when is_Zpos r -> pos_literal t
  | MLcons (_, r, [t]) when is_Zneg r -> Option.map Z.neg (pos_literal t)
  | _ -> None

(* -------------------------------------------------------------------- *)
(* Constructors and patterns                                             *)
(* -------------------------------------------------------------------- *)

(* Constructor translation for a natively lowered constructor.
   [tr] translates argument expressions; returns [None] when [r] is
   not a native constructor (the caller falls back to the tagged
   encoding).

     nil   -> []
     cons  -> [H|T]
     pair  -> {A,B}            (untagged tuple)
     O     -> 0                (nat literals fold to one integer)
     S x   -> call 'erlang':'+'(x, 1)
     Z0    -> 0;  Zpos p -> p;  Zneg p -> 0 - p
     xH    -> 1;  xO p -> 2*p;  xI p -> 2*p + 1 *)
let constructor (tr : ml_ast -> Cerlang.expr) (r : global) (args : ml_ast list) :
  Cerlang.expr option =
  let open Cerlang in
  let bad name = ETuple (EAtom (Atom ("coq_bad_" ^ name)) :: List.map tr args) in
  if is_nil r && args = [] then Some ENil
  else if is_cons r then
    (match args with
     | [h; t] -> Some (ECons (tr h, tr t))
     | _ -> Some (bad "cons"))
  else if is_pair r then Some (ETuple (List.map tr args))
  else if is_O r && args = [] then Some (EInt "0")
  else if is_S r then
    (match args with
     | [t] ->
       (match nat_literal (MLcons (Tunknown, r, [t])) with
        | Some n when n <= nat_fold_limit -> Some (EInt (string_of_int n))
        | _ -> Some (erlang_call "+" [tr t; EInt "1"]))
     | _ -> Some (bad "S"))
  else if is_Z0 r && args = [] then Some (EInt "0")
  else if is_Zpos r then
    (* a positive is lowered to its integer value: Zpos p = p *)
    (match args with
     | [t] ->
       (match z_literal (MLcons (Tunknown, r, [t])) with
        | Some n -> Some (EInt (Z.to_string n))
        | _ -> Some (tr t))
     | _ -> Some (bad "Zpos"))
  else if is_Zneg r then
    (match args with
     | [t] ->
       (match z_literal (MLcons (Tunknown, r, [t])) with
        | Some n -> Some (EInt (Z.to_string n))
        | _ -> Some (erlang_call "-" [EInt "0"; tr t]))
     | _ -> Some (bad "Zneg"))
  else if is_xH r && args = [] then Some (EInt "1")
  else if is_xO r || is_xI r then
    (match args with
     | [t] ->
       (match pos_literal (MLcons (Tunknown, r, [t])) with
        | Some n -> Some (EInt (Z.to_string n))
        | _ ->
           if is_xO r
           then Some (erlang_call "*" [tr t; EInt "2"])
           else Some (erlang_call "+" [erlang_call "*" [tr t; EInt "2"];
                                       EInt "1"]))
     | _ -> Some (bad "pos"))
  else None

(* Pattern translation for a natively lowered constructor with
   sub-patterns [ps] ([sub] translates them); returns [None] when [r]
   is not a native constructor. Only patterns that stay patterns
   (lists, pairs, nullary literals) are handled here; arithmetic
   matches (O/S, Z, positive) go through [rewrite_branch] instead.
   Deep sub-patterns on arithmetic constructors are rejected
   ([Some _ -> None] via the caller's tagged fallback). *)
let pattern (sub : ml_pattern -> Cerlang.pattern) (r : global) (ps : ml_pattern list) :
  Cerlang.pattern option =
  let open Cerlang in
  if is_nil r && ps = [] then Some PNil
  else if is_cons r then
    (match ps with
     | [h; t] -> Some (PCons (sub h, sub t))
     | _ -> Some (PTuple [PAtom (Atom "coq_bad_cons")]))
  else if is_pair r then Some (PTuple (List.map sub ps))
  else if (is_O r || is_Z0 r) && ps = [] then Some (PInt "0")
  else if is_xH r && ps = [] then Some (PInt "1")
  else None

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
let rewrite_branch (v : Cerlang.var) (pat : ml_pattern) (name : Cerlang.var option) :
  (Cerlang.pattern * Cerlang.expr * (Cerlang.var * Cerlang.expr) list) option =
  let open Cerlang in
  let no_binder pat' g = Some (pat', g, []) in
  let one_var g mk = Some (PVar v, g, match name with
                           | Some x -> [x, mk x]
                           | None -> []) in
  let x = EVar v in
  match pat with
  | Pwild | Prel _ -> no_binder PWild guard_true
  | Pcons (r, []) | Pusual r
    when is_O r || is_Z0 r ->
    no_binder (PInt "0") guard_true
  | Pcons (r, []) | Pusual r when is_xH r ->
    no_binder (PInt "1") guard_true
  | Pcons (r, [Pwild]) | Pcons (r, [Prel 1]) | Pusual r ->
    if is_S r then
      one_var (erlang_call ">" [x; EInt "0"])
        (fun _ -> erlang_call "-" [x; EInt "1"])
    else if is_Zpos r then
      one_var (erlang_call ">" [x; EInt "0"]) (fun _ -> x)
    else if is_Zneg r then
      one_var (erlang_call "<" [x; EInt "0"])
        (fun _ -> erlang_call "-" [EInt "0"; x])
    else if is_xO r then
      one_var (erlang_call "=:=" [erlang_call "band" [x; EInt "1"]; EInt "0"])
        (fun _ -> erlang_call "bsr" [x; EInt "1"])
    else if is_xI r then
      one_var (erlang_call "=:=" [erlang_call "band" [x; EInt "1"]; EInt "1"])
        (fun _ -> erlang_call "bsr" [x; EInt "1"])
    else None
  | _ -> None

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

type zop =
  | Bin of string        (* 'erlang':'f'(A,B) *)
  | Un of string         (* 'erlang':'f'(A) *)
  | Const2 of string * int  (* 'erlang':'f'(A, c) *)
  | Compare              (* 3-way case, returns 'Eq' | 'Lt' | 'Gt' *)
  | Identity             (* fun X -> X *)

let z_entries = [
  ["Corelib.BinNums.IntDef.Z.add"; "Stdlib.ZArith.BinIntDef.Z.add"; "Stdlib.ZArith.BinInt.Z.add"], Bin "+";
  ["Corelib.BinNums.IntDef.Z.sub"; "Stdlib.ZArith.BinIntDef.Z.sub"; "Stdlib.ZArith.BinInt.Z.sub"], Bin "-";
  ["Corelib.BinNums.IntDef.Z.mul"; "Stdlib.ZArith.BinIntDef.Z.mul"; "Stdlib.ZArith.BinInt.Z.mul"], Bin "*";
  ["Corelib.BinNums.IntDef.Z.max"; "Stdlib.ZArith.BinIntDef.Z.max"; "Stdlib.ZArith.BinInt.Z.max"], Bin "max";
  ["Corelib.BinNums.IntDef.Z.min"; "Stdlib.ZArith.BinIntDef.Z.min"; "Stdlib.ZArith.BinInt.Z.min"], Bin "min";
  ["Corelib.BinNums.IntDef.Z.opp"; "Stdlib.ZArith.BinIntDef.Z.opp"; "Stdlib.ZArith.BinInt.Z.opp"], Const2 ("-", 0);
  ["Corelib.BinNums.IntDef.Z.succ"; "Stdlib.ZArith.BinIntDef.Z.succ"; "Stdlib.ZArith.BinInt.Z.succ"], Const2 ("+", 1);
  ["Corelib.BinNums.IntDef.Z.pred"; "Stdlib.ZArith.BinIntDef.Z.pred"; "Stdlib.ZArith.BinInt.Z.pred"], Const2 ("-", 1);
  ["Corelib.BinNums.IntDef.Z.abs"; "Stdlib.ZArith.BinIntDef.Z.abs"; "Stdlib.ZArith.BinInt.Z.abs"], Un "abs";
  ["Corelib.BinNums.IntDef.Z.eqb"; "Stdlib.ZArith.BinIntDef.Z.eqb"; "Stdlib.ZArith.BinInt.Z.eqb"], Bin "=:=";
  ["Corelib.BinNums.IntDef.Z.leb"; "Stdlib.ZArith.BinIntDef.Z.leb"; "Stdlib.ZArith.BinInt.Z.leb"], Bin "=<";
  ["Corelib.BinNums.IntDef.Z.ltb"; "Stdlib.ZArith.BinIntDef.Z.ltb"; "Stdlib.ZArith.BinInt.Z.ltb"], Bin "<";
  ["Corelib.BinNums.IntDef.Z.compare"; "Stdlib.ZArith.BinIntDef.Z.compare"; "Stdlib.ZArith.BinInt.Z.compare"], Compare;
  ["Corelib.BinNums.IntDef.Z.of_nat"; "Stdlib.ZArith.BinIntDef.Z.of_nat"; "Stdlib.ZArith.BinInt.Z.of_nat"], Identity;
  ["Corelib.BinNums.IntDef.Z.to_nat"; "Stdlib.ZArith.BinIntDef.Z.to_nat"; "Stdlib.ZArith.BinInt.Z.to_nat"], Identity;
]

let zops = lazy (
  List.concat_map
    (fun (qs, op) ->
      List.filter_map (fun q ->
          match locate_global [q] with
          | Some g -> Some (g, op)
          | None -> None) qs)
    z_entries)

let zop_of (r : global) =
  let tbl = Lazy.force zops in
  List.find_opt (fun (g, _) -> r.glob = g) tbl |> Option.map snd

(* apply an operator to exactly [op_arity] argument expressions;
   [fresh] mints a fresh variable (for the Compare case) *)
let op_apply ~(fresh : unit -> Cerlang.var) op (args : Cerlang.expr list) : Cerlang.expr =
  let open Cerlang in
  match op, args with
  | Bin f, [a; b] -> erlang_call f [a; b]
  | Un f, [a] -> erlang_call f [a]
  | Const2 (f, c), [a] -> erlang_call f [a; EInt (string_of_int c)]
  | Identity, [a] -> a
  | Compare, [a; b] ->
    let x = fresh () in
    ECase (a, [ (PVar x, erlang_call "=:=" [EVar x; b], EAtom (Atom "Eq"));
                (PVar x, erlang_call "<" [EVar x; b], EAtom (Atom "Lt"));
                (PVar x, guard_true, EAtom (Atom "Gt")) ])
  | _ -> assert false

(* the operator's arity *)
let op_arity = function
  | Bin _ -> 2 | Un _ -> 1 | Const2 _ -> 1 | Compare -> 2 | Identity -> 1
