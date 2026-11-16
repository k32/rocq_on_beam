(* Erlang-facing datatypes.                                              *)
(*                                                                       *)
(* Datatypes and operations that map onto native Erlang data, intended  *)
(* for use by extracted code. Currently only homogeneous maps, built on  *)
(* the LibTx Storage class (through its interface, via the list         *)
(* instance).                                                            *)

Declare ML Module "rocq-to-erlang".

From Stdlib Require Import List Classes.EquivDec.
From Stdlib Require ZArith.
From LibTx Require Import Storage.Classes Storage.Instances.List.
Require Import Types EqDecNat.
Import ListNotations.

Module HMap.

(* A wrapper around the bare association list: extraction matches on
   this constructor and renders a map as {'HMapW', List}, discerning it
   from a regular (native) list. The second constructor prevents Rocq's
   extraction from unboxing the single-constructor wrapper away. *)
Inductive t {K V : Type} : Type :=
| wrap : list (K * V) -> t
| empty : t.

Section HMap.
Context {K V : Type} `{EqDec K eq}.

Definition make : @t K V := wrap [].

(* the wrapped Storage representation; [empty] never occurs from
   [make]/[put], and reads as the empty map *)
Definition unwrap (s : @t K V) : list (K * V) :=
  match s with
  | wrap l => l
  | empty => []
  end.

Definition put (k : K) (v : V) (s : @t K V) : @t K V :=
  wrap (put k v (unwrap s)).

(* like native maps:find: {ok, Value} | error *)
Definition find (k : K) (s : @t K V) : MapFindResult :=
  match get k (unwrap s) with
  | Some v => ok v
  | None => error
  end.

Definition remove (k : K) (s : @t K V) : @t K V :=
  wrap (delete k (unwrap s)).

Definition keys (s : @t K V) : list K :=
  keys_snapshot (unwrap s).

(* A total fold: every key of [keys_snapshot] has a value
   ([keys_snapshot_some]), and the library's [getT] turns that
   existence proof into a total lookup -- no option is matched. *)
Fixpoint foldT (A : Type) (f : K -> V -> A -> A) (u : list (K * V))
    (l : list K) :
    forall (acc : A),
      (forall k, In k l -> In k (keys_snapshot u)) -> A :=
  match l with
  | [] => fun acc _ => acc
  | k :: l' =>
      fun acc Hsub =>
        f k (getT k u (Hsub k (in_eq k l')))
          (foldT A f u l' acc
             (fun k0 Hin => Hsub k0 (in_cons k k0 l' Hin)))
  end.

Definition fold (A : Type) (f : K -> V -> A -> A) (acc : A) (s : @t K V) : A :=
  foldT A f (unwrap s) (keys s) acc (fun _ H => H).

End HMap.

End HMap.

(* Native call targets: applications of these definitions extract as
   calls to the Erlang functions given as "module:function/arity"; the
   trailing integer counts leading Rocq arguments dropped untranslated
   (the EqDec dictionary of the HMap methods), 0 for none. Declared
   here, they apply to every importer of this module. *)

CoreErlang Native Call HMap.put := "maps:put/3" 1.
CoreErlang Native Call HMap.find := "maps:find/2" 1.
CoreErlang Native Call HMap.remove := "maps:remove/2" 1.
CoreErlang Native Call HMap.keys := "maps:keys/1" 1.
CoreErlang Native Call HMap.fold := "maps:fold/3" 1.
(* HMap.make stays handwired in the plugin: it lowers to the fixed
   value #{}, which no native call expresses. *)

(* The plain Z operations -> erlang BIFs. The same operations exist
   under three names (the canonical Corelib definition, the Stdlib
   re-export, and the Stdlib-internal path); which one a term
   references depends on how ZArith was imported, so all three are
   declared. The unary opp/pred/succ and the of_nat/to_nat conversions
   stay handwired in the plugin (synthetic BIF operands / identity). *)
CoreErlang Native Call Corelib.BinNums.IntDef.Z.add := "erlang:+/2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.sub := "erlang:-/2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.mul := "erlang:*/2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.max := "erlang:max/2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.min := "erlang:min/2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.eqb := "erlang:=:=/2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.leb := "erlang:=</2" 0.
CoreErlang Native Call Corelib.BinNums.IntDef.Z.ltb := "erlang:</2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.add := "erlang:+/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.sub := "erlang:-/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.mul := "erlang:*/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.max := "erlang:max/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.min := "erlang:min/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.abs := "erlang:abs/1" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.eqb := "erlang:=:=/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.leb := "erlang:=</2" 0.
CoreErlang Native Call Stdlib.ZArith.BinIntDef.Z.ltb := "erlang:</2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.add := "erlang:+/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.sub := "erlang:-/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.mul := "erlang:*/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.max := "erlang:max/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.min := "erlang:min/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.abs := "erlang:abs/1" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.eqb := "erlang:=:=/2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.leb := "erlang:=</2" 0.
CoreErlang Native Call Stdlib.ZArith.BinInt.Z.ltb := "erlang:</2" 0.
