Declare ML Module "coreerlang_plugin".

From Stdlib Require Import Arith List.
Require Import Erlang.
Import ListNotations.

(* HMap through the wrapper: extraction must render maps as
   {'HMapW', List}, distinct from regular native lists. find returns
   MapFindResult, lowered to native {ok, V} | error. *)

Definition m0 : @HMap.t nat nat := HMap.make.

Definition m1 : HMap.t := HMap.put 2 20 (HMap.put 1 10 m0).

Definition lookup (k : nat) : MapFindResult := HMap.find k m1.

Definition add (k v : nat) (m : HMap.t) : HMap.t :=
  HMap.put k v m.

Definition sum : nat := HMap.fold nat (fun k v a => k * v + a) m1 0.

(* matching on MapFindResult from Rocq code *)
Definition get_or_zero (k : nat) : nat :=
  match lookup k with
  | Ok v => v
  | Error => 0
  end.

CoreErlang Extraction "test_core/hmap" m1 lookup add sum get_or_zero.
