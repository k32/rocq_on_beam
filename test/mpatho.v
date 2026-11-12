Declare ML Module "rocq-core-erlang".

From Stdlib Require Import List ZArith Arith.
Require Import Erlang.Runtime.
Import ListNotations.

(* Pathological map cases. Every oracle is computed by the kernel
   (vm_compute) through the Rocq Storage implementation, so each test
   checks that the native maps-based lowering agrees with the list-
   based reference. All folds are order-independent (commutative
   functions over distinct keys), since native maps:fold order is
   arbitrary. *)

(* overwriting: the last put must win *)
Definition mput3 (k : nat) : MapFindResult :=
  HMap.find k
    (HMap.put k 3 (HMap.put k 2 (HMap.put k 1 HMap.make))).

Definition mput3_result : MapFindResult := Eval vm_compute in mput3 7.

(* remove and reinsert; removing an absent key changes nothing *)
Definition mremove (k : nat) : nat :=
  let m := HMap.put 1 10 (HMap.put 2 20 HMap.make) in
  let m := HMap.remove k m in
  let m := HMap.put 3 30 m in
  HMap.fold nat (fun k' v acc => v + acc) 0 m.

Definition mremove_result : nat := Eval vm_compute in mremove 1.
Definition mremove_absent_result : nat := Eval vm_compute in mremove 9.

(* maps of maps: the inner value is itself a map *)
Definition mnest (k : nat) : MapFindResult :=
  let inner0 := HMap.put 1 100 HMap.make in
  let inner1 := HMap.put 2 200 inner0 in
  let outer := HMap.put 11 inner0 (HMap.put 22 inner1 HMap.make) in
  match HMap.find k outer with
  | ok inner => ok (HMap.fold nat (fun _ v acc => v + acc) 0 inner)
  | error => error
  end.

Definition mnest_22_result : MapFindResult := Eval vm_compute in mnest 22.
Definition mnest_33_result : MapFindResult := Eval vm_compute in mnest 33.

(* a 50-entry map built by recursion, then summed *)
Fixpoint mbuild (i : nat) : HMap.t :=
  match i with
  | 0 => HMap.make
  | S j => HMap.put j (j * j) (mbuild j)
  end.

Definition mbig : nat := HMap.fold nat (fun _ v acc => v + acc) 0 (mbuild 50).

Definition mbig_result : nat := Eval vm_compute in mbig.

(* a fold that builds a new map (key-shifted), then folded again *)
Definition mchain (m : HMap.t) : nat :=
  let shifted := HMap.fold _ (fun k v acc => HMap.put (k + 100) v acc) HMap.make m in
  HMap.fold nat (fun _ v acc => v + acc) 0 shifted.

Definition mchain_result : nat :=
  Eval vm_compute in mchain (HMap.put 1 10 (HMap.put 2 20 HMap.make)).

(* find over a mixed present/absent key list, branching on the result *)
Fixpoint msumkeys (l : list nat) (m : HMap.t) : nat :=
  match l with
  | [] => 0
  | k :: t =>
      match HMap.find k m with
      | ok v => v + msumkeys t m
      | error => msumkeys t m
      end
  end.

Definition msumkeys_result : nat :=
  Eval vm_compute in
    msumkeys [1; 2; 3; 4; 5] (HMap.put 1 10 (HMap.put 3 30 (HMap.put 5 50 HMap.make))).

(* one map value shared by two folds *)
Definition mshare : nat :=
  let m := HMap.put 1 10 (HMap.put 2 20 HMap.make) in
  let a := HMap.fold nat (fun _ v acc => v + acc) 0 m in
  let b := HMap.fold nat (fun k _ acc => k + acc) 0 m in
  a * 1000 + b.

Definition mshare_result : nat := Eval vm_compute in mshare.

(* partially applied put at various positions *)
Definition mpartial (m : HMap.t) : HMap.t :=
  let put1 : nat -> HMap.t -> HMap.t := HMap.put 1 in
  let putv : HMap.t -> HMap.t := put1 111 in
  HMap.put 2 22 (putv m).

Definition mpartial_result : nat :=
  Eval vm_compute in
    HMap.fold nat (fun _ v acc => v + acc) 0 (mpartial (mbuild 5)).

CoreErlang Separate Extraction mput3 mremove mnest mbuild mbig mchain msumkeys mshare mpartial mput3_result mremove_result mremove_absent_result mnest_22_result mnest_33_result mbig_result mchain_result msumkeys_result mshare_result mpartial_result.
