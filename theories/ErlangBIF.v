(* Erlang-facing datatypes.                                              *)
(*                                                                       *)
(* Datatypes and operations that map onto native Erlang data, intended  *)
(* for use by extracted code. Currently only homogeneous maps, built on  *)
(* the LibTx Storage class (through its interface, via the list         *)
(* instance).                                                            *)

From Stdlib Require Import List Classes.EquivDec.
From LibTx Require Import Storage.Classes Storage.Instances.List.
Require Import ErlangTypes.
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
  | Some v => Ok v
  | None => Error
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

Definition fold (A : Type) (f : K -> V -> A -> A) (s : @t K V) (acc : A) : A :=
  foldT A f (unwrap s) (keys s) acc (fun _ H => H).

End HMap.

End HMap.
