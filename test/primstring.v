Declare ML Module "rocq-core-erlang".

(* primitive strings lower to binaries *)
Require Import PrimString.

Definition greeting : string := "hi".

(* NB: string operations like [cat] extract as axiom markers and their
   arity mismatches make the module uncompilable; operations mapping is
   future work (see PLAN.md) -- only literals are lowered for now. *)
CoreErlang Extraction "primstring" greeting.
