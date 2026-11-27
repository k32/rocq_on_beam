(* Test extraction of external functions *)
Declare ML Module "rocq_on_beam".
Require Extraction.
Extraction Language Erlang.

From Stdlib Require Import List.
Import ListNotations.

Extract Constant plus => "erlang:+".

Definition t_call_custom a := a + a.

Recursive Extraction t_call_custom.
