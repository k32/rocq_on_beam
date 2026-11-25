Declare ML Module "rocq_on_beam".
Require Extraction.
Extraction Language Erlang.

Fixpoint t_mutual_fix (a : nat) :=
  let x := S a in
  match a with
  | O => x
  | S a => t_mutual_fix2 a
  end
with
t_mutual_fix2 a :=
  match a with
  | O => O
  | S a => t_mutual_fix a
  end.

Extraction t_mutual_fix.
