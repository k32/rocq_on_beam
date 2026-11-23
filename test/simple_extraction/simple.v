Declare ML Module "rocq_on_beam".
Require Extraction.
Extraction Language Erlang.

Definition foo :=
  1.

Extraction foo.

Fixpoint bar (a : nat) :=
  match a with
  | O => O
  | S a => baz a
  end
with baz a :=
       match a with
       | O => O
       | S a => bar a
       end.

Extraction bar.
