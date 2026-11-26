Require Extraction.
Declare ML Module "rocq_on_beam".

Extraction Language Erlang.

Definition hi := 1.

Module m.
  Definition foo {T : Set} (a : T) :=
    (a, a).
End m.

Definition bar a :=
  let (x, x) := m.foo a in
  x + x.

Recursive Extraction hi bar.
