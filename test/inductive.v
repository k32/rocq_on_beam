Declare ML Module "rocq-core-erlang".

(* A plain inductive with several constructors of different arities.
   Each constructor becomes a tagged tuple (or atom when nullary):
     Leaf -> 'Leaf'
     Node n l r -> {'Node', N, L, R} *)

Inductive tree : Type :=
  | Leaf : tree
  | Node : nat -> tree -> tree -> tree.

Fixpoint sum_tree (t : tree) : nat :=
  match t with
  | Leaf => O
  | Node n l r => n + (sum_tree l + sum_tree r)
  end.

Fixpoint mirror (t : tree) : tree :=
  match t with
  | Leaf => Leaf
  | Node n l r => Node n (mirror r) (mirror l)
  end.

(* Mutual inductive types, and a mutual fixpoint over them.
   red/green constructors are interleaved, so the extracted functions
   for both counts form one mutually recursive group. *)

Inductive red : Type :=
  | RLeaf : red
  | RNode : green -> red
with green : Type :=
  | GNil : green
  | GNode : red -> green -> green.

Fixpoint count_red (r : red) : nat :=
  match r with
  | RLeaf => O
  | RNode g => S (count_green g)
  end
with count_green (g : green) : nat :=
  match g with
  | GNil => O
  | GNode r _ => count_red r
  end.

CoreErlang Extraction "inductive" sum_tree mirror count_red count_green.
