Declare ML Module "rocq-core-erlang".

From Erlang Require Import GenServer.

(* Two different gen_servers: a counter and a toggle. Each is a module
   ascribing to the gen_server module type; separate extraction emits
   each as its own dotted Erlang module ('gen_server.Counter',
   'gen_server.Toggle').

   NB: this test must not use builtin library functions (Nat.sub,
   negb, ...). Each CoreErlang Separate Extraction command is its own
   project, and the per-test extractions run in parallel into the
   same directory, so two tests would race over the shared dependency
   module (e.g. 'Corelib.Init.Nat', each shard containing only its
   own closure). Until the pipeline merges such shards, stay
   structural here: match on constructors instead of calling stdlib
   functions. *)

Module Counter <: gen_server.
  Definition InitArg := nat.

  Definition ExitReason := bool.

  Definition State := nat.

  Inductive cast := inc | dec.

  Definition Cast := cast.

  Definition init (n : InitArg) : @init_res State ExitReason := i_ok n.

  Definition handle_cast
    (c : Cast) (s : State) : @handle_cast_res State ExitReason :=
    cst_noreply
      match c with
      | inc => S s
      | dec => match s with
              | O => O
              | S m => m
              end
              (* not [s - 1]: no builtin dependencies, see above *)
      end.
End Counter.

Module Toggle <: gen_server.
  Definition InitArg := bool.

  Definition ExitReason := nat.

  Definition State := bool.

  Inductive tcast := flip | noop.

  Definition Cast := tcast.

  Definition init (b : InitArg) : @init_res State ExitReason := i_ok b.

  Definition handle_cast
    (c : Cast) (s : State) : @handle_cast_res State ExitReason :=
    cst_noreply
      match c, s with
      | flip, true => false
      | flip, false => true
      | noop, _ => s
      end.
End Toggle.

CoreErlang Separate Extraction
  Counter.init Counter.handle_cast Toggle.init Toggle.handle_cast.
