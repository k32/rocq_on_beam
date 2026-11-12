Declare ML Module "rocq-core-erlang".

From Erlang Require Import GenServer.

(* Two different gen_servers: a counter and a toggle. Each is a module
   ascribing to the gen_server module type; separate extraction emits
   each as its own dotted Erlang module ('gen_server.Counter',
   'gen_server.Toggle'). Each test is a separate OTP application
   running on its own node, so shared dependency modules of different
   tests never meet. *)

Module Counter <: gen_server.
  Definition InitArg := nat.

  Definition ExitReason := bool.

  Definition State := nat.

  Inductive cast := inc | dec.

  Definition Cast := cast.

  Inductive ccall := cget | cadd (n : nat).

  Definition Call := ccall.

  Definition Reply := nat.

  (* the From handle is only passed through *)
  Definition From := nat.

  Inductive cinfo := creset (n : nat).

  Definition Info := cinfo.

  Definition init (n : InitArg) : @init_res State ExitReason := i_ok n.

  Definition handle_cast
    (c : Cast) (s : State) : @handle_cast_res State ExitReason :=
    cst_noreply
      match c with
      | inc => S s
      | dec => s - 1
      end.

  Definition handle_call
    (c : Call) (f : From) (s : State)
    : @call_res Reply State ExitReason :=
    match c with
    | cget => c_reply s s
    | cadd n => c_reply n (s + n)
    end.

  Definition handle_info
    (i : Info) (s : State) : @info_res State ExitReason :=
    match i with
    | creset n => inf_noreply n
    end.
End Counter.

Module Toggle <: gen_server.
  Definition InitArg := bool.

  Definition ExitReason := nat.

  Definition State := bool.

  Inductive tcast := flip | noop.

  Definition Cast := tcast.

  Inductive tcall := tfire.

  Definition Call := tcall.

  Definition Reply := bool.

  Definition From := bool.

  Inductive tinfo := tnoop.

  Definition Info := tinfo.

  Definition init (b : InitArg) : @init_res State ExitReason := i_ok b.

  Definition handle_cast
    (c : Cast) (s : State) : @handle_cast_res State ExitReason :=
    cst_noreply
      match c, s with
      | flip, true => false
      | flip, false => true
      | noop, _ => s
      end.

  Definition handle_call
    (c : Call) (f : From) (s : State)
    : @call_res Reply State ExitReason :=
    c_noreply s.

  Definition handle_info
    (i : Info) (s : State) : @info_res State ExitReason :=
    inf_noreply s.
End Toggle.

CoreErlang Separate Extraction
  Counter.init Counter.handle_cast Counter.handle_call Counter.handle_info
  Toggle.init Toggle.handle_cast.
