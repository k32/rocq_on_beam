Class FromT {from_t : Set} :=
  {}.

Class GenServerT :=
  { init_arg_t : Set;
    state_t : Set;
    exit_reason_t : Set;

    call_t : Set;
    cast_t : Set;
    info_t : Set;

    reply_t : call_t -> Set;
  }.

Inductive init_res `{GenServerT} :=
| i_ok : state_t -> init_res
| i_stop : exit_reason_t -> init_res
| i_ignore.

Inductive handle_cast_res `{GenServerT} :=
| cst_noreply : state_t -> handle_cast_res
| cst_stop : exit_reason_t -> state_t -> handle_cast_res.

Inductive call_res `{GenServerT} (call : call_t) :=
| c_reply : reply_t call -> state_t -> call_res call
| c_stop_reply : exit_reason_t -> reply_t call -> state_t -> call_res call.

Inductive info_res `{GenServerT} :=
| inf_noreply : state_t -> info_res
| inf_stop : exit_reason_t -> state_t -> info_res.

Module Type gen_server.
  Parameter t : GenServerT.

  Parameter init : @init_arg_t t -> @init_res t.

  Parameter handle_cast : @cast_t t -> @state_t t -> @handle_cast_res t.

  Parameter handle_call : forall `{FromT} (call : @call_t t), from_t -> @state_t t -> @call_res t call.

  Parameter handle_info : @info_t t -> @state_t t -> @info_res t.
End gen_server.

Module Type gen_server_invariant (m : gen_server).
  Import m.

  Parameter prop : (@state_t t -> Prop).

  Parameter init_valid : forall (arg : @init_arg_t t),
      match init arg with
      | i_ok s => prop s
      | _ => True
      end.

  Parameter handle_cast_valid : forall s (cast : @cast_t t),
      prop s ->
      match handle_cast cast s with
      | cst_noreply s' => prop s'
      | cst_stop _ s' => prop s'
      end.

  Parameter handle_call_valid : forall `{FromT} s (call : @call_t t) (from : from_t),
      prop s ->
      match handle_call call from s with
      | c_reply _ _ s' => prop s'
      | c_stop_reply _ _ _ s' => prop s'
      end.

  Parameter handle_info_valid : forall s (inf : @info_t t),
      prop s ->
      match handle_info inf s with
      | inf_noreply s' => prop s'
      | inf_stop _ s' => prop s'
      end.
End gen_server_invariant.
