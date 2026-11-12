Inductive init_res {State ExitReason} :=
| i_ok : State -> init_res
| i_stop : ExitReason -> init_res
| i_ignore.

Inductive handle_cast_res {State ExitReason} :=
| cst_noreply : State -> handle_cast_res
| cst_stop : ExitReason -> State -> handle_cast_res.

Inductive call_res {Reply State ExitReason} :=
| c_reply : Reply -> State -> call_res
| c_noreply : State -> call_res
| c_stop_reply : ExitReason -> Reply -> State -> call_res
| c_stop : ExitReason -> State -> call_res.

Inductive info_res {State ExitReason} :=
| inf_noreply : State -> info_res
| inf_stop : ExitReason -> State -> info_res.

Module Type gen_server.
  Parameter InitArg : Set.

  Parameter ExitReason : Set.

  Parameter State : Set.

  Parameter Cast : Set.

  Parameter Call : Set.

  Parameter Reply : Set.

  (* the gen_server From handle: opaque, values only pass through *)
  Parameter From : Set.

  Parameter Info : Set.

  Parameter init : InitArg -> @init_res State ExitReason.

  Parameter handle_cast : Cast -> State -> @handle_cast_res State ExitReason.

  Parameter handle_call
    : Call -> From -> State -> @call_res Reply State ExitReason.

  Parameter handle_info : Info -> State -> @info_res State ExitReason.
End gen_server.
