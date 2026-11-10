(* Extractable Erlang-facing datatypes.                              *)
(*                                                                    *)
(* Types whose native Erlang counterparts the extractor knows about:  *)
(* the constructors are registered there and lowered accordingly.     *)

(* isomorphic to native maps:find's result: {ok, Value} | error *)
Inductive MapFindResult {V : Type} : Type :=
| ok : V -> MapFindResult
| error : MapFindResult.
