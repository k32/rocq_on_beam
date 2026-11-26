open Extraction_plugin
open Common
open Names
open Erlang_ast

(** Storage for local Erlang variables *)

module EVar =
struct
  type t = var

  let compare (Var (n1, i1)) (Var (n2, i2)) =
    match compare i1 i2 with
    | 0 ->
       String.compare n1 n2
    | v ->
       v
end

module EVarMap = Map.Make(EVar)

type e_mod_map = e_module ModPath.Map.t

(** "Global" context used for recursive extraction *)

type gctx = {
    state : State.t;
    top_arity : int KerName.Map.t;
    modules : e_mod_map;
}

let new_gctx state =
  { state = state;
    top_arity = KerName.Map.empty;
    modules = ModPath.Map.empty
  }

let erl_mod_name mod_path = Atom (ModPath.to_string mod_path)

let push_defn mod_path exported (defn : e_defn) ctx =
  let exp () = defn.name, List.length defn.args in
  let m = (match ModPath.Map.find_opt mod_path ctx.modules with
           | Some m ->
              {m with
                exports = if exported then
                            exp () :: m.exports
                          else
                            m.exports;
                locals = defn :: m.locals
              }
           | None ->
              { name = erl_mod_name mod_path;
                exports = if exported then
                            [exp ()]
                          else
                            [];
                locals = [defn]
          }) in
  {ctx with modules = ModPath.Map.add mod_path m ctx.modules}

let fold_modules f a {modules = m} = ModPath.Map.fold f m a

let fold_lefti f acc a =
  let acc = ref acc in
  Array.iteri (fun i x -> acc := f i x !acc) a;
  !acc
