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

(** Storage for keyed by module path and (optionally) structure
    name *)
module EStruct =
struct
  type t = ModPath.t * Id.t option

  let compare (m1, i1) (m2, i2) =
    match ModPath.compare m1 m2 with
    | 0 ->
       (match i1, i2 with
        | None, None -> 0
        | Some _, None -> 1
        | None, Some _ -> -1
        | Some i1, Some i2 -> Id.compare i1 i2)
    | v ->
       v
end

module EStructMap = Map.Make(EStruct)

type e_mod_map = e_module EStructMap.t

(** "Global" context used for recursive extraction *)

type gctx = {
    state : State.t;
    top_arity : int KerName.Map.t;
    modules : e_mod_map;
}

let erl_mod_name mod_path = function
  | None -> Atom (ModPath.to_string mod_path)
  | Some id -> Atom (ModPath.to_string mod_path ^ "." ^ Id.to_string id)

let push_defn mod_path strct_id exported (defn : e_defn) ctx =
  let key = mod_path, strct_id in
  let exp () = defn.name, List.length defn.args in
  let m = (match EStructMap.find_opt key ctx.modules with
           | Some m ->
              {m with
                exports = if exported then
                            exp () :: m.exports
                          else
                            m.exports;
                locals = defn :: m.locals
              }
           | None ->
              { name = erl_mod_name mod_path strct_id;
                exports = if exported then
                            [exp ()]
                          else
                            [];
                locals = [defn]
          }) in
  {ctx with modules = EStructMap.add key m ctx.modules}

let fold_modules f a {modules = m} = EStructMap.fold f m a

let fold_lefti f acc a =
  let acc = ref acc in
  Array.iteri (fun i x -> acc := f i x !acc) a;
  !acc
