open Extraction_plugin
open Names
open Erlang_ast

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

type gctx = {
    table : Table.t;
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

let fold_modules f a {modules = m} = EStructMap.fold f a m
