(*-----------------------------------------------------------------------*)
(*  Module and structure translation.                                     *)
(*                                                                       *)
(*  This layer owns what the output modules are: it walks the MiniML    *)
(*  structure, turns declarations into function definitions, keeps the  *)
(*  toplevel function map used for call resolution, and assembles the  *)
(*  Cerlang module(s). Term translation (de Bruijn resolution, eta      *)
(*  handling, constructors, fixpoints) stays in [cetrans], which this   *)
(*  module drives; native decisions remain in [cenative].               *)
(*                                                                       *)
(*  Two modes:                                                           *)
(*  - Monolithic: every declaration lands in one module, named by the   *)
(*    caller (the [CoreErlang Extraction "file" ...] command).          *)
(*  - Separate: one module per Rocq module, named by its ModPath        *)
(*    (dotted, e.g. 'foo.Inner'); submodules recurse into [MEstruct],   *)
(*    aliases ([MEident]) emit nothing, module types are signature-only *)
(*    and skipped, residual functors are an error.                      *)
(*-----------------------------------------------------------------------*)

open Names
module Miniml = Extraction_plugin.Miniml
module Mlutil = Extraction_plugin.Mlutil
module Table = Extraction_plugin.Table
open Miniml

type mode = Monolithic | Separate

(* One toplevel function in the making: its lambda identifiers
   (outermost-first), the body under the lambdas, the initial
   binder environment (fixpoint group, if any), and whether it is
   exported (listed in the module exports) or internal. *)
type fdef = {
  fd_kn : KerName.t; (* for the call-resolution map *)
  fd_name : Cerlang.atom;
  fd_ids : ml_ident list; (* outermost-first *)
  fd_body : ml_ast; (* body under the lambdas and the group *)
  fd_group : Cetrans.binder list; (* innermost-first, for MLfix groups *)
  fd_exported : bool;
}

(* -------------------------------------------------------------------- *)
(* Grouping declarations per Rocq module                                 *)
(* -------------------------------------------------------------------- *)

(* A module under construction: its (dotted) name and its declarations,
   dependency-ordered *)
type module_group = { mg_name : Cerlang.atom; mg_decls : ml_decl list }

let decls_of_elems elems =
  List.filter_map
    (fun (_, elem) -> match elem with SEdecl d -> Some d | _ -> None)
    elems

(* [Separate] grouping: the toplevel structure pairs are one group per
   pair (their ModPath), submodules recurse. This module's own decls
   come first, then the submodules'. *)
let rec group_of_module mp elems =
  let decls = ref [] in
  let subs = ref [] in
  List.iter
    (fun (_, elem) ->
      match elem with
      | SEdecl d -> decls := d :: !decls
      | SEmodule m -> (
          match m.ml_mod_expr with
          | MEstruct (mp', elems') ->
              subs := group_of_module mp' elems' :: !subs
          | MEident _ ->
              (* an alias: emits nothing; references to its members
                 resolve to the target module via the kernel name *)
              ()
          | MEfunctor _ | MEapply _ ->
              CErrors.user_err
                Pp.(
                  str
                    "CoreErlang extraction: functor modules are not \
                     extractable (module-level functions do not exist in \
                     Core Erlang)"
                  ++ str " while extracting module "
                  ++ str (ModPath.to_string mp)))
      | SEmodtype _ -> (* signature-only *) ())
    elems;
  { mg_name = Cerlang.Atom (ModPath.to_string mp);
    mg_decls = List.rev !decls
  }
  :: List.concat (List.rev !subs)

(* -------------------------------------------------------------------- *)
(* Declarations -> fundefs                                               *)
(* -------------------------------------------------------------------- *)

let fundefs_of_decl is_exported d =
  match d with
  | Dind _ | Dtype _ -> []
  | Dterm (r, body, _) ->
      Cetrans.check_custom r (Pp.str "constant");
      let ids, body' = Mlutil.collect_lams body in
      [
        {
          fd_kn = Table.repr_of_r r;
          fd_name = Cetrans.global_name r;
          fd_ids = List.rev ids;
          fd_body = body';
          fd_group = [];
          fd_exported = is_exported r;
        };
      ]
  | Dfix (rv, defs, _) ->
      let k = Array.length rv in
      let arity j =
        match defs.(j) with
        | MLfix (_, _, inner) -> Cetrans.count_lams inner.(j)
        | b -> Cetrans.count_lams b
      in
      (* binder list for the group: [rv.(k-1);...;rv.(0)], i.e. the same
         order as [rv] reversed; each entry points to the toplevel name *)
      let group =
        List.init k (fun m ->
            Cetrans.BFun (Cetrans.global_name rv.(k - 1 - m), arity (k - 1 - m)))
      in
      List.init k (fun j ->
          let idsl, body' =
            match defs.(j) with
            | MLfix (_, _, inner) -> Mlutil.collect_lams inner.(j)
            | b -> Mlutil.collect_lams b
          in
          Cetrans.check_custom rv.(j) (Pp.str "fixpoint");
          {
            fd_kn = Table.repr_of_r rv.(j);
            fd_name = Cetrans.global_name rv.(j);
            fd_ids = List.rev idsl;
            fd_body = body';
            fd_group = group;
            fd_exported = is_exported rv.(j);
          })

(* -------------------------------------------------------------------- *)
(* Structure -> Cerlang modules                                          *)
(* -------------------------------------------------------------------- *)

let translate_funs ?(exported = KerName.Set.empty)
    ?(internal = KerName.Set.empty) ~mode:(m : mode) ~funs mgroup =
  let is_exported =
    match m with
    | Monolithic -> fun r -> KerName.Set.mem (Table.repr_of_r r) exported
    | Separate ->
        (* cross-module calls need exported callees; only explicitly
           internal ([noexport]) references stay out *)
        fun r -> not (KerName.Set.mem (Table.repr_of_r r) internal)
  in
  let { mg_name = m; mg_decls = decls } = mgroup in
  let funcs =
    List.concat_map (fundefs_of_decl is_exported) decls
    |> List.filter (fun fd -> not (Cenative.is_erlangbif_kn fd.fd_kn))
    |> List.map (fun fd ->
        (match fd.fd_name with Cerlang.Atom n -> Cetrans.current_function := n);
        let ctx =
          { Cetrans.env = fd.fd_group; funs = !funs; cur_module = m;
            counter = ref 0
          }
        in
        let ctx, params =
          List.fold_left
            (fun (ctx, params) id ->
              let v = Cetrans.fresh_var ctx in
              (Cetrans.push_binder (Cetrans.BVar v) ctx, v :: params))
            (ctx, []) fd.fd_ids
        in
        let body = Cetrans.trans_expr ctx fd.fd_body in
        (* Eta promotion: a toplevel constant of function type (e.g.
           [Definition add12 := add3 1 2]) translates to an eta-expanded
           [EFun] wrapping a partial application. Promote its parameters
           to the fundef's own, so the exported arity matches the
           function's logical arity instead of a 0-arity closure box.
           Not applied to fixpoint group members: their group binders
           were computed from the leading lambdas. *)
        let params, body =
          match (fd.fd_group, body) with
          | [], Cerlang.EFun (ps, b) -> (params @ List.rev ps, b)
          | _ -> (params, body)
        in
        (* later declarations must see the promoted arity *)
        funs := KerName.Map.add fd.fd_kn (m, fd.fd_name, List.length params) !funs;
        {
          Cerlang.fun_name = fd.fd_name;
          fun_params = List.rev params;
          fun_body = body;
          fun_exported = fd.fd_exported;
        })
  in
  { Cerlang.mod_name = m; mod_funcs = funcs }

(* Turn a MiniML structure into Cerlang modules.
   1. group the declarations per module;
   2. build the global function map (module, name, arity) used for
      call resolution -- cross-module references become remote calls;
   3. translate each group's declarations in dependency order (the
      structure is topologically sorted), promoting eta-expanded
      bodies and keeping the arity map in sync.
   [exported] is the set of kernel names marked [export]; unmarked
   functions are extracted (they may be needed by the exported ones)
   but stay internal, i.e. absent from the module export list. *)
let translate_structure ~(modname : Cerlang.atom)
    ?(exported = KerName.Set.empty) ?(internal = KerName.Set.empty)
    ?(mode = Monolithic) struc :
    Cerlang.module_ list =
  let groups =
    match mode with
    | Monolithic ->
        [ { mg_name = modname;
            mg_decls = List.concat_map (fun (_, elems) -> decls_of_elems elems) struc
          } ]
    | Separate ->
        List.concat_map
          (fun (mp, elems) -> group_of_module mp elems)
          struc
  in
  (* pass 1: initial arities (from the leading lambdas), all modules *)
  let funs = ref KerName.Map.empty in
  List.iter
    (fun { mg_name = m; mg_decls = decls } ->
      List.iter
        (fun d ->
          match d with
          | Dterm (r, body, _) when not (Cenative.is_erlangbif r) ->
              funs :=
                KerName.Map.add (Table.repr_of_r r)
                  (m, Cetrans.global_name r, Cetrans.count_lams body)
                  !funs
          | Dfix (rv, defs, _)
            when not (Array.exists Cenative.is_erlangbif rv) ->
              Array.iteri
                (fun j r ->
                  let a =
                    match defs.(j) with
                    | MLfix (_, _, inner) -> Cetrans.count_lams inner.(j)
                    | b -> Cetrans.count_lams b
                  in
                  funs :=
                    KerName.Map.add (Table.repr_of_r r)
                      (m, Cetrans.global_name r, a)
                      !funs)
                rv
          | _ -> ())
        decls)
    groups;
  (* pass 2: translate, module by module *)
  List.map (translate_funs ~exported ~internal ~mode ~funs) groups
