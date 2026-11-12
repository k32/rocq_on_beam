(*-----------------------------------------------------------------------*)
(*  Module and structure translation.                                     *)
(*                                                                       *)
(*  This layer owns what the output modules are: it walks the MiniML    *)
(*  structure, turns declarations into function definitions, keeps the  *)
(*  toplevel function map used for call resolution, and assembles the  *)
(*  Cerlang module(s). Term translation (de Bruijn resolution, eta      *)
(*  handling, constructors, fixpoints) stays in [cetrans], which this   *)
(*  module drives; native decisions remain in [cenative].               *)
(*-----------------------------------------------------------------------*)

open Names
module Miniml = Extraction_plugin.Miniml
module Mlutil = Extraction_plugin.Mlutil
module Table = Extraction_plugin.Table
open Miniml

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

let fundefs_of_decl exported d =
  let is_exported r = KerName.Set.mem (Table.repr_of_r r) exported in
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

(* Turn a MiniML structure into Cerlang modules (one for now; the
   nested-modules work will grow this into one module per Rocq
   module, each named by its ModPath):
   1. build the toplevel function map (for call resolution);
   2. translate each declaration's body under its group binders.
   [exported] is the set of kernel names marked [export]; unmarked
   functions are extracted (they may be needed by the exported ones)
   but stay internal, i.e. absent from the module export list. *)
let translate_structure ~(modname : Cerlang.atom)
    ?(exported = KerName.Set.empty) struc : Cerlang.module_ list =
  let funs = ref KerName.Map.empty in
  let add r arity =
    funs := KerName.Map.add (Table.repr_of_r r) (Cetrans.global_name r, arity) !funs
  in
  let decls =
    List.concat_map
      (fun (_, elems) ->
        List.filter_map
          (fun (_, elem) -> match elem with SEdecl d -> Some d | _ -> None)
          elems)
      struc
  in
  (* pass 1: initial arities (from the leading lambdas) *)
  List.iter
    (fun d ->
      let skip =
        match d with
        | Dterm (r, _, _) -> Cenative.is_erlangbif r
        | Dfix (rv, _, _) -> Array.exists Cenative.is_erlangbif rv
        | _ -> false
      in
      if not skip then
        match d with
        | Dterm (r, body, _) -> add r (Cetrans.count_lams body)
        | Dfix (rv, defs, _) ->
            Array.iteri
              (fun j r ->
                add r
                  (match defs.(j) with
                  | MLfix (_, _, inner) -> Cetrans.count_lams inner.(j)
                  | b -> Cetrans.count_lams b))
              rv
        | _ -> ())
    decls;
  (* pass 2: translate in dependency order (the structure is
     topologically sorted), promoting eta-expanded bodies and keeping
     the arity map in sync for the definitions that follow. *)
  let funcs =
    List.concat_map (fundefs_of_decl exported) decls
    |> List.filter (fun fd -> not (Cenative.is_erlangbif_kn fd.fd_kn))
    |> List.map (fun fd ->
        (match fd.fd_name with Cerlang.Atom n -> Cetrans.current_function := n);
        let ctx = { Cetrans.env = fd.fd_group; funs = !funs; counter = ref 0 } in
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
        funs := KerName.Map.add fd.fd_kn (fd.fd_name, List.length params) !funs;
        {
          Cerlang.fun_name = fd.fd_name;
          fun_params = List.rev params;
          fun_body = body;
          fun_exported = fd.fd_exported;
        })
  in
  [ { Cerlang.mod_name = modname; mod_funcs = funcs } ]
