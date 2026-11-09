(*-----------------------------------------------------------------------*)
(*  Minimal extraction of Rocq terms to Core Erlang.                     *)
(*                                                                       *)
(*  Pipeline (two phases):                                               *)
(*    1. [Cetrans] : Rocq's MiniML (from the standard extraction         *)
(*       internals) -> Core Erlang AST ([Cerlang])                       *)
(*    2. [Ceprint] : Core Erlang AST -> concrete syntax accepted by erlc *)
(*                                                                       *)
(*  This module only drives Rocq: locate references, run the extraction  *)
(*  environment, and print / write the result.                           *)
(*----------------------------------------------------------------------- *)

open Names
module Miniml = Extraction_plugin.Miniml
module Common = Extraction_plugin.Common
module Extract_env = Extraction_plugin.Extract_env
module Modutil = Extraction_plugin.Modutil
module Table = Extraction_plugin.Table
open Miniml

(* -------------------------------------------------------------------- *)
(* Reference location (like `locate_ref` in Extract_env, constants only) *)
(* -------------------------------------------------------------------- *)

let locate_ref qid =
  let gr = Smartlocate.global_with_alias qid in
  let inst = Environ.universes_of_global (Global.env ()) gr in
  List.map (fun inst -> { glob = gr; inst }) (InfvInst.generate inst)

(* -------------------------------------------------------------------- *)
(* Extraction                                                           *)
(* -------------------------------------------------------------------- *)

let extract_structure ~opaque_access refs =
  let state =
    Common.State.make ~modular:false ~library:false ~keywords:Id.Set.empty () in
  let struc =
    Modutil.optimize_struct state (refs, [])
      (Extract_env.mono_environment state ~opaque_access refs [])
  in
  Table.warning_axioms (Common.State.get_table state);
  struc

let translate ~modname struc =
  Ceprint.string_of_module (Cetrans.translate_structure ~modname struc)

let modname_of_qualid qid =
  match locate_ref qid with
  | r :: _ -> Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r))
  | [] -> Cerlang.Atom "extracted"

(* -------------------------------------------------------------------- *)
(* The vernacular entry points                                          *)
(* -------------------------------------------------------------------- *)

let coreerlang_extraction ~opaque_access x =
  let refs = locate_ref x in
  let struc = extract_structure ~opaque_access refs in
  Feedback.msg_notice Pp.(str (translate ~modname:(modname_of_qualid x) struc))

let coreerlang_extraction_file ~opaque_access f l =
  let refs = List.concat_map locate_ref l in
  let struc = extract_structure ~opaque_access refs in
  (* module name gets the common rocq_ prefix; the output file is named
     after the module so that erlc accepts it *)
  let base = Filename.remove_extension (Filename.basename f) in
  let modname = Cerlang.Atom base in
  let dir = Filename.dirname f in
  (* the file is named after the module, with the atom escaped so the
     filename is filesystem-safe *)
  let file =
    (if dir = "." then "" else dir ^ "/")
    ^ "rocq_" ^ Ceprint.escape_atom base ^ ".core"
  in
  let oc = open_out file in
  output_string oc (translate ~modname struc);
  close_out oc;
  Feedback.msg_info Pp.(str ("Core Erlang extraction written to " ^ file))
