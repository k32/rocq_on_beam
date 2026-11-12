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
(*-----------------------------------------------------------------------*)

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
    Common.State.make ~modular:false ~library:false ~keywords:Id.Set.empty ()
  in
  let struc =
    Modutil.optimize_struct state (refs, [])
      (Extract_env.mono_environment state ~opaque_access refs [])
  in
  Table.warning_axioms (Common.State.get_table state);
  struc

let translate ~modname ?exported struc =
  Ceprint.string_of_module
    (Cemodule.translate_structure ~modname ?exported struc)

let modname_of_qualid qid =
  match locate_ref qid with
  | r :: _ -> Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r))
  | [] -> Cerlang.Atom "extracted"

(* -------------------------------------------------------------------- *)
(* The vernacular entry points                                          *)
(* -------------------------------------------------------------------- *)

let coreerlang_extraction ~opaque_access (x, exported) =
  let refs = locate_ref x in
  let struc = extract_structure ~opaque_access refs in
  let exported =
    if exported then
      List.fold_left
        (fun s r -> KerName.Set.add (Table.repr_of_r r) s)
        KerName.Set.empty refs
    else KerName.Set.empty
  in
  Feedback.msg_notice
    Pp.(str (translate ~modname:(modname_of_qualid x) ~exported struc))

(* Collect (refs, exported kernel names) from a reference list *)
let refs_and_exported l =
  List.fold_left
    (fun (refs, exported) (x, export) ->
      let rs = locate_ref x in
      let exported' =
        if export then
          List.fold_left
            (fun acc r -> KerName.Set.add (Table.repr_of_r r) acc)
            exported rs
        else exported
      in
      (refs @ rs, exported'))
    ([], KerName.Set.empty) l

(* Translate first, then write: on error, no (truncated) output file
   is left *)
let write_core_file ~modname ~exported ~file struc =
  let contents = translate ~modname ~exported struc in
  let oc = open_out file in
  output_string oc contents;
  close_out oc;
  Feedback.msg_info Pp.(str ("Core Erlang extraction written to " ^ file))

let coreerlang_extraction_file ~opaque_access f l =
  let refs, exported = refs_and_exported l in
  let struc = extract_structure ~opaque_access refs in
  (* the file is named after the module (erlc requires name and module
     to match); [basename] keeps it a single path component and any
     quotes are escaped (though erlc would reject such names anyway) *)
  let base = Filename.remove_extension (Filename.basename f) in
  let modname = Cerlang.Atom base in
  let dir = Filename.dirname f in
  let file =
    (if dir = "." then "" else dir ^ "/") ^ Ceprint.escape_atom base ^ ".core"
  in
  write_core_file ~modname ~exported ~file struc

(* Separate extraction: no output-file argument; the module is named
   after the Rocq module of the (first) extracted reference, and the
   .core file is written to rocq's cwd. For now the whole extraction
   closure still lands in that one module; per-home-module splitting
   comes with the nested-modules work. *)
let coreerlang_extraction_separate ~opaque_access l =
  let refs, exported = refs_and_exported l in
  let modname =
    match refs with
    | r :: _ -> Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r))
    | [] -> Cerlang.Atom "extracted"
  in
  let file =
    (match modname with Cerlang.Atom n -> Ceprint.escape_atom n) ^ ".core"
  in
  let struc = extract_structure ~opaque_access refs in
  write_core_file ~modname ~exported ~file struc
