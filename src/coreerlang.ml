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
  List.map Ceprint.string_of_module
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
  (match translate ~modname:(modname_of_qualid x) ~exported struc with
   | s :: _ -> Feedback.msg_notice Pp.(str s)
   | [] -> Feedback.msg_notice Pp.(str "Core Erlang extraction: nothing to extract"))

(* Collect (refs, exported kernel names, explicitly internal kernel
   names) from a reference list: [noexport(...)] marks internal *)
let refs_and_exported l =
  List.fold_left
    (fun (refs, exported, internal) (x, export) ->
      let rs = locate_ref x in
      let exported', internal' =
        if export then
          ( List.fold_left
              (fun acc r -> KerName.Set.add (Table.repr_of_r r) acc)
              exported rs,
            internal )
        else
          ( exported,
            List.fold_left
              (fun acc r -> KerName.Set.add (Table.repr_of_r r) acc)
              internal rs )
      in
      (refs @ rs, exported', internal'))
    ([], KerName.Set.empty, KerName.Set.empty) l

(* Write one rendered module to [file]. Translate first, then write:
   on error, no (truncated) output file is left *)
let dump_core_file file contents =
  let oc = open_out file in
  output_string oc contents;
  close_out oc;
  Feedback.msg_info Pp.(str ("Core Erlang extraction written to " ^ file))

(* Write every non-empty module of the structure, one .core file per
   module, named after the module (erlc requires name and module to
   match); empty modules (nothing extracted into them) are skipped.
   [file_of] maps a module name to its output file. *)
let write_core_modules ~modname ~exported ?(internal = KerName.Set.empty)
    ?(mode = Cemodule.Monolithic) ~file_of struc =
  let mods =
    Cemodule.translate_structure ~modname ~exported ~internal ~mode struc
  in
  match List.filter (fun m -> m.Cerlang.mod_funcs <> []) mods with
  | [] -> Feedback.msg_info Pp.(str "Core Erlang extraction: nothing to extract")
  | mods ->
      List.iter
        (fun m ->
          let file = file_of m.Cerlang.mod_name in
          dump_core_file file (Ceprint.string_of_module m))
        mods

let coreerlang_extraction_file ~opaque_access f l =
  let refs, exported, _internal = refs_and_exported l in
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
  (* monolithic: exactly one module, named after the requested file *)
  (match translate ~modname ~exported struc with
   | contents :: _ -> dump_core_file file contents
   | [] -> ())

(* Separate extraction: no output-file argument; each module is named
   after the Rocq module of the extracted references (dotted), and the
   .core files are written to rocq's cwd. Cross-module calls force the
   callees into the export lists, so everything not explicitly marked
   [noexport(...)] is exported. *)
let coreerlang_extraction_separate ~opaque_access l =
  let refs, exported, internal = refs_and_exported l in
  let modname =
    match refs with
    | r :: _ -> Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r))
    | [] -> Cerlang.Atom "extracted"
  in
  let struc = extract_structure ~opaque_access refs in
  let file_of (Cerlang.Atom n) = Ceprint.escape_atom n ^ ".core" in
  write_core_modules ~modname ~exported ~internal ~mode:Cemodule.Separate
    ~file_of struc
