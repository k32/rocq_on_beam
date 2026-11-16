(*-----------------------------------------------------------------------
  Minimal extraction of Rocq terms to Core Erlang.

  Pipeline (two phases):
    1. [Cetrans] : Rocq's MiniML (from the standard extraction
       internals) -> Core Erlang AST ([Cerlang])
    2. [Ceprint] : Core Erlang AST -> concrete syntax accepted by erlc

  This module only drives Rocq: locate references, run the extraction
  environment, and print / write the result.
-----------------------------------------------------------------------*)

open Names
module Miniml = Extraction_plugin.Miniml
module Common = Extraction_plugin.Common
module Extract_env = Extraction_plugin.Extract_env
module Modutil = Extraction_plugin.Modutil
module Table = Extraction_plugin.Table
open Miniml

(*---------------------------------------------------------------------
 Reference location (like `locate_ref` in Extract_env, constants only)
---------------------------------------------------------------------*)

let locate_ref qid =
  let gr = Smartlocate.global_with_alias qid in
  let inst = Environ.universes_of_global (Global.env ()) gr in
  List.map (fun inst -> { glob = gr; inst }) (InfvInst.generate inst)

(*--------------------------------------------------------------------
 Extraction
--------------------------------------------------------------------*)

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

(*--------------------------------------------------------------------
 Native call declaration
--------------------------------------------------------------------*)

(* Parse "module:function/arity", the Erlang side of a native call
   declaration; [None] when malformed *)
let parse_native_target s =
  match (String.index_opt s ':', String.rindex_opt s '/') with
  | Some i, Some j ->
      if i > 0 && j > i + 1 then
        let m = String.sub s 0 i in
        let f = String.sub s (i + 1) (j - i - 1) in
        let a = String.sub s (j + 1) (String.length s - j - 1) in
        match int_of_string_opt a with
        | Some arity when arity >= 0 ->
            Some (Cerlang.Atom m, Cerlang.Atom f, arity)
        | _ -> None
      else None
  | _ -> None

(* Erlang Native Call r := "m:f/a" drop: applications of the Rocq
   definition [r] extract as calls to the Erlang function [m:f/a]; the
   [drop] leading Rocq arguments are dropped untranslated (e.g. a
   typeclass dictionary), usually 0. The declaration travels with the
   enclosing module: requiring it replays the entry. *)
let erlang_native_call r s drop =
  let loc = r.CAst.loc in
  match parse_native_target s with
  | None ->
      CErrors.user_err ?loc
        Pp.(
          str "Erlang extraction: invalid native call target "
          ++ qs s
          ++ str ": expected \"module:function/arity\"")
  | Some (m, f, arity) -> (
      if drop < 0 then
        CErrors.user_err ?loc
          Pp.(
            str
              "Erlang extraction: the number of dropped arguments must not be \
               negative")
      else
        match Smartlocate.global_with_alias r with
        | GlobRef.ConstRef _ as gr ->
            Cenative.declare_native_call gr m f arity drop
        | (IndRef _ | ConstructRef _ | VarRef _) as gr ->
            CErrors.user_err ?loc
              Pp.(
                str "Erlang extraction: the native call target of "
                ++ Nametab.pr_global_env Id.Set.empty gr
                ++ str " must be a definition (constant)"))

(*--------------------------------------------------------------------
 The vernacular entry points
--------------------------------------------------------------------*)

let erlang_extraction ~opaque_access (x, exported) =
  let refs = locate_ref x in
  let struc = extract_structure ~opaque_access refs in
  let exported =
    if exported then
      List.fold_left
        (fun s r -> KerName.Set.add (Table.repr_of_r r) s)
        KerName.Set.empty refs
    else KerName.Set.empty
  in
  match translate ~modname:(modname_of_qualid x) ~exported struc with
  | s :: _ -> Feedback.msg_notice Pp.(str s)
  | [] ->
      Feedback.msg_notice Pp.(str "Core Erlang extraction: nothing to extract")

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
    ([], KerName.Set.empty, KerName.Set.empty)
    l

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
  | [] ->
      Feedback.msg_info Pp.(str "Core Erlang extraction: nothing to extract")
  | mods ->
      List.iter
        (fun m ->
          let file = file_of m.Cerlang.mod_name in
          dump_core_file file (Ceprint.string_of_module m))
        mods

let erlang_extraction_file ~opaque_access f l =
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
  match translate ~modname ~exported struc with
  | contents :: _ -> dump_core_file file contents
  | [] -> ()

(* Separate extraction: no output-file argument; each module is named
   after the Rocq module of the extracted references (dotted), and the
   .core files are written to rocq's cwd. Cross-module calls force the
   callees into the export lists, so everything not explicitly marked
   [noexport(...)] is exported. *)
let erlang_extraction_separate ~opaque_access l =
  let refs, exported, internal = refs_and_exported l in
  let modname =
    match refs with
    | r :: _ -> Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r))
    | [] -> Cerlang.Atom "extracted"
  in
  let struc = extract_structure ~opaque_access refs in
  let file_of (Cerlang.Atom n) = Ceprint.escape_atom n ^ ".erl" in
  write_core_modules ~modname ~exported ~internal ~mode:Cemodule.Separate
    ~file_of struc
