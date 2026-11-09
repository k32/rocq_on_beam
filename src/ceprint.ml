(*-----------------------------------------------------------------------*)
(*  Phase 2: pretty-print the Core Erlang AST (Cerlang) as source text.   *)
(*                                                                       *)
(*  This phase is pure: no naming, no de Bruijn indices, no Rocq         *)
(*  knowledge — just a straightforward rendering of the AST into the     *)
(*  Core Erlang concrete syntax (as accepted by erlc).                   *)
(*----------------------------------------------------------------------- *)

open Cerlang

(* -------------------------------------------------------------------- *)
(* Atom naming                                                          *)
(* -------------------------------------------------------------------- *)

(* Only the quote character needs escaping; the empty atom '' is valid
   Erlang. *)
let escape_atom s =
  let buf = Buffer.create (String.length s + 5) in
  String.iter
    (fun c ->
      if c = '\'' then Buffer.add_string buf "\\'"
      else Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Render an atom as Core Erlang source: quoted, with the common
   [rocq_] prefix prepended and any quotes escaped. This is the single
   place where atoms are quoted, so every atom in the generated code
   automatically carries the prefix. *)
let unatom (Cerlang.Atom s) = "'rocq_" ^ escape_atom s ^ "'"

(* Render a variable: variables are created pre-sanitized by
   [Cetrans.fresh_var], so rendering is just unwrapping. *)
let unvar (Cerlang.Var s) = s

(* Note: in this Core Erlang dialect (OTP 26+), anonymous functions have
   no [end] keyword, [fun] bodies extend as far as possible, and case
   clauses are juxtaposed without a separator but must carry a guard. *)

let string_of_fun_ref (f : atom) arity = unatom f ^ "/" ^ string_of_int arity

let rec string_of_expr e =
  match e with
  | EVar v -> unvar v
  | EFunRef (f, n) -> string_of_fun_ref f n
  | EInt s -> s
  | EFloat s -> s
  | EString s -> "\"" ^ String.escaped s ^ "\""
  | EAtom a -> unatom a
  | ETuple l -> "{" ^ String.concat "," (List.map string_of_expr l) ^ "}"
  | ECall (m, f, args) ->
    "call " ^ unatom m ^ ":" ^ unatom f ^ " ("
    ^ String.concat "," (List.map string_of_expr args) ^ ")"
  | EApply (h, args) ->
    "apply " ^ string_of_expr h ^ " ("
    ^ String.concat "," (List.map string_of_expr args) ^ ")"
  | EFun (params, body) ->
    "fun (" ^ String.concat "," (List.map unvar params) ^ ") -> " ^ string_of_expr body
  | ELet (v, e1, e2) ->
    "let " ^ unvar v ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | ELetrec (bindings, body) ->
    let b = List.map string_of_letrec_binding bindings in
    "letrec " ^ String.concat " " b ^ " in " ^ string_of_expr body
  | ECase (t, branches) ->
    let br (p, e) =
      "<" ^ string_of_pattern p ^ "> when 'true' -> " ^ string_of_expr e
    in
    "case " ^ string_of_expr t ^ " of "
    ^ String.concat " " (List.map br branches) ^ " end"

and string_of_letrec_binding b =
  unatom b.lr_name ^ "/" ^ string_of_int (List.length b.lr_params)
  ^ " = fun (" ^ String.concat "," (List.map unvar b.lr_params) ^ ") -> "
  ^ string_of_expr b.lr_body

and string_of_pattern p =
  match p with
  | PWild -> "_"
  | PVar v -> unvar v
  | PAtom a -> unatom a
  | PTuple l -> "{" ^ String.concat "," (List.map string_of_pattern l) ^ "}"

let string_of_fundef fd =
  unatom fd.fun_name ^ "/" ^ string_of_int (List.length fd.fun_params)
  ^ " =\n    fun (" ^ String.concat "," (List.map unvar fd.fun_params) ^ ") ->\n        "
  ^ string_of_expr fd.fun_body ^ "\n\n"

let string_of_module m =
  let buf = Buffer.create 4096 in
  let exported = List.filter (fun fd -> fd.fun_exported) m.mod_funcs in
  Buffer.add_string buf ("module " ^ unatom m.mod_name ^ " [");
  List.iteri (fun i fd ->
      if i > 0 then Buffer.add_string buf ",";
      Buffer.add_string buf
        (string_of_fun_ref fd.fun_name (List.length fd.fun_params)))
    exported;
  Buffer.add_string buf "] attributes []\n";
  List.iter (fun fd -> Buffer.add_string buf (string_of_fundef fd)) m.mod_funcs;
  Buffer.add_string buf "end\n";
  Buffer.contents buf
