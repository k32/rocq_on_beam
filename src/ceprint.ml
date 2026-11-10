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

(* Render an atom as Core Erlang source: quoted, with any quotes
   escaped. This is the single place where atoms are quoted. *)
let unatom (Cerlang.Atom s) = "'" ^ escape_atom s ^ "'"

(* Render a variable: variables are unique integers created by
   [Cetrans.fresh_var]; the concrete name is derived here. *)
let unvar (Cerlang.Var n) = "_v" ^ string_of_int n

(* Note: in this Core Erlang dialect (OTP 26+), anonymous functions have
   no [end] keyword, [fun] bodies extend as far as possible, and case
   clauses are juxtaposed without a separator but must carry a guard. *)

let string_of_fun_ref (f : atom) arity = unatom f ^ "/" ^ string_of_int arity

let rec string_of_expr indent e =
  let nl = "\n" ^ String.make indent ' ' in
  match e with
  | EVar v -> unvar v
  | EFunRef (f, n) -> string_of_fun_ref f n
  | EInt s -> s
  | EFloat s -> s
  | EString s -> "\"" ^ String.escaped s ^ "\""
  | EAtom a -> unatom a
  | ENil -> "[]"
  | ECons (h, t) -> "[" ^ string_of_expr indent h ^ "|" ^ string_of_expr indent t ^ "]"
  | ETuple l -> "{" ^ String.concat "," (List.map (string_of_expr indent) l) ^ "}"
  | EMap assocs ->
    "~{" ^ String.concat ","
      (List.map (fun (k, v) -> string_of_expr indent k ^ "=>" ^ string_of_expr indent v) assocs)
    ^ "}~"
  | EBinary s ->
    (* one integer segment per byte; all four segment arguments are
       mandatory (shorter forms crash erlc's parser) *)
    let segs =
      String.to_seq s
      |> Seq.map (fun c ->
             "#<" ^ string_of_int (Char.code c)
             ^ ">(8,1,'integer',['unsigned'|['big']])")
      |> List.of_seq |> String.concat ","
    in
    "#{" ^ segs ^ "}#"
  | ECall (m, f, args) ->
    "call " ^ unatom m ^ ":" ^ unatom f ^ " ("
    ^ String.concat "," (List.map (string_of_expr indent) args) ^ ")"
  | EApply (h, args) ->
    "apply " ^ string_of_expr indent h ^ " ("
    ^ String.concat "," (List.map (string_of_expr indent) args) ^ ")"
  | EFun (params, body) ->
    "fun (" ^ String.concat "," (List.map unvar params) ^ ") ->" ^ nl ^ string_of_expr (indent + 1) body ^ nl
  | ELet (v, e1, e2) ->
    "let " ^ unvar v ^ " = " ^ string_of_expr indent e1 ^ " in" ^ nl ^ string_of_expr (indent + 1) e2 ^ nl
  | ELetrec (bindings, body) ->
    let b = List.map (string_of_letrec_binding indent) bindings in
    "letrec " ^ String.concat " " b ^ " in " ^ string_of_expr indent body
  | ECase (t, branches) ->
    let br (p, g, e) =
      "<" ^ string_of_pattern p ^ "> when " ^ string_of_expr (indent + 1) g
      ^ " -> " ^ string_of_expr (indent + 1) e
    in
    "case " ^ string_of_expr indent t ^ " of" ^ nl
    ^ String.concat " " (List.map br branches)
    ^ nl ^ "end"
  | EPrimop (p, args) ->
    "primop " ^ unatom p ^ " ("
    ^ String.concat "," (List.map (string_of_expr indent) args) ^ ")"

and string_of_letrec_binding indent b =
  unatom b.lr_name ^ "/" ^ string_of_int (List.length b.lr_params)
  ^ " = fun (" ^ String.concat "," (List.map unvar b.lr_params) ^ ") -> "
  ^ string_of_expr indent b.lr_body

and string_of_pattern p =
  match p with
  | PWild -> "_"
  | PVar v -> unvar v
  | PAtom a -> unatom a
  | PNil -> "[]"
  | PCons (h, t) -> "[" ^ string_of_pattern h ^ "|" ^ string_of_pattern t ^ "]"
  | PInt i -> i
  | PTuple l -> "{" ^ String.concat "," (List.map string_of_pattern l) ^ "}"

let string_of_fundef fd =
  unatom fd.fun_name ^ "/" ^ string_of_int (List.length fd.fun_params)
  ^ " =\n  fun (" ^ String.concat "," (List.map unvar fd.fun_params) ^ ") ->\n        "
  ^ string_of_expr 5 fd.fun_body ^ "\n\n"

let string_of_module m =
  let buf = Buffer.create 4096 in
  let exported = List.filter (fun fd -> fd.fun_exported) m.mod_funcs in
  Buffer.add_string buf ("module " ^ unatom m.mod_name ^ " [");
  List.iteri (fun i fd ->
      if i > 0 then Buffer.add_string buf ",";
      Buffer.add_string buf
        (string_of_fun_ref fd.fun_name (List.length fd.fun_params)))
    exported;
  Buffer.add_string buf "] attributes []\n\n";
  List.iter (fun fd -> Buffer.add_string buf (string_of_fundef fd)) m.mod_funcs;
  Buffer.add_string buf "end\n";
  Buffer.contents buf
