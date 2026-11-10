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
let unvar (Cerlang.Var n) = "_" ^ string_of_int n

(* Note: in this Core Erlang dialect (OTP 26+), anonymous functions have
   no [end] keyword, [fun] bodies extend as far as possible, and case
   clauses are juxtaposed without a separator but must carry a guard. *)

let string_of_fun_ref (f : atom) arity = unatom f ^ "/" ^ string_of_int arity

type ctx = {
    buf : Buffer.t
}

let join ctx pre sep post f l =
  let _ = Buffer.add_string ctx.buf pre in
  match l with
  | [] ->
     Buffer.add_string ctx.buf post
  | e :: l ->
      let _ = f e in
      let _ = List.map (fun e ->
                              let _ = Buffer.add_string ctx.buf sep in
                              f e) l in
      Buffer.add_string ctx.buf post

let rec string_of_expr ctx e =
  let push str = Buffer.add_string ctx.buf str in
  (* Separate tokens with newline to make Erlang compile errors useful: *)
  let _ = push "\n" in
  match e with
  | EVar v ->
     push (unvar v)
  | EFunRef (f, n) ->
     push (string_of_fun_ref f n)
  | EInt s ->
     push s
  | EFloat s ->
     push s
  | EString s ->
     let _ = push "\"" in
     let _ = push (String.escaped s) in
     push "\""
  | EAtom a ->
     push (unatom a)
  | ENil ->
     push "[]"
  | ECons (h, t) ->
     (* [$h | $t] *)
     let _ = push "[" in
     let _ = string_of_expr ctx h in
     let _ = push " | " in
     let _ = string_of_expr ctx t in
     push "]"
  | ETuple elems ->
     (* {$a, $b, ..., $x} *)
     join ctx "{" ", " "}" (string_of_expr ctx) elems
  | EMap assocs ->
     (* ~{$a => $b, $c => $d, ...}~ *)
     let emit (k, v) =
       let _ = string_of_expr ctx k in
       let _ = push " => " in
       string_of_expr ctx v in
     join ctx "~{" ", "  "}~" emit assocs
  | EBinary bytes ->
    (* one integer segment per byte; all four segment arguments are
       mandatory (shorter forms crash erlc's parser) *)
     let emit_seg c =
       let _ = push "#<" in
       let _ = push (string_of_int (Char.code c)) in
       push ">(8,1,'integer',['unsigned'|['big']])" in
     bytes
     |> String.to_seq
     |> List.of_seq
     |> join ctx  "#{" ", " "}#" emit_seg
  | ECall (m, f, args) ->
     let _ = List.map push ["call "; unatom m; ":"; unatom f] in
     join ctx "(" ", " ")" (string_of_expr ctx) args
  | EApply (h, args) ->
     let _ = push "apply " in
     let _ = string_of_expr ctx h in
     join ctx "(" ", " ")" (string_of_expr ctx) args
  | EFun (params, body) ->
     let params = List.map unvar params in
     let _ = push ("fun(" ^ String.concat "," params ^ ") -> ") in
     string_of_expr ctx body
  | ELet (v, e1, e2) ->
     let _ = List.map push [" let "; unvar v; " = "] in
     let _ = string_of_expr ctx e1 in
     let _ = push " in " in
     string_of_expr ctx e2
  | ELetrec (bindings, body) ->
     let emit_binding i =
       let _ = List.map push [unatom i.lr_name; "/"; List.length i.lr_params |> string_of_int; " = fun"] in
       let _ = join ctx "(" "," ")" (fun i -> push (unvar i)) i.lr_params in
       let _ = push " -> " in
       string_of_expr ctx i.lr_body in
     let _ = push " letrec " in
     let _ = List.map emit_binding bindings in
     let _ = push " in " in
     string_of_expr ctx body
  | ECase (scrutinee, branches) ->
     let _ = push " case " in
     let _ = string_of_expr ctx scrutinee in
     let _ = push " of " in
     let emit_branch (pattern, guard, expr) =
       let _ = List.map push [" <"; string_of_pattern pattern; "> when "] in
       let _ = string_of_expr ctx guard in
       let _ = push " -> " in
       string_of_expr ctx expr in
     join ctx "" " " " end " emit_branch branches
  | EPrimop (p, args) ->
     let _ = List.map push ["primop "; unatom p] in
     join ctx "(" ", " ")" (string_of_expr ctx) args

and string_of_pattern p =
  match p with
  | PWild -> "_"
  | PVar v -> unvar v
  | PAtom a -> unatom a
  | PNil -> "[]"
  | PCons (h, t) -> "[" ^ string_of_pattern h ^ "|" ^ string_of_pattern t ^ "]"
  | PInt i -> i
  | PTuple l -> "{" ^ String.concat "," (List.map string_of_pattern l) ^ "}"

let string_of_fundef buf fd =
  let push str = Buffer.add_string buf str in
  let arity = List.length fd.fun_params in
  let _ = List.map push
            [unatom fd.fun_name; "/"; string_of_int arity; " = fun("] in
  let _ = push (String.concat ", " (List.map unvar fd.fun_params)) in
  let _ = push ") -> " in
  let _ = string_of_expr { buf = buf } fd.fun_body in
  push "\n\n"

let string_of_module m =
  let buf = Buffer.create 10000 in
  let exported = List.filter (fun fd -> fd.fun_exported) m.mod_funcs in
  Buffer.add_string buf ("module " ^ unatom m.mod_name ^ " [");
  List.iteri (fun i fd ->
      if i > 0 then Buffer.add_string buf ",";
      Buffer.add_string buf
        (string_of_fun_ref fd.fun_name (List.length fd.fun_params)))
    exported;
  Buffer.add_string buf "] attributes []\n\n";
  List.iter (string_of_fundef buf) m.mod_funcs;
  Buffer.add_string buf "end\n";
  Buffer.contents buf
