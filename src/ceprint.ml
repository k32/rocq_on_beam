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
      let _ = f ctx e in
      let _ = List.map (fun e ->
                              let _ = Buffer.add_string ctx.buf sep in
                              f ctx e) l in
      Buffer.add_string ctx.buf post

let rec emit_expr ctx e =
  let push str = Buffer.add_string ctx.buf str in
  (* Separate exprs with newline to get useful Erlang compile errors locations: *)
  let _ = push "\n" in
  let emit_fun params body =
    (* fun($v1, $v2, ...) -> $body *)
    let _ = push "fun" in
    let _ = join ctx "(" "," ")" (fun _ i -> push (unvar i)) params in
    let _ = push " -> " in
    emit_expr ctx body in
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
     let _ = emit_expr ctx h in
     let _ = push " | " in
     let _ = emit_expr ctx t in
     push "]"
  | ETuple elems ->
     (* {$a, $b, ..., $x} *)
     join ctx "{" ", " "}" emit_expr elems
  | EMap assocs ->
     (* ~{$a => $b, $c => $d, ...}~ *)
     let emit ctx (k, v) =
       let _ = emit_expr ctx k in
       let _ = push " => " in
       emit_expr ctx v in
     join ctx "~{" ", "  "}~" emit assocs
  | EBinary bytes ->
    (* one integer segment per byte; all four segment arguments are
       mandatory (shorter forms crash erlc's parser) *)
     let emit_seg _ c =
       let _ = push "#<" in
       let _ = push (string_of_int (Char.code c)) in
       push ">(8,1,'integer',['unsigned'|['big']])" in
     bytes
     |> String.to_seq
     |> List.of_seq
     |> join ctx  "#{" ", " "}#" emit_seg
  | EApply (c, args) -> (
    match c with
    | CGlobal (m, f) ->
      let _ = push "call " in
      let _ = push (unatom m) in
      let _ = push ":" in
      let _ = push (unatom f) in
      emit_args ctx args
    | CExpr h ->
      let _ = push "apply " in
      emit_expr ctx h;
      emit_args ctx args
    | CPrimop p ->
      let _ = push "primop " in
      let _ = push (unatom p) in
      emit_args ctx args)
| EFun (params, body) ->
     emit_fun params body
  | ELet (v, e1, e2) ->
     let _ = List.map push ["let "; unvar v; " = "] in
     let _ = emit_expr ctx e1 in
     let _ = push " in " in
     emit_expr ctx e2
  | ELetrec (bindings, body) ->
     let emit_binding i =
       let _ = List.map push [unatom i.lr_name; "/"; List.length i.lr_params |> string_of_int; " = "] in
       emit_fun i.lr_params i.lr_body in
     let _ = push "letrec " in
     let _ = List.map emit_binding bindings in
     let _ = push " in " in
     emit_expr ctx body
  | ECase (scrutinee, branches) ->
     let _ = push "case " in
     let _ = emit_expr ctx scrutinee in
     let _ = push " of " in
     let emit_branch ctx (pattern, guard, expr) =
       let _ = List.map push [" <"; string_of_pattern pattern; "> when "] in
       let _ = emit_expr ctx guard in
       let _ = push " -> " in
       emit_expr ctx expr in
     join ctx "" " " " end " emit_branch branches

and emit_args ctx args =
  let push str = Buffer.add_string ctx.buf str in
  let _ = push "(" in
  let rec go l =
    match l with
    | [] -> push ")"
    | [ e ] ->
      let _ = emit_expr ctx e in
      push ")"
    | e :: t ->
      let _ = emit_expr ctx e in
      let _ = push "," in
      go t
  in
  go args

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
  let _ = emit_expr { buf = buf } fd.fun_body in
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
