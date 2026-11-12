(*-----------------------------------------------------------------------*)
(*  Phase 2: pretty-print the Core Erlang AST (Cerlang) as source text.   *)
(*                                                                       *)
(*  This phase is pure: no naming, no de Bruijn indices, no Rocq         *)
(*  knowledge — just a straightforward rendering of the AST into the     *)
(*  Core Erlang concrete syntax (as accepted by erlc).                   *)
(*-----------------------------------------------------------------------*)

open Cerlang

(* -------------------------------------------------------------------- *)
(* Atom naming                                                          *)
(* -------------------------------------------------------------------- *)

(* Only the quote character is escaped; the empty atom '' is valid
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
  Buffer.add_string ctx.buf pre;
  match l with
  | [] ->
     Buffer.add_string ctx.buf post
  | e :: l ->
     f ctx e;
     List.iter (fun e ->
                  Buffer.add_string ctx.buf sep;
                  f ctx e) l;
     Buffer.add_string ctx.buf post

let rec emit_expr ctx e =
  let push str = Buffer.add_string ctx.buf str in
  (* Separate exprs with newline to get useful Erlang compile errors locations: *)
  push "\n";
  let emit_fun params body =
    (* fun($v1, $v2, ...) -> $body *)
    push "fun";
    join ctx "(" "," ")" (fun _ i -> push (unvar i)) params;
    push " -> ";
    emit_expr ctx body in
  match e with
  | EVar v ->
     push (unvar v)
  | EFunRef (f, n) ->
     push (string_of_fun_ref f n)
  | EInt z ->
     push (Z.to_string z)
  | EFloat f ->
     (* back to the kernel's printer via the exact 64-bit round-trip *)
     push (Float64.to_string (Float64.of_float f))
  | EString s ->
     push "\"";
     push (String.escaped s);
     push "\""
  | EAtom a ->
     push (unatom a)
  | ENil ->
     push "[]"
  | ECons (h, t) ->
     (* [$h | $t] *)
     push "[";
     emit_expr ctx h;
     push " | ";
     emit_expr ctx t;
     push "]"
  | ETuple elems ->
     (* {$a, $b, ..., $x} *)
     join ctx "{" ", " "}" emit_expr elems
  | EMap assocs ->
     (* ~{$a => $b, $c => $d, ...}~ *)
     let emit ctx (k, v) =
       emit_expr ctx k;
       push " => ";
       emit_expr ctx v in
     join ctx "~{" ", " "}~" emit assocs
  | EBinary bytes ->
    (* one integer segment per byte; all four segment arguments are
       mandatory (shorter forms crash erlc's parser) *)
    let emit_seg _ c =
      push "#<";
      push (string_of_int (Char.code c));
      push ">(8,1,'integer',['unsigned'|['big']])" in
    bytes
    |> String.to_seq
    |> List.of_seq
    |> join ctx  "#{" ", " "}#" emit_seg
  | EApply (c, args) -> (
    match c with
    | CGlobal (m, f) ->
      push "call ";
      push (unatom m);
      push ":";
      push (unatom f);
      emit_args ctx args
    | CExpr h ->
      push "apply ";
      emit_expr ctx h;
      emit_args ctx args
    | CPrimop p ->
      push "primop ";
      push (unatom p);
      emit_args ctx args)
| EFun (params, body) ->
     emit_fun params body
  | ELet (v, e1, e2) ->
     List.iter push ["let "; unvar v; " = "];
     emit_expr ctx e1;
     push " in ";
     emit_expr ctx e2
  | ELetrec (bindings, body) ->
     let emit_binding i =
       List.iter push [unatom i.lr_name; "/"; List.length i.lr_params |> string_of_int; " = "];
       emit_fun i.lr_params i.lr_body in
     push "letrec ";
     List.iter emit_binding bindings;
     push " in ";
     emit_expr ctx body
  | ECase (scrutinee, branches) ->
     push "case ";
     emit_expr ctx scrutinee;
     push " of ";
     let emit_branch ctx (pattern, guard, expr) =
       List.iter push [" <"; string_of_pattern pattern; "> when "];
       emit_expr ctx guard;
       push " -> ";
       emit_expr ctx expr in
     join ctx "" " " " end " emit_branch branches

and emit_args ctx args =
  let push str = Buffer.add_string ctx.buf str in
  push "(";
  let rec go l =
    match l with
    | [] -> push ")"
    | [ e ] ->
      emit_expr ctx e;
      push ")"
    | e :: t ->
      emit_expr ctx e;
      push ",";
      go t
  in
  go args

and string_of_pattern p =
  match p with
  | PWild -> "_"
  | PVar v -> unvar v
  | PAtom a -> unatom a
  | PNil -> "[]"
  | PCons (h, t) -> "[" ^ string_of_pattern h ^ " | " ^ string_of_pattern t ^ "]"
  | PInt i -> Z.to_string i
  | PTuple l -> "{" ^ String.concat "," (List.map string_of_pattern l) ^ "}"

let string_of_fundef buf fd =
  let push str = Buffer.add_string buf str in
  let arity = List.length fd.fun_params in
  List.iter push
    [unatom fd.fun_name; "/"; string_of_int arity; " = fun("];
  push (String.concat ", " (List.map unvar fd.fun_params));
  push ") -> ";
  emit_expr { buf = buf } fd.fun_body;
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
