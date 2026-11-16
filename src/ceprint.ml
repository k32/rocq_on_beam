(*-----------------------------------------------------------------------
  Phase 2: pretty-print the Erlang AST (Cerlang) as source text.

  This phase is pure: no naming, no de Bruijn indices, no Rocq
  knowledge — just a straightforward rendering of the AST into the
  Erlang concrete syntax (as accepted by erlc).
-----------------------------------------------------------------------*)

open Cerlang

(* Only the quote character is escaped; the empty atom '' is valid
   Erlang. *)
let escape_atom s =
  let buf = Buffer.create (String.length s + 5) in
  String.iter
    (fun c ->
      if c = '\'' then Buffer.add_string buf "\\'" else Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Render an atom as Erlang source: quoted, with any quotes escaped.
   This is the single place where atoms are quoted. *)
let unatom (Cerlang.Atom s) = "'" ^ escape_atom s ^ "'"

(* Render a variable: variables are unique integers created by
   [Cetrans.fresh_var]; the concrete name is derived here. *)
let unvar (Cerlang.Var n) = "_" ^ string_of_int n

let string_of_fun_ref (f : atom) arity =
  unatom f (* ^ "/" ^ string_of_int arity *)

let join buf pre sep post f l =
  Buffer.add_string buf pre;
  match l with
  | [] -> Buffer.add_string buf post
  | fst :: rest ->
      f buf fst;
      List.iter
        (fun i ->
          Buffer.add_string buf sep;
          f buf i)
        rest;
      Buffer.add_string buf post

let rec emit_expr buf e =
  let push str = Buffer.add_string buf str in
  (* Separate exprs with newline to get useful Erlang compile errors locations: *)
  push "\n";
  let emit_fun name params body =
    (* fun $name?($v1, $v2, ...) -> $body *)
    push "fun ";
    (match name with None -> () | Some str -> push str);
    join buf "(" "," ")" (fun _ i -> push (unvar i)) params;
    push " -> ";
    emit_expr buf body;
    push "\nend"
  in
  match e with
  | EVar v -> push (unvar v)
  | EFunRef (f, n) -> push (string_of_fun_ref f n)
  | EInt z -> push (Z.to_string z)
  | EFloat f ->
      (* back to the kernel's printer via the exact 64-bit round-trip *)
      push (Float64.to_string (Float64.of_float f))
  | EString s ->
      push "\"";
      push (String.escaped s);
      push "\""
  | EAtom a -> push (unatom a)
  | ENil -> push "[]"
  | ECons (h, t) ->
      (* [$h | $t] *)
      push "[";
      emit_expr buf h;
      push " | ";
      emit_expr buf t;
      push "]"
  | ETuple elems ->
      (* {$a, $b, ..., $x} *)
      join buf "{" ", " "}" emit_expr elems
  | EMap assocs ->
      (* #{$a => $b, $c => $d, ...} *)
      let emit ctx (k, v) =
        emit_expr ctx k;
        push " => ";
        emit_expr ctx v
      in
      join buf "#{" ", " "}" emit assocs
  | EBinary bytes ->
      let emit_byte _ c = push (string_of_int (Char.code c)) in
      bytes |> String.to_seq |> List.of_seq |> join buf "<<" ", " ">>" emit_byte
  | EApply (c, args) -> (
      match c with
      | CGlobal (m, f) ->
          push (unatom m);
          push ":";
          push (unatom f);
          emit_args buf args
      | CExpr callee ->
          emit_expr buf callee;
          emit_args buf args)
  | EFun (params, body) -> emit_fun None params body
  | ELet (v, e1, e2) ->
      push (unvar v);
      push " = ";
      emit_expr buf e1;
      push ",";
      emit_expr buf e2
  | ELetrec (bindings, body) ->
      let emit_binding i =
        emit_fun (Some (unatom i.lr_name)) i.lr_params i.lr_body
      in
      List.iter emit_binding bindings;
      push ",";
      emit_expr buf body
  | ECase (scrutinee, branches) ->
      push "case";
      emit_expr buf scrutinee;
      push "\nof\n";
      let emit_branch ctx (pattern, guard, expr) =
        push (string_of_pattern pattern);
        push " when";
        emit_expr ctx guard;
        push " ->";
        emit_expr ctx expr
      in
      join buf "" ";\n" "\nend" emit_branch branches

and emit_args buf args = join buf "(" ", " ")" emit_expr args

and string_of_pattern p =
  match p with
  | PWild -> "_"
  | PVar v -> unvar v
  | PAtom a -> unatom a
  | PNil -> "[]"
  | PCons (h, t) ->
      "[" ^ string_of_pattern h ^ " | " ^ string_of_pattern t ^ "]"
  | PInt i -> Z.to_string i
  | PTuple l -> "{" ^ String.concat "," (List.map string_of_pattern l) ^ "}"

let string_of_fundef buf fd =
  let push str = Buffer.add_string buf str in
  push (unatom fd.fun_name);
  let emit_arg _ arg = push (unvar arg) in
  join buf "(" ", " ") ->" emit_arg fd.fun_params;
  emit_expr buf fd.fun_body;
  push ".\n\n\n"

let string_of_module m =
  let buf = Buffer.create 10000 in
  let push = Buffer.add_string buf in
  push ("-module(" ^ unatom m.mod_name ^ ").\n");
  (* Exports: *)
  let exported = List.filter (fun fd -> fd.fun_exported) m.mod_funcs in
  let emit_export fn =
    push "-export([";
    push (unatom fn.fun_name ^ "/" ^ string_of_int (List.length fn.fun_params));
    push "]).\n"
  in
  List.iter emit_export exported;
  push "\n";
  (* Functions *)
  List.iter (string_of_fundef buf) m.mod_funcs;
  Buffer.contents buf
