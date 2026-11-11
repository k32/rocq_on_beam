let extract base =
  Printf.printf
{|(rocq.extraction
 (prelude %s)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files %s.core))

(rule
 (alias runtest)
 (deps %s.core)
 (target %s.beam)
 (action
  (run erlc %%{deps})))

|}
    base base base base

let run_suite base =
  Printf.printf
{|(rule
 (alias runtest)
 (deps %s.erl)
 (target %s.beam)
 (action
  (run erlc %%{deps})))

(rule
 (alias %s)
 (action
  (run erl -pa . -noshell -eval "eunit:test([%s], [verbose])" -s init stop)))

|}
  base base base base

let () =
  let files = Sys.readdir "." |> Array.to_list |> List.sort String.compare in
  let rocq_srcs = files |> List.filter_map (Filename.chop_suffix_opt ~suffix:".v") in
  let _ = List.iter extract rocq_srcs in
  let suites = files |> List.filter_map (Filename.chop_suffix_opt ~suffix:".erl") in
  List.iter run_suite suites
