let extract base =
  Printf.printf
{|(rocq.extraction
 (prelude %s)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files %s.core))
|}
    base base

let () =
  let modules = Sys.readdir "." |> Array.to_list |> List.sort String.compare
                |> List.filter_map (Filename.chop_suffix_opt ~suffix:".v") in
  List.iter extract modules
