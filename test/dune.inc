(rocq.extraction
 (prelude adder)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files adder.core))

(rule
 (alias runtest)
 (deps adder.core)
 (target adder.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude curry)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files curry.core))

(rule
 (alias runtest)
 (deps curry.core)
 (target curry.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude dependent_arg)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files dependent_arg.core))

(rule
 (alias runtest)
 (deps dependent_arg.core)
 (target dependent_arg.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude hmap)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files hmap.core))

(rule
 (alias runtest)
 (deps hmap.core)
 (target hmap.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude inductive)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files inductive.core))

(rule
 (alias runtest)
 (deps inductive.core)
 (target inductive.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude mcurry)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files mcurry.core))

(rule
 (alias runtest)
 (deps mcurry.core)
 (target mcurry.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude mpatho)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files mpatho.core))

(rule
 (alias runtest)
 (deps mpatho.core)
 (target mpatho.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude nat)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files nat.core))

(rule
 (alias runtest)
 (deps nat.core)
 (target nat.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude nested)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files nested.core))

(rule
 (alias runtest)
 (deps nested.core)
 (target nested.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude patho)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files patho.core))

(rule
 (alias runtest)
 (deps patho.core)
 (target patho.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude poly)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files poly.core))

(rule
 (alias runtest)
 (deps poly.core)
 (target poly.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude primstring)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files primstring.core))

(rule
 (alias runtest)
 (deps primstring.core)
 (target primstring.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude record)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files record.core))

(rule
 (alias runtest)
 (deps record.core)
 (target record.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude rich)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files rich.core))

(rule
 (alias runtest)
 (deps rich.core)
 (target rich.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude shadow)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files shadow.core))

(rule
 (alias runtest)
 (deps shadow.core)
 (target shadow.beam)
 (action
  (run erlc %{deps})))

(rocq.extraction
 (prelude z)
 (theories Stdlib Erlang LibTx)
 (plugins rocq-runtime.plugins.extraction rocq-core-erlang)
 (extracted_files z.core))

(rule
 (alias runtest)
 (deps z.core)
 (target z.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias runtest)
 (deps adder_tests.erl)
 (target adder_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias adder_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([adder_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps curry_tests.erl)
 (target curry_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias curry_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([curry_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps dependent_arg_tests.erl)
 (target dependent_arg_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias dependent_arg_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([dependent_arg_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps hmap_tests.erl)
 (target hmap_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias hmap_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([hmap_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps inductive_tests.erl)
 (target inductive_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias inductive_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([inductive_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps mcurry_tests.erl)
 (target mcurry_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias mcurry_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([mcurry_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps mpatho_tests.erl)
 (target mpatho_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias mpatho_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([mpatho_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps nat_tests.erl)
 (target nat_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias nat_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([nat_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps nested_tests.erl)
 (target nested_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias nested_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([nested_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps patho_tests.erl)
 (target patho_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias patho_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([patho_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps poly_tests.erl)
 (target poly_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias poly_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([poly_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps primstring_tests.erl)
 (target primstring_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias primstring_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([primstring_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps record_tests.erl)
 (target record_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias record_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([record_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps rich_tests.erl)
 (target rich_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias rich_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([rich_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps rocq_core_erlang_test.erl)
 (target rocq_core_erlang_test.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias rocq_core_erlang_test)
 (action
  (run erl -pa . -noshell -eval "eunit:test([rocq_core_erlang_test], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps shadow_tests.erl)
 (target shadow_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias shadow_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([shadow_tests], [verbose])" -s init stop)))

(rule
 (alias runtest)
 (deps z_tests.erl)
 (target z_tests.beam)
 (action
  (run erlc %{deps})))

(rule
 (alias z_tests)
 (action
  (run erl -pa . -noshell -eval "eunit:test([z_tests], [verbose])" -s init stop)))

