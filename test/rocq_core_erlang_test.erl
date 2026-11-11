%% Aggregator eunit suite: runs every *_tests module in one go.
-module(rocq_core_erlang_test).
-export([all/0]).
-export([adder_test/0, curry_test/0, dependent_arg_test/0, hmap_test/0,
         inductive_test/0, mcurry_test/0, mpatho_test/0, nat_test/0,
         nested_test/0, patho_test/0, poly_test/0, primstring_test/0,
         record_test/0, rich_test/0, shadow_test/0, z_test/0]).

all() -> [adder_test, curry_test, dependent_arg_test, hmap_test,
          inductive_test, mcurry_test, mpatho_test, nat_test, nested_test,
          patho_test, poly_test, primstring_test, record_test, rich_test,
          shadow_test, z_test].

adder_test() -> eunit:test(adder_tests, []).
curry_test() -> eunit:test(curry_tests, []).
dependent_arg_test() -> eunit:test(dependent_arg_tests, []).
hmap_test() -> eunit:test(hmap_tests, []).
inductive_test() -> eunit:test(inductive_tests, []).
mcurry_test() -> eunit:test(mcurry_tests, []).
mpatho_test() -> eunit:test(mpatho_tests, []).
nat_test() -> eunit:test(nat_tests, []).
nested_test() -> eunit:test(nested_tests, []).
patho_test() -> eunit:test(patho_tests, []).
poly_test() -> eunit:test(poly_tests, []).
primstring_test() -> eunit:test(primstring_tests, []).
record_test() -> eunit:test(record_tests, []).
rich_test() -> eunit:test(rich_tests, []).
shadow_test() -> eunit:test(shadow_tests, []).
z_test() -> eunit:test(z_tests, []).
