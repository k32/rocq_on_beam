%% Aggregator eunit suite: runs every *_tests module in one go,
%% propagating failures.
-module(rocq_core_erlang_test).
-export([all/0]).
-export([adder_test/0, curry_test/0, dependent_arg_test/0, floats_test/0,
         hmap_test/0, inductive_test/0, mcurry_test/0, mpatho_test/0,
         nat_test/0, nested_test/0, patho_test/0, poly_test/0,
         primstring_test/0, record_test/0, rich_test/0, shadow_test/0,
         z_test/0]).

all() -> [adder_test, curry_test, dependent_arg_test, floats_test,
          hmap_test, inductive_test, mcurry_test, mpatho_test, nat_test,
          nested_test, patho_test, poly_test, primstring_test,
          record_test, rich_test, shadow_test, z_test].

adder_test() -> run(adder_tests).
curry_test() -> run(curry_tests).
dependent_arg_test() -> run(dependent_arg_tests).
floats_test() -> run(floats_tests).
hmap_test() -> run(hmap_tests).
inductive_test() -> run(inductive_tests).
mcurry_test() -> run(mcurry_tests).
mpatho_test() -> run(mpatho_tests).
nat_test() -> run(nat_tests).
nested_test() -> run(nested_tests).
patho_test() -> run(patho_tests).
poly_test() -> run(poly_tests).
primstring_test() -> run(primstring_tests).
record_test() -> run(record_tests).
rich_test() -> run(rich_tests).
shadow_test() -> run(shadow_tests).
z_test() -> run(z_tests).

run(M) ->
    case eunit:test(M, []) of
        ok -> ok;
        Error -> erlang:error({suite_failed, M, Error})
    end.
