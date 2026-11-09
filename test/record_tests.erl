-module(record_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'rocq_O';
nat(N) -> {'rocq_S', nat(N - 1)}.

point(X, Y) -> {'rocq_mkPoint', X, Y}.

%% Records extract as one-constructor inductives: tagged tuples with
%% the constructor name, and projections as matches on that tag.

origin_test() ->
    ?assertEqual(point('rocq_O', 'rocq_O'), rocq_record:rocq_origin()).

projection_test() ->
    ?assertEqual(nat(3),
                 rocq_record:rocq_sum_coords(fun rocq_record:rocq_add/2,
                                             point(nat(1), nat(2)))).

match_test() ->
    ?assertEqual(point(nat(2), nat(5)),
                 rocq_record:rocq_translate(point(nat(1), nat(3)), nat(1), nat(2))).

%% A record with a single function field is eta-collapsed by extraction:
%% 'mkWrapper A B f' is extracted to just f, so apply_wrapped becomes the
%% identity on functions and adder_wrapper is the wrapped function itself.

apply_wrapped_test() ->
    Succ = fun(X) -> {'rocq_S', X} end,
    ?assertEqual(Succ, rocq_record:rocq_apply_wrapped(Succ)),
    ?assertEqual(nat(1), (rocq_record:rocq_apply_wrapped(Succ))(nat(0))).

adder_wrapper_test() ->
    ?assertEqual(nat(6), rocq_record:rocq_adder_wrapper(nat(3))).
