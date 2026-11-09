-module(record_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'O';
nat(N) -> {'S', nat(N - 1)}.

point(X, Y) -> {'mkPoint', X, Y}.

%% Records extract as one-constructor inductives: tagged tuples with
%% the constructor name, and projections as matches on that tag.

origin_test() ->
    ?assertEqual(point('O', 'O'), record:origin()).

projection_test() ->
    ?assertEqual(nat(3),
                 record:sum_coords(fun record:add/2,
                                             point(nat(1), nat(2)))).

match_test() ->
    ?assertEqual(point(nat(2), nat(5)),
                 record:translate(point(nat(1), nat(3)), nat(1), nat(2))).

%% A record with a single function field is eta-collapsed by extraction:
%% 'mkWrapper A B f' is extracted to just f, so apply_wrapped becomes the
%% identity on functions and adder_wrapper is the wrapped function itself.

apply_wrapped_test() ->
    Succ = fun(X) -> {'S', X} end,
    ?assertEqual(Succ, record:apply_wrapped(Succ)),
    ?assertEqual(nat(1), (record:apply_wrapped(Succ))(nat(0))).

adder_wrapper_test() ->
    ?assertEqual(nat(6), record:adder_wrapper(nat(3))).
