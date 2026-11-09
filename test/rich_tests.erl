-module(rich_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'O';
nat(N) -> {'S', nat(N - 1)}.

%% extracted lists: 'nil' | {'cons', H, T}
lst([]) -> 'nil';
lst([H | T]) -> {'cons', H, lst(T)}.

len_test() ->
    ?assertEqual(nat(0), rich:len('nil')),
    ?assertEqual(nat(2), rich:len(lst([nat(1), nat(2)]))).

partially_applied_test() ->
    %% double applied to 2
    ?assertEqual(nat(4), rich:partially_applied()).

even_odd_test() ->
    ?assertEqual('true',  rich:even(nat(2))),
    ?assertEqual('false', rich:even(nat(3))),
    ?assertEqual('true',  rich:odd(nat(1))),
    ?assertEqual('false', rich:odd(nat(2))).

fact_helper_test() ->
    ?assertEqual(nat(1), rich:fact_helper(nat(0))),
    ?assertEqual(nat(6), rich:fact_helper(nat(3))).

sumlist_test() ->
    ?assertEqual(nat(3), rich:sumlist(lst([nat(1), nat(2)]))).
