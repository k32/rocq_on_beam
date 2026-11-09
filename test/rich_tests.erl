-module(rich_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'rocq_O';
nat(N) -> {'rocq_S', nat(N - 1)}.

%% extracted lists: 'rocq_nil' | {'rocq_cons', H, T}
lst([]) -> 'rocq_nil';
lst([H | T]) -> {'rocq_cons', H, lst(T)}.

len_test() ->
    ?assertEqual(nat(0), rocq_rich:rocq_len('rocq_nil')),
    ?assertEqual(nat(2), rocq_rich:rocq_len(lst([nat(1), nat(2)]))).

partially_applied_test() ->
    %% double applied to 2
    ?assertEqual(nat(4), rocq_rich:rocq_partially_applied()).

even_odd_test() ->
    ?assertEqual('rocq_true',  rocq_rich:rocq_even(nat(2))),
    ?assertEqual('rocq_false', rocq_rich:rocq_even(nat(3))),
    ?assertEqual('rocq_true',  rocq_rich:rocq_odd(nat(1))),
    ?assertEqual('rocq_false', rocq_rich:rocq_odd(nat(2))).

fact_helper_test() ->
    ?assertEqual(nat(1), rocq_rich:rocq_fact_helper(nat(0))),
    ?assertEqual(nat(6), rocq_rich:rocq_fact_helper(nat(3))).

sumlist_test() ->
    ?assertEqual(nat(3), rocq_rich:rocq_sumlist(lst([nat(1), nat(2)]))).
