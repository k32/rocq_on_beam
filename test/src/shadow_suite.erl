-module(shadow_suite).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers now
nat(N) -> N.

%% Repeated shadowing of one name by let/lamda/fix binders:
%%   x1 = x0+1; x2 = 2*x1; result = (x2+10) + x2  (== 4*(x0+1)+10)
shadowlet_test() ->
    ?assertEqual(nat(26), shadow:shadowlet(nat(3))),
    ?assertEqual(nat(18), shadow:shadowlet(nat(1))),
    ?assertEqual(nat(14), shadow:shadowlet(nat(0))).

%% let shadows the parameter, the fix parameter shadows the let,
%% and an inner let shadows the fix parameter:
%%   shadowfix x = (2*x + 3)
shadowfix_test() ->
    ?assertEqual(nat(13), shadow:shadowfix(nat(5))),
    ?assertEqual(nat(3), shadow:shadowfix(nat(0))).
