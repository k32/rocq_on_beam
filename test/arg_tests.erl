-module(arg_tests).
-include_lib("eunit/include/eunit.hrl").

%% Arg := {'Int', N} | {'Bool', B}; ret_type is a type-level function
%% and is erased by extraction (absent from the module).

doit_test() ->
    ?assertEqual(4, arg:doit({'Int', 3})),
    ?assertEqual(0, arg:doit({'Int', -1})),
    ?assertEqual('false', arg:doit({'Bool', 'true'})),
    ?assertEqual('true', arg:doit({'Bool', 'false'})).

%% ret_type is erased: it has no runtime existence
ret_type_erased_test() ->
    ?assertMatch({'EXIT', {undef, _}}, catch arg:ret_type({'Int', 3})).
