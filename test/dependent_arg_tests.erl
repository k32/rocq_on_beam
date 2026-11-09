-module(dependent_arg_tests).
-include_lib("eunit/include/eunit.hrl").

%% Arg := {'Int', Z} | {'Bool', B}; ret_type is a type-level function
%% and is erased by extraction (absent from the module).

doit_test() ->
    ?assertEqual(4, dependent_arg:doit({'Int', 3})),
    ?assertEqual(0, dependent_arg:doit({'Int', -1})),
    ?assertEqual(-123456789012345678901234567889,
                 dependent_arg:doit({'Int', -123456789012345678901234567890})),
    ?assertEqual('false', dependent_arg:doit({'Bool', 'true'})),
    ?assertEqual('true', dependent_arg:doit({'Bool', 'false'})).

%% ret_type is erased: it has no runtime existence
ret_type_erased_test() ->
    ?assertMatch({'EXIT', {undef, _}}, catch dependent_arg:ret_type({'Int', 3})).
