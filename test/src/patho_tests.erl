-module(patho_tests).
-include_lib("eunit/include/eunit.hrl").

%% Each test compares the extracted function applied at runtime with
%% the kernel-computed (vm_compute) oracle extracted as *_result().

shadows_test() ->
    ?assertEqual(patho:shadows_result(), patho:shadows(7)).

meven_test() ->
    ?assertEqual(patho:meven_result(), patho:meven(10)).

church_test() ->
    ?assertEqual(patho:church_result(), patho:church(2, 10)).

deepmatch_test() ->
    ?assertEqual(patho:deepmatch_result(),
                 patho:deepmatch({'Some', [{'Some', {3, true}},
                                           'None',
                                           {'Some', {9, false}}]})).

fixnest_test() ->
    ?assertEqual(patho:fixnest_result(), patho:fixnest(5)).

zfact_test() ->
    ?assertEqual(patho:zfact_result(), patho:zfact(20)).

selfapp_test() ->
    ?assertEqual(patho:selfapp_result(), patho:selfapp(41)).

pipeline_test() ->
    ?assertEqual(patho:pipeline_result(), patho:pipeline(5)).
