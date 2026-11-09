-module(inductive_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'rocq_O';
nat(N) -> {'rocq_S', nat(N - 1)}.

%% tree := 'rocq_Leaf' | {'rocq_Node', N, L, R}
tree(leaf) -> 'rocq_Leaf';
tree({node, N, L, R}) -> {'rocq_Node', N, tree(L), tree(R)}.

%% red   := 'rocq_RLeaf' | {'rocq_RNode', G}
%% green := 'rocq_GNil' | {'rocq_GNode', R, G}
red(rleaf) -> 'rocq_RLeaf';
red({rnode, G}) -> {'rocq_RNode', green(G)}.

green(gnil) -> 'rocq_GNil';
green({gnode, R, G}) -> {'rocq_GNode', red(R), green(G)}.

sum_tree_test() ->
    T = tree({node, nat(1), {node, nat(2), leaf, leaf}, {node, nat(3), leaf, leaf}}),
    ?assertEqual(nat(6), rocq_inductive:rocq_sum_tree(T)).

sum_tree_leaf_test() ->
    ?assertEqual(nat(0), rocq_inductive:rocq_sum_tree(tree(leaf))).

mirror_test() ->
    T = tree({node, nat(1), {node, nat(2), leaf, leaf}, leaf}),
    ?assertEqual(tree({node, nat(1), leaf, {node, nat(2), leaf, leaf}}),
                 rocq_inductive:rocq_mirror(T)).

mirror_is_involution_test() ->
    T = tree({node, nat(1), {node, nat(2), leaf, {node, nat(3), leaf, leaf}}, leaf}),
    ?assertEqual(T, rocq_inductive:rocq_mirror(rocq_inductive:rocq_mirror(T))).

%% mutual inductive types + mutual fixpoint
count_red_green_test() ->
    %% RNode (GNode RLeaf GNil)
    R = red({rnode, {gnode, rleaf, gnil}}),
    ?assertEqual(nat(1), rocq_inductive:rocq_count_red(R)),

    %% GNode RLeaf (GNode RNil ...): count_green = 0 + 0 = 0,
    %% count_red through RNode = 1 + 0 = 1
    G = green({gnode, rleaf, {gnode, rleaf, gnil}}),
    ?assertEqual(nat(0), rocq_inductive:rocq_count_green(G)),

    R2 = red({rnode, {gnode, {rnode, {gnode, rleaf, gnil}}, gnil}}),
    ?assertEqual(nat(2), rocq_inductive:rocq_count_red(R2)).
