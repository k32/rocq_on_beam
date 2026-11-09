-module(inductive_tests).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers now
nat(N) -> N.

%% tree := 'Leaf' | {'Node', N, L, R}
tree(leaf) -> 'Leaf';
tree({node, N, L, R}) -> {'Node', N, tree(L), tree(R)}.

%% red   := 'RLeaf' | {'RNode', G}
%% green := 'GNil' | {'GNode', R, G}
red(rleaf) -> 'RLeaf';
red({rnode, G}) -> {'RNode', green(G)}.

green(gnil) -> 'GNil';
green({gnode, R, G}) -> {'GNode', red(R), green(G)}.

sum_tree_test() ->
    T = tree({node, nat(1), {node, nat(2), leaf, leaf}, {node, nat(3), leaf, leaf}}),
    ?assertEqual(nat(6), inductive:sum_tree(T)).

sum_tree_leaf_test() ->
    ?assertEqual(nat(0), inductive:sum_tree(tree(leaf))).

mirror_test() ->
    T = tree({node, nat(1), {node, nat(2), leaf, leaf}, leaf}),
    ?assertEqual(tree({node, nat(1), leaf, {node, nat(2), leaf, leaf}}),
                 inductive:mirror(T)).

mirror_is_involution_test() ->
    T = tree({node, nat(1), {node, nat(2), leaf, {node, nat(3), leaf, leaf}}, leaf}),
    ?assertEqual(T, inductive:mirror(inductive:mirror(T))).

%% mutual inductive types + mutual fixpoint
count_red_green_test() ->
    %% RNode (GNode RLeaf GNil)
    R = red({rnode, {gnode, rleaf, gnil}}),
    ?assertEqual(nat(1), inductive:count_red(R)),

    %% GNode RLeaf (GNode RNil ...): count_green = 0 + 0 = 0,
    %% count_red through RNode = 1 + 0 = 1
    G = green({gnode, rleaf, {gnode, rleaf, gnil}}),
    ?assertEqual(nat(0), inductive:count_green(G)),

    R2 = red({rnode, {gnode, {rnode, {gnode, rleaf, gnil}}, gnil}}),
    ?assertEqual(nat(2), inductive:count_red(R2)).
