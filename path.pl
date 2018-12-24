:- [preds].

%% Problem description
k_n(N, Adjs) :-
    range(N, Nodes),
    maplist(adjs(Nodes), Nodes, Adjs).

adjs(Nodes, Node, Node-As) :-
    tfilter(dif(Node), Nodes, As).


%% Algorithm description
warshall(Adjs, NodesIn, NodesOut) :-
    phrase(reachables(NodesIn, Adjs), Rs, NodesIn),
    % Rs is the reachables with NodesIn at the end,
    % since we need to include them, also
    sort(Rs, Rs_sorted),
    if_(Rs_sorted = NodesIn,
        NodesOut = Rs_sorted,
        warshall(Adjs, Rs_sorted, NodesOut)).

%% Get all the adjacent nodes of a list of nodes
reachables([], _) --> [].
reachables([Node|Nodes], Adjs) -->
    { member(Node-Rs, Adjs) },
    Rs,
    reachables(Nodes, Adjs).
