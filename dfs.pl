:- [preds].
%% General search framework with loop detection (and path reconstruction)

%% Test program
% Specify queueing function
queue_algo(dfs).

%% States are lists of letters
%% Specify arcs: arc(Head, Name, Weight, Tail)
pred_arc(T, H, 1, [H | T]) :-
    length(T, N), N<3,
    member(H, [a, d, i]).

% Specify goals: find palindromes
goal(L) :-
    length(L, 3), reverse(L, L).

main(Solution) :-
    search([], Solution).

%% The general search algorithm
% Node == path == [links] == ArcName-ArcWeight>>State
% Visited is a list of expanded states
% Solution is a node
search(StartState, Solution) :-
    StartNode  = [nil-0>>StartState],
    StartQueue = [StartNode],
    search(StartQueue, [], Solution).

search([VisitingNode | QueueRest0],
       Visited,
       Solution) :-
    VisitingNode = [_-_>>State | _],

    findall(DummyNode,
            (
                pred_arc(State, Name, Weight, Child),
                DummyNode = [Name-Weight>>Child
                             | VisitingNode],
                %% Eliminate loops - phase 1
                % Child is the point of contention
                \+((   member(Child, Visited)
                   % A better proposal already exists
                   ;   BetterPath = [_-_>>Child | _],
                       member(BetterPath, QueueRest0),
                       path_cost(BetterPath, BetterCost),
                       path_cost(DummyNode,  Cost),
                       BetterCost #=< Cost
                   ))
            ),
            NewNodes),

    %% Eliminate loops - phase 2
    % Remaining nodes with the same state in the queue,
    % are worse than this node
    findall(DummyPath,
            (
                member(DummyPath, QueueRest0),
                \+(( DummyPath = [_-_>>Child | _],
                     member([_-_>>Child | _], NewNodes) ))
            ),
            QueueRest),

    queue_algo(Algo),
    call(Algo, NewNodes, QueueRest, Queue),
    search(Queue, [State|Visited], Solution).

search([RevSolution|_], _, Solution) :-
    RevSolution = [_-_>>Goal | _],
    goal(Goal),
    reverse(RevSolution, Solution).

path_cost([], 0).
path_cost([_-Weight>>_ | RestPath], Cost) :-
    path_cost(RestPath, RestCost),
    Cost #= Weight+RestCost.

%% Queueing strategy
dfs(New, Queue0, Queue) :-
    append(New, Queue0, Queue).

bfs(New, Queue0, Queue) :-
    map_list_to_pairs(path_cost, New,    KeyNew),
    map_list_to_pairs(path_cost, Queue0, KeyQueue0),
    keymerge(KeyNew, KeyQueue0, KeyQueue),
    pairs_values(KeyQueue, Queue).
