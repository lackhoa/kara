:- [preds].

%% Test program
%% Nodes are lists of letters
% Unnamed arcs
arc(X, Y) :-
    arc(X, Y, _).

%% arc(T, [H | T], H) :-
%%     length(T, N), N<3,
%%     member(H, [a, d, i]).

% find palindromes
%% goal(L) :-
%%     reverse(L, L).

%% main(Goal, Path) :-
%%     search([nil-[]], [], Goal, Path).

%% General search with loop detection (and path reconstruction)
node_children(Node, Children) :-
    findall(C, arc(Node, C), Children).

prev_visited_path(Prev, Visited, [ArcName>>Prev|Path]) :-
    member(PrevPrev-Prev, Visited),
    (  arc(PrevPrev, Prev, ArcName)
    ->  prev_visited_path(PrevPrev, Visited, Path)
    ;  Path = [],  % Met root
       ArcName = 'noarc'  ).

search([Prev-Goal|_], Visited, Goal, Path) :-
    goal(Goal),
    prev_visited_path(Goal, [Prev-Goal|Visited], Path0),
    % Reverse only for aesthetics
    reverse(Path0, Path).

search([Prev-Node|Rest], Visited, Goal, Path) :-
    %% format('Node: ~w\n', [Node]),
    node_children(Node, Children),
    findall(Node-Child, member(Child, Children), NC),
    add_df(NC, Rest, Visited, Agenda),  % Insert scheduling algo here!
    search(Agenda, [Prev-Node|Visited], Goal, Path).


%% dfs-specific
add_df([], Agenda, _, Agenda).

add_df([Prev-Node|Rest], Agenda0, Visited, Agenda):-
    (  (member(_-Node, Agenda0);
        member(_-Node, Visited))
    ->  add_df(Rest, Agenda0,             Visited, Agenda)
    ;  add_df(Rest, [Prev-Node|Agenda0], Visited, Agenda)
    ).
