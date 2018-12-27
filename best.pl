:- [preds].

% goal/1,  children/2 and eval/2 depend on the problem

search_bstf([Goal | Rest], Goal) :-
    % Found the goal!
    goal(Goal),
    !.

search_bstf([Current | Rest], Goal) :-
    % Haven't found goal
    children(Current, Children),
    add_bstf(Children, Rest, Agenda_p),
    search_bstf(Agenda_p, Goal).

add_bstf([], Agenda, Agenda).

add_bstf([Child | Children], Agenda, Agenda_p) :-
    add_one(Child, Agenda, Agenda_tmp),
    add_bstf(Children, Agenda_tmp, Agenda_p).

add_one(Child, Agenda, Agenda_p) :-
    eval(Child, Value),
    add_one(Value, Child, Agenda, Agenda_p).

add_one(Value, Child, [], [Child]).

add_one(Value, Child, [Node | Rest], [Child, Node | Rest]) :-
    % Found the right place to insert
    eval(Node, NodeV), Value < NodeV,
    !.

add_one(Value, Child, [Node | Rest], [Node | Rest_p]) :-
    % Keep looking for a place to insert
    add_one(Value, Child, Rest, Rest_p).

merge([], R, R).
merge(R, [], R).

merge([X | Xs], [Y | Ys], [X | R]) :-
    % The smaller element is on the left
    eval(X, Vx), eval(Y, Vy), Vx < Vy,
    !,
    merge(Xs, [Y | Ys], R).

merge(X, [Y | Ys], [Y | R]) :-
    % The smaller (or equal) element is on the right
    merge(X, Ys, R).





% get_tile(P, N, T) <- pos. P contains tile T at square N
get_tile(Pos, N, T) :-
    get_tile(Pos, 1, N, T).

get_tile([X | _Xs], N, N, X).
get_tile([_X | Xs], I, N, Y) :-
    J is I+1, get_tile(Xs, J, N, Y).

% replace(P, N, T, P1) <- P1 is P with tile T at square N
replace([_X | Xs], 1, Y, [Y | Xs]).
replace([X | Xs], I, Y, [X | Zs]) :-
    J is I-1, replace(Xs, J, Y, Zs).






tiles(Moves, Cost) :-
    % moves `Moves` lead to a goal position at cost `Cost`
    start(Start),
    eval(Start, Value),
    tiles_a([v(Value, Start)], Final, [], Visited),
    construct_moves(Final, Visited, [], Moves, 0, Cost).

% tiles_a(A, M, V0, V) <- goal position can be reached from one of the positions on A with last move M
tiles_a([v(V, LastMove) | Rest], LastMove, Visited, Visited) :-
    goal(LastMove).

tiles_a([v(V, LastMove) | Rest], Goal, Visited0, Visited) :-
    %% show_move(LastMove, V),
    setof0(v(Value, NextMove),
           (move(LastMove, NextMove), eval(NextMove, Value)),
           Children),
    merge(Children, Rest, NewAgenda),    % best-first
    tiles_a(NewAgenda, Goal, [LastMove | Visited0], Visited).





move(m(PosOld, Pos,    OldCost),
     m(Pos,    PosNew, Cost)) :-
    get_tile(Pos, Ne, e), get_tile(Pos, Nbw, BW), not(BW=e),
    Diff is abs(Ne - Nbw), Diff<4,
    replace(Pos, Ne, BW, Pos1),
    replace(Pos1, Nbw, e, PosNew),
    ( Diff=1    -> Cost=1
    ; otherwise -> Cost is Diff-1 ).

start(m(noparent, [b, b, b, e, w, w, w], 0)).

% reconstruct total cost and path from visited nodes

construct_moves(m(noparent, Start, 0), V, Ms, [Start | Ms], Cost, Cost).

construct_moves(m(P, Pos, C), Visited, Ms0, Ms, Cost0, Cost) :-
    element(m(GP, P, C1), Visited),  % GP is parent of P
    Cost1 is Cost0+C,
    construct_moves(m(GP, P, C1), Visited, [Pos | Ms0], Ms, Cost1, Cost).

show_move(m(P, Pos, C), Value) :-
    write(Pos-Value), nl.

goal(LastMove) :-
    eval(LastMove, 0).

eval(m(P, Pos, C), Value) :-
    bLeftOfw(Pos, Value).

bLeftOfw(Pos, Value) :-
    findall((Nb,Nw),
            (get_tile(Pos, Nb, b),get_tile(Pos, Nw, w),Nb < Nw),
            L),
    length(L, Value).
