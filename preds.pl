% See if the goal succeed without side effects
try(Goal) :-
    \+ \+ Goal.

var_remove_one(X, [Y | Ys], Ys) :-
    X == Y, !.
var_remove_one(X, [Y | Ys], [Y | Zs]) :-
    var_remove_one(X, Ys, Zs).

var_proper_subset([], [_ | _]).
var_proper_subset([X | Xs], [Y | Ys]) :-
    var_remove_one(X, [Y | Ys], Zs),
    var_proper_subset(Xs, Zs).


var_element(X, [Y | _])  :-
    X == Y, !.
var_element(X, [_ | Ys]) :-
    var_element(X,Ys).


% setof, succeeds with empty set if no solution can be found
setof0(X, G, L):-
    setof(X, G, L), !.
setof0(_, _, []).

element(X, [X | _]).
element(X , [_ | Ys]) :-
    element(X, Ys).
