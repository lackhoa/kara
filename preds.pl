:- use_module(library(clpfd)).
:- use_module(reif).

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

element(X, [X | _]) :- !.
element(X , [_ | Ys]) :-
    element(X, Ys).


list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

concatenation([]) --> [].
concatenation([List|Lists]) -->
    list(List),
    concatenation(Lists).

range(Low, High, Ls) :-
    High_pred is High - 1,
    bagof(I, between(Low, High_pred, I), Ls).

range(High, Ls) :-
    range(0, High, Ls).

%% Safe length
list_length([], 0).
list_length([_|Ls], N) :-
    list_length(Ls, M), N #= M+1.

%% Length minus one
length_pred(Ls, N) :-
    list_length(Ls, M), N #= M-1.

%% Choose function
n_from_chosen(0, _, []).
n_from_chosen(N, [X|Es], [X|Xs]) :-
   N #> 0,
   N #= N0+1,
   n_from_chosen(N0, Es, Xs).
n_from_chosen(N, [_|Es], Xs) :-
   N #> 0,
   n_from_chosen(N, Es, Xs).

%% Update list
list_ivs_updated(L0, IVs0, L) :-
    keysort(IVs0, IVs),
    list_ivs_updated_(0, L0, IVs, L).
list_ivs_updated_(_, L, [], L).
list_ivs_updated_(Id, [_|Xs], [Id-Val|IVs], [Val|Rest]) :-
    J #= Id+1,
    list_ivs_updated_(J, Xs, IVs, Rest).
list_ivs_updated_(I, [X|Xs], [Id-Val|IVs], [X|Rest]) :-
    I #\= Id,
    J #= I+1,
    list_ivs_updated_(J, Xs, [Id-Val|IVs], Rest).

% Triska's DCG state
state(S),     [S] --> [S].
state(S0, S), [S] --> [S0].
