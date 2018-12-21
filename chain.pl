:- [preds].

% model(M) <- M is a model of the clauses defined by cl/1
model(M) :-
    model_check([], M).

model_check(M0, M) :-
    is_violated(Head, M0),  % instance of violated clause
    !,
    disj_element(L, Head),  % L: ground literal from head
    model_check([L | M0], M).  % add L to the model, check it all over again
model_check(M, M).  % no more violated clauses

is_violated(H, M) :-
    cl(H :- B),
    satisfied_body(B, M),   % grounds the variables
    \+(satisfied_head(H, M)).

% body is a conjunction
satisfied_body(true, _M).
satisfied_body(A, M) :-
    member(A, M).
satisfied_body((A, B), M) :-
    member(A, M),
    satisfied_body(B, M).

% head is a disjunction
satisfied_head(A, M) :-
    member(A, M).
satisfied_head((A; _B), M) :-
    member(A, M).
satisfied_head((_A; B), M) :-
    satisfied_head(B, M).

disj_element(X, X) :-
    % single-element disjunction
    X \= false,
    X \= (_; _).
disj_element(X, (X; _Ys)).
disj_element(X, (_Y; Ys)) :-
    disj_element(X, Ys).

%%% Example disjunctive clauses to compute model for
cl((X = Z :- X = Y, Y = Z)).
cl(X = Y :- Y = X).
cl(a = b :- true).
cl(b = c :- true).
cl(x = y :- true).
