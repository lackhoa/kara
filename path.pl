:- [preds].

eq(a, b). eq(b, c). eq(d, c).

eq(x, y). eq(y, y). eq(y, z).

path([A, B]) :-
    (eq(A, B); eq(B, A)),
    A \== B.

path([A, B, C | Cs]) :-
    path([A, B]),
    path([B, C]),
    A \== C,
    path([B, C | Cs]),
    \+(element(A, [B, C | Cs])).

max_path(X) :-
    path(X),
    \+(path([_ | X])).
