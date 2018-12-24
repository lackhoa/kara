:- [preds].
:- [dfs].

pair_go(jug(ACap,A0), jug(BCap,B0),
        jug(ACap,A) , jug(BCap,B)) :-
    A+B #= A0+B0,  % The total water volume doesn't change
    A0 #\= A,  % There must be some changes
    A in 0..ACap, B in 0..BCap,  % Don't spill anything
    (  A #= ACap ; B #= BCap
     ; A #= 0    ; B #= 0  ).

jugs_go(Jugs0, Jugs) :-
    length_pred(Jugs0, LenPred),
    [XId, YId] ins 0..LenPred, XId #\= YId, label([XId,YId]),
    nth0(XId, Jugs0, jug(XCap,X0)),
    nth0(YId, Jugs0, jug(YCap,Y0)),
    pair_go(jug(XCap,X0), jug(YCap,Y0),
            jug(XCap,X),  jug(YCap,Y)),
    list_ivs_updated(
        Jugs0,
        [XId-jug(XCap,X), YId-jug(YCap,Y)],
        Jugs).

arc(J1, J2) :-
    jugs_go(J1, J2).

goal([_, jug(_,4), jug(_,4)]).

main(Goal, Path) :-
    Start = [jug(3,0), jug(5,0), jug(8,8)],
    search([nil-Start], [], Goal, Path).
