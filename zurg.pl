:- [preds].
:- [dfs].

time(buzz , 5).
time(woody, 10).
time(rex  , 20).
time(hamm , 25).

%% A state transition begins with selecting
% one or two person from Here, then move to There
% ("Here" is the side with the flashlight)
pred_arc(state(Here-L0, There-K, Time0),
         X-with-Y,
         1,
         state(There-[X,Y|K], Here-L, Time)
        ) :-
    select(X, L0, L1), select(Y, L1, L), X @< Y,
    time(X, XT), time(Y, YT),
    TotTime #= max(XT, YT),
    Time #= Time0-TotTime, 0 #=< Time.

pred_arc(state(Here-L0, There-K, Time0),
         only-X,
         0,
         state(There-[X|K], Here-L, Time)) :-
    select(X, L0, L),
    time(X, XT),
    Time #= Time0-XT, 0 #=< Time.

goal(state(right-_, left-[], _)).

main(Solution) :-
    search(state(left-[buzz,woody,rex,hamm],
                 right-[],
                 60),
           Solution).
