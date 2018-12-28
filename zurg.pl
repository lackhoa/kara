:- [preds].
:- [dfs].

time(buzz , 5).
time(woody, 10).
time(rex  , 20).
time(hamm , 25).

%% A state transition begins with selecting
% one or two person from Here, then move to There
% ("Here" is the side with the flashlight)
pred_arc(state(Here-L0, There-K0, Time0),
         Xs,
         TotTime,
         state(There-K, Here-L, Time)
        ) :-
    % Select toys
    select_many(Xs, L0, L),
    % The bridge
    length(Xs, LenXs), LenXs in 1..2,
    % Time concern
    maplist(time, Xs, Ts), max_list(Ts, TotTime),
    Time #= Time0-TotTime, 0 #=< Time,
    % Unload toys
    append(Xs, K0, K).

goal(state(right-_, left-[], _)).

main(Solution) :-
    search(state(left-[buzz,woody,rex,hamm],
                 right-[],
                 60),
           Solution).
