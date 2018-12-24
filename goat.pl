:- [preds].
:- [dfs].

%% A state transition begins with optionally selecting
% something from here, then unload it on the other side,
% or just move to the other side without doing anything
arc(state(Here-L0,     There-K),
    state(There-[X|K], Here-L)) :-
        select(X, L0, L),
        \+(subset([cabbage,goat], L)),
        \+(subset([wolf,goat],    L)).

arc(state(Here-L,  There-K),
    state(There-K, Here-L)) :-
        \+(subset([cabbage,goat], L)),
        \+(subset([wolf,goat],    L)).

goal(state(right-_, left-[])).

main(Goal, Path) :-
    Start = state(left-[wolf,goat,cabbage], right-[]),
    search([nil-Start], [], Goal, Path).
