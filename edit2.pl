:- [preds].

min_edit(As, Bs, Min-Es) :-
    empty_assoc(Assoc0),  % the memoization table
    phrase(min_dist(As, Bs, Min-Es), [Assoc0], _).

min_dist([], [], 0-[])   --> [].
min_dist(As, Bs, Min-Es) -->
    (   state(S0),
        { get_assoc(store(As,Bs), S0, Min-Es) }  % Memoized
    ->   []  % Yes
    ;   { findall(option(Action,Cost,As1,Bs1),
                  edit_option(As, Bs, Action, Cost, As1, Bs1),
                  Options) },
        assess_options(Options, CostOptions),
        state(S0, S),
        { keysort(CostOptions, [Min-Es|_]),
          put_assoc(store(As,Bs), S0, Min-Es, S) }
    ).

% Return a list of choices and their cost
%% It's written like this instead of mapping, because
% every recursive call must refer to the state
assess_options([], []) --> [].
assess_options([option(Action,Cost,As,Bs) | Options],
               [Min-[Action|Es]           | Rest]) -->
    min_dist(As, Bs, Min0-Es),
    { Min #= Min0 + Cost },
    assess_options(Options, Rest).

edit_option([A|As], Bs    , drop(A)  , 1, As, Bs).
edit_option(As    , [B|Bs], insert(B), 1, As, Bs).
edit_option([A|As], [A|Bs], use(A)   , 0, As, Bs).
