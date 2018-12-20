:- [preds].

:- op(600,xfx,'->'). % operator for inverse substitution

% anti_unify(T1, T2, T) <-  T is the anti-unification
%                         of T1 and T2
anti_unify(T1, T2, T) :-
    anti_unify(T1, T2, T, [], _S).

% anti-unification with inverse subs and accumulators
anti_unify(T1, T2, T1, S, S) :-
    T1 == T2, !.                        % same terms

anti_unify(T1, T2, V, S, S) :-
    subs_lookup(S, T1, T2, V), !.      % already substituted

anti_unify(T1, T2, T, S, Sp) :-
    nonvar(T1), nonvar(T2),
    % Same functor
    functor(T1, F, N), functor(T2, F, N), !,
    functor(T, F, N),
    anti_unify_args(N, T1, T2, T, S, Sp).

anti_unify(T1, T2, V, S, [[T1, T2]->V | S]).

anti_unify_args(0, _T1, _T2, _T, S, S).

anti_unify_args(N, T1, T2, T, S, Spp) :-
    N>0, N1 is N-1,
    arg(N, T1, Arg1), arg(N, T2, Arg2), arg(N, T, Arg),
    anti_unify(Arg1, Arg2, Arg, S, Sp),
    anti_unify_args(N1, T1, T2, T, Sp, Spp).

subs_lookup([[T1, T2]->V | _S2], Term1, Term2, V) :-
    T1 == Term1, T2 == Term2, !.  % no alternative solutions needed

subs_lookup([_ | S1], T1, T2, V) :-
    subs_lookup(S1, T1, T2, V).






:-op(900, fy, not).

% theta_lgg(C1, C2, C) <- C is the θ-LGG of clause C1 and C2
theta_lgg(H1 :- B1, H2 :- B2, H :- B) :-
    anti_unify(H1, H2, H, [], S),            % heads
    theta_lgg_bodies(B1, B2, [], B, S, _Sp). % bodies

% select literal from first body...
theta_lgg_bodies([], _B2, B, B, S, S).

theta_lgg_bodies([L | B1], B2, B, Bpp, S, Spp) :-
    theta_lgg_literal(L, B2, B, Bp, S, Sp),
    theta_lgg_bodies(B1, B2, Bp, Bpp, Sp, Spp).

% and one from second body
theta_lgg_literal(_L1, [], B, B, S, S).

theta_lgg_literal(L1, [L2 | B2], B, Bp, S, Spp) :-
    same_predicate(L1, L2), !,
    anti_unify(L1, L2, L, S, Sp),
    theta_lgg_literal(L1, B2, [L | B], Bp, Sp, Spp).

theta_lgg_literal(L1, [_L2 | B2], B, Bp, S, Sp) :-
    theta_lgg_literal(L1, B2, B, Bp, S, Sp).

same_predicate(L1, L2) :- functor(L1, P, N), functor(L2, P, N).





% rlgg(E1, E2, M, C) <- C is RLGG of E1 and E2 relative to M
rlgg(E1, E2, M, H :- B) :-
    anti_unify(E1, E2, H, [], S),
    term_variables(H, V),
    rlgg_bodies(M, M, [], B, S, _Sp, V).

rlgg_bodies([], _, B, B, S, S, _).

rlgg_bodies([L | B1], B2, B, Bpp, S, Spp, V) :-
    rlgg_literal(L, B2, B, Bp, S, Sp, V),
    rlgg_bodies(B1, B2, Bp, Bpp, Sp, Spp, V).

rlgg_literal(_, [], B, B, S, S, _).

rlgg_literal(L1, [L2 | B2], B, Bp, S, Spp, V) :-
    same_predicate(L1, L2),
    anti_unify(L1, L2, L, S, Sp),
    term_variables(L, Vars), var_proper_subset(Vars, V),
    !,
    rlgg_literal(L1, B2, [L | B], Bp, Sp, Spp, V).

rlgg_literal(L1, [_ | B2], B, Bp, S, Sp, V) :-
    rlgg_literal(L1, B2, B, Bp, S, Sp, V).








induce_rlgg(Exs, Clauses) :-
    pos_neg(Exs, Poss, Negs),
    bg_model(BG), append(Poss, BG, Model),
    covering(Poss, Negs, Model, [], Clauses).


% split positive and negative examples
pos_neg([], [], []).

pos_neg([+E | Exs], [E | Poss], Negs) :-
    pos_neg(Exs, Poss, Negs).

pos_neg([-E | Exs], Poss, [E | Negs]) :-
    pos_neg(Exs, Poss, Negs).


% covering algorithm
covering(Poss, Negs, Model, H, Hp) :-
    construct_hypothesis(Poss, Negs, Model, Hyp),
    !,
    remove_pos(Poss, Model, Hyp, NewPoss),
    covering(NewPoss, Negs, Model, [Hyp | H], Hp).

% add uncovered examples to hypothesis
covering(P, _, _, H, Hp) :-
    append(H, P, Hp).

% remove covered positive examples
remove_pos([], _, _, []).

remove_pos([P | Ps], Model, Hyp, NewP) :-
    covers_ex(Hyp, P, Model),
    !,
    write('Covered example: '), write(P), nl,
    remove_pos(Ps, Model, Hyp, NewP).

remove_pos([P | Ps], Model, Hyp, [P | NewP]) :-
    remove_pos(Ps, Model, Hyp, NewP).


% extensional coverage, relative to a ground model
covers_ex(Head :- Body, Example, Model) :-
    try((Head = Example, forall(element(L, Body),
                           element(L, Model)))).


% construct a hypothesis clause by means of RLGG
construct_hypothesis([E1, E2 | _], Negs, Model, Clause) :-
    write('RLGG of '), write(E1), write(' and '), write(E2), write(' is'),
    rlgg(E1, E2, Model, Cl),
    reduce(Cl, Negs, Model, Clause),
    !,
    nl, tab(5), portray_clause(Clause), nl.

construct_hypothesis([_, E2 | Es], Negs, Model, Clause) :-
    write(' too general'), nl,
    construct_hypothesis([E2 | Es], Negs, Model, Clause).


% remove redundant literals
reduce(H :- B, Negs, M, H :- Bpp) :-
    setof0(L,
           (element(L, B), not var_element(L, M)),
           Bp),
    reduce_negs(H, Bp, [], Bpp, Negs, M).

reduce_negs(H, [_ | B], In, Bp, Negs, M) :-
    append(In, B, Body),
    not covers_neg(H :- Body, Negs, M),
    !,  % remove L
    reduce_negs(H, B, In, Bp, Negs, M).

reduce_negs(H, [L | B], In, Bp, Negs, M) :-
    % keep L
    reduce_negs(H, B, [L | In], Bp, Negs, M).

reduce_negs(H, [], Body, Body, Negs, M) :-
    % fail if clause covers negatives
    not covers_neg(H :- Body, Negs, M).

covers_neg(Clause, Negs, Model) :-
    element(N, Negs),
    covers_ex(Clause, N, Model).
