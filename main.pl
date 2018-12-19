:-op(600,xfx,'->'). % operator for inverse substitution

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
theta_lgg((H1 :- B1), (H2 :- B2), (H :- B)) :-
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
    same_predicate(L1, L2),
    anti_unify(L1, L2, L, S, Sp),
    theta_lgg_literal(L1, B2, [L | B], Bp, Sp, Spp).

theta_lgg_literal(L1, [L2 | B2], B, Bp, S, Sp) :-
    not same_predicate(L1, L2),
    theta_lgg_literal(L1, B2, B, Bp, S, Sp).

same_predicate(L1, L2) :- functor(L1, P, N), functor(L2, P, N).






%% % rlgg(E1, E2, M, C) <- C is RLGG of E1 and E2 relative to M

%% rlgg(E1, E2, M, (H :- B)) :-
%%     anti_unify(E1, E2, H, [], S10, [], S20),
%%     varsin(H, V),  % determine variables in head of clause
%%     rlgg_bodies(M, M, [], B, S10, S1, S20, S2, V).

%% % varsin(T, V) <- V is list of variables occuring in term T
%% %               (standard predicate in many Prologs)

%% rlgg_bodies([], B2, B, B, S1, S1, S2, S2, V).

%% rlgg_bodies([L|B1], B2, B0, B, S10, S1, S20, S2, V) :-
%%     rlgg_literal(L, B2, B0, B00, S10, S11, S20, S21, V),
%%     rlgg_bodies(B1, B2, B00, B, S11, S1, S21, S2, V).

%% rlgg_literal(L1, [], B, B, S1, S1, S2, S2, V).

%% rlgg_literal(L1, [L2|B2], B0, B, S10, S1, S20, S2, V) :-
%%     same_predicate(L1, L2),
%%     anti_unify(L1, L2, L, S10, S11, S20, S21),
%%     varsin(L, Vars),
%%     var_proper_subset(Vars, V),    % no new variables
%%     !, rlgg_literal(L1, B2, [L|B0], B, S11, S1, S21, S2, V).

%% rlgg_literal(L1, [L2|B2], B0, B, S10, S1, S20, S2, V) :-
%%     rlgg_literal(L1, B2, B0, B, S10, S1, S20, S2, V).

%% var_proper_subset([],Ys) :-
%%     Ys \= [].

%% var_proper_subset([X|Xs],Ys) :-
%%     var_remove_one(X,Ys,Zs),
%%     var_proper_subset(Xs,Zs).
