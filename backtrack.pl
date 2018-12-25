% This is read as "the user wants G0 (the third g),
% so we need to start the computation with a single focused goal of G0 (the second g),
% and to do that we only need to prove the conjunctive goal of G0 (the first g)"
mi_backtrack(G0) :-
    mi_backtrack_([[G0]-G0], G0).

% A single step of computation:
resstep_([A|AsRest], As) :-
    % findall/4 appends As0 at the end for free
    findall(Gs-G,
            % Note that Rest is appended intact (depth first search)
            (A = [G0|Rest]-G, mi_ldclause(G0, Gs, Rest)),
            As,
            AsRest).

mi_backtrack_([[]-G | _], G).  % Proven every necessary
mi_backtrack_(Alts0, G) :-
    resstep_(Alts0, Alts1),
    mi_backtrack_(Alts1, G).
