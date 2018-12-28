:- [preds].
:- [dfs].

pred_arc([A|As]-[A|Bs], use(A)   , 0, As-Bs).
pred_arc(As-[B|Bs]    , insert(B), 1, As-Bs).
pred_arc([A|As]-Bs    , drop(A)  , 1, As-Bs).

goal([]-[]).

queue_algo(bfs).

main(Solution) :-
    search([a,b,c,e,f]-[a,x,b,d,e,f],
           Solution).
