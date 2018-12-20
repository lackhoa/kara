:- [preds].

children(Node, Children):-
    % Children are paths
    findall(C, arc(Node, C), Children).

arc(T, [H | T]) :-
    % nodes are lists of letters
    length(T, N), N<5,
    member(H, [a, d, i, m]).

% find palindromes
goal(L) :-
    reverse(L, L).

% depth-first search with loop detection

main() :-
    search_df_loop([[]], []).

search_df_loop([Goal | _], _) :-
    % The `Goal` at the end is just for querying purpoes
    goal(Goal),
    print(Goal).

search_df_loop([Current | Rest], Visited) :-
    % Agenda contains paths
    children(Current, Children),
    add_df(Children, Rest, Visited, NewAgenda),
    search_df_loop(NewAgenda, [Current | Visited]).

add_df([], Agenda, _, Agenda).

add_df([Child | Rest], OldAgenda, Visited, NewAgenda):-
    (element(Child, OldAgenda);
     element(Child, Visited)),
    !,
    add_df(Rest, OldAgenda, Visited, NewAgenda).

add_df([Child | Rest], OldAgenda, Visited, [Child | NewAgenda]) :-
    add_df(Rest, OldAgenda, Visited, NewAgenda).
