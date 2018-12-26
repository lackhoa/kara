:- [preds].

tree_build([A1,A2|As], test(Trait, Yes, No)) :-
    Candidates = [A1,A2|As],
    all_traits(AllTraits),
    findall(Delta-T-Pos,
            ( member(T, AllTraits),
              trait_positive(T, AllPos),
              intersection(AllPos, Candidates, Pos),
              length(Pos, PL),
              length(Candidates, CL),
              Delta is abs((PL/CL) - 0.5) ),
            DTPs),
    findall(Delta, member(Delta-T-_, DTPs), Deltas),
    min_list(Deltas, Min_Delta),
    once(member(Min_Delta-Trait-Positive, DTPs)),
    tree_build(Positive, Yes),
    subtract(Candidates, Positive, Negative),
    tree_build(Negative, No).

% Found unique candidate
tree_build([A], leaf(A)).

animal_trait([  dog     - [fur,woof,tetra,live,birth,cute],
                cat     - [fur,meow,tetra,live,birth,cute],
                duck    - [feather,quack,biped,live,lay_eggs],
                man     - [biped,live,think,birth],
                android - [biped,think],
                ai      - [think,perfect],
                god     - [biped,think,perfect]
             ]).

all_traits(Traits) :-
    animal_trait(ATs),
    findall(TraitList, member(_-TraitList, ATs), TraitLists),
    foldl(union, TraitLists, [], Traits).

all_animals(Animals) :-
    animal_trait(ATs),
    findall(Animal, member(Animal-_, ATs), Animals).

trait_positive(Trait, Positive) :-
    animal_trait(ATs),
    findall(Animal,
            (member(Animal-Traits, ATs), member(Trait, Traits)),
            Positive).

main(Tree) :-
    all_animals(All_Animals),
    tree_build(All_Animals, Tree).
