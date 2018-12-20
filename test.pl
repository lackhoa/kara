:- [main].

bg_model([]).

?-induce_rlgg([+append([1,2],[3,4],[1,2,3,4]),
               +append([2],[3,4],[2,3,4]),
               +append([],[3,4],[3,4]),
               +append([a],[],[a]),
               +append([x],[z],[x,z]),
               +append([],[],[]),
               -append([a],[b],[b]),
               -append([c],[b],[c,a]),
               -append([1],[3,4],[1,2,3])
              ],
               Clauses).
