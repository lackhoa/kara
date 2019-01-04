(define time
  (lambda (x)
    (case x [buzz 1] [woody 2] [rex 3] [hamm 4])))

;; A state transition begins with selecting
;; one or two person from Here, then move to There
;; ("Here" is the side with the flashlight)
(define arcs
  (lambda (tail)
    (run* (weight name head)
      (fresh (here l^ there k^ time^)
        (== tail [list here l^ there k^ time^])
        (fresh (chosen l n)
          ;; Selct for the bridge
          (membero chosen (list (build-num 1) (build-num 2)))
          (lengtho chosen n)
          (select-many chosen l^ l)
          ;; Time concern
          (project (time^ chosen)
            (let* ([tot-time (max (map time chosen))]
                   [time (- time^ tot-time)])))

          ()
          )))))

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
