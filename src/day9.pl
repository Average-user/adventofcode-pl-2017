:- use_module(tools).

gopened(60).
gclosed(62).

ignore_stuff([],      []) :- !.
ignore_stuff([X],     [X]) :- !.
ignore_stuff([A,B|T], XS) :-
  atom_codes('!', [I]),
  A = I -> ignore_stuff(T, XS)
         ; ignore_stuff([B|T], XS1),
           XS = [A|XS1].

erase_garbage([],    _, []).
erase_garbage([H|T], f, Xs) :-
  gopened(H) -> erase_garbage(T, t, Xs)
              ; erase_garbage(T, f, Xs1),
                Xs = [H|Xs1].
erase_garbage([H|T], t, Xs) :-
  gclosed(H) -> erase_garbage(T, f, Xs)
              ; erase_garbage(T, t, Xs).

clean([],    []) :- !.
clean([H|T], Xs) :-
  atom_codes('{}', Gs),
  member(H, Gs) -> clean(T, Xs1),
                   Xs = [H|Xs1]
                 ; clean(T, Xs).

find_gps([],          _, 0, []) :- !.
find_gps([X],         _, 0, [X]) :- !.
find_gps([123,125|T], C, C, T) :- !.
find_gps([H|T]      , C, S, Xs) :-
  C1 is C+1, find_gps(T, C1, S, Xs1),
  Xs = [H|Xs1].

sum_gps(Xs, A, S) :-
  find_gps(Xs, 1, C, Nxs),
  (C = 0 -> S = A
          ; A1 is A+C,
            sum_gps(Nxs, A1, S)).

% Part 9.A solution
partA(A) :-
  from_file("Inputs/day9.txt", F),
  ignore_stuff(F, Is),
  erase_garbage(Is, f, Gs),
  clean(Gs, Cs),
  sum_gps(Cs, 0, A), !.


count_garbage([],    _, C, C).
count_garbage([H|T], f, C, S) :-
  gopened(H) -> count_garbage(T, t, C, S)
              ; count_garbage(T, f, C, S).
count_garbage([H|T], t, C, S) :-
  gclosed(H) -> count_garbage(T, f, C, S)
              ; C1 is C+1,
                count_garbage(T, t, C1, S).

% Part 9.B solution.
partB(B) :-
  from_file("Inputs/day9.txt", F),
  ignore_stuff(F, Is),
  count_garbage(Is, f, 0, B), !.

% complete Day 9 solution.
main((A, B)) :- partA(A), partB(B).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [X|_]),
  atom_codes(X, F).
