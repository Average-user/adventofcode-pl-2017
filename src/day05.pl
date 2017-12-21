:- use_module(tools).
:- set_prolog_stack(local, limit(100 000 000 000)).
:- set_prolog_stack(global, limit(100 000 000 000)).

take_drop(X, Xs, Ts, Ds) :- append(Ts, Ds, Xs), length(Ts, X), !.

divide(Xs, Ys, N) :- length(Xs, L), L < N, Ys = [Xs], !.
divide(Xs, Ys, N) :-
  take_drop(N, Xs, Ts, Ds),
  divide(Ds, Ys1, N),
  Ys = [Ts|Ys1].

modify(P, [X|Xs], (0, Y), NXs, E) :- modifyX(P, X, Y, NX, E), NXs = [NX|Xs].
modify(P, [H|T],  (X, Y), NXs, E) :-
  succ(X1, X),
  modify(P, T, (X1, Y), NXs1, E),
  NXs = [H|NXs1].

modifyX(b, [H|T], 0, [H1|T], H) :- H > 2 -> H1 is H-1 ; H1 is H+1, !.
modifyX(_, [H|T], 0, [H1|T], H) :- H1 is H+1, !.
modifyX(P, [H|T], N, XS,     X) :-
  N1 is N-1, modifyX(P, T, N1, XS1, X), XS = [H|XS1].

changeX(P, I, XS, NI, NXS) :-
  X is I div 60,
  Y is I mod 60,
  modify(P, XS, (X, Y), NXS, E),
  NI is I+E.

changesA( XS, I, AC, Z) :-
  changeX(a, I, XS, NI, NXS),
  AC1 = AC+1,
  changesA(NXS, NI, AC1, Z), !;
  Z is AC-1.

changesB( XS, I, AC, Z) :-
  changeX(b, I, XS, NI, NXS),
  AC1 = AC+1,
  changesB(NXS, NI, AC1, Z), !;
  Z is AC-1.

/* | I implemented a new solution that instead of take 5 minutes to run part A,
   | it just takes 15 seconds. But isn't enough for part B, I think thats
   | because it uses too much memory                                          */
day05a(A) :- from_file("Inputs/day05.txt", I),
  divide(I, Xs, 60), changesA(Xs, 0, 1, A).

day05b(B) :- from_file("Inputs/day05.txt", I),
  divide(I, Xs, 60), changesB(Xs, 0, 1, B).


%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(flip(number_codes), Lines, F).
