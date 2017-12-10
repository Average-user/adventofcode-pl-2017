:- use_module(tools).


nth([H|_], 0, H) :- !.
nth([_|T], N, E) :- N1 is N-1, nth(T, N1, E).

nth1(XS, N, E) :- N1 is N mod 256, nth(XS, N1, E).

replaceIn([_|T], 0, E, [E|T]) :- !.
replaceIn([H|T], N, E, Xs) :-
  N1 is N-1,
  replaceIn(T, N1, E, Xs1),
  Xs = [H|Xs1].

replaceIn1(Xs, N, E, Rs) :-
  N1 is N mod 256, replaceIn(Xs, N1, E, Rs).

replaceAll([],        []          , Xs, Xs) :- !.
replaceAll([I|Index], [E|Elements], Xs, Rs) :-
  replaceIn1(Xs, I, E, Xs1),
  replaceAll(Index, Elements, Xs1, Rs).

process(Xs, []         , _, _   , _ , 1, Xs) :- !.
process(Xs, []         , I, Skip, LS, C, Rs) :-
  C1 is C-1,
  process(Xs, LS, I, Skip, LS, C1, Rs).
process(Xs, [L|Lengths], I, Skip, Ls, C, Rs) :-
  (L = 0 -> L1 = I ; L1 is (L+I-1)),
  numlist(I, L1, ToTake),
  maplist(nth1(Xs), ToTake, Ns),
  reverse(Ns, Ns1),
  replaceAll(ToTake, Ns1, Xs, NXs),
  NSkip is Skip+1,
  NI    is Skip+I+L,
  process(NXs, Lengths, NI, NSkip, Ls, C, Rs).

day10a(A) :-
  from_file("Inputs/day10.txt", F1),
  split_string(F1, ",", ",", F2),
  maplist(string_to_number, F2, F),
  numlist(0,255, Xs),
  process(Xs, F, 0, 0, F, 1, [X,Y|_]),
  A is X*Y.

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [F|_]).
