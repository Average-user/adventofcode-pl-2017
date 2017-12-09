:- use_module(tools).

max(XS, M) :- member(M, XS), select(M, XS, YS), forall(member(Y, YS), Y =< M).
min(XS, M) :- member(M, XS), select(M, XS, YS), forall(member(Y, YS), Y >= M).

dif(XS, X) :- max(XS, Ma), min(XS, Mi), X is Ma-Mi.

% Day 2.A solution
day02a(S) :- from_file("Inputs/day2.txt", A), maplist(dif, A, B), sum(B, S), !.


evenlyDiv(XS, R) :-
  member(A, XS),
  member(B, XS),
  not(A=B),
  Z is A mod B,
  Z =:= 0,
  R is A div B.

% Day 2.B solution.
day02b(S) :-
  from_file("Inputs/day2.txt", A),
  maplist(evenlyDiv, A, B),
  sum(B, S), !.

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(split_tabs, Lines, Lists),
  maplist(map_to_numbers, Lists, F).
