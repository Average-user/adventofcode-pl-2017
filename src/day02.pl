:- use_module(tools).

dif(XS, X) :- max_list(XS, Ma), min_list(XS, Mi), X is Ma-Mi.

evenlyDiv(XS, R) :-
  member(A, XS),
  member(B, XS),
  not(A=B),
  0 is A mod B,
  R is A div B.


% Day 2.A solution
day02a(S) :- from_file("Inputs/day2.txt", A), maplist(dif, A, B), sum(B, S), !.

% Day 2.B solution.
day02b(S) :-
  from_file("Inputs/day2.txt", A),
  maplist(evenlyDiv, A, B),
  sum_list(B, S), !.

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(split_tabs, Lines, Lists),
  maplist(map_to_numbers, Lists, F).
