:- use_module(tools).

sumH([],  0).
sumH([_], 0).
sumH([X,X|YS], S) :- sumH([X|YS], S2), S is X+S2.
sumH([_|XS], S)   :- sumH(XS, S).

sumA([H|T], S) :-
  sumH([H|T], S2),
  (last(T, H) -> S is S2+H ; S = S2).

elemIndex(I, XS, E) :-
  length(XS, L),
  I2 is I mod L,
  nth0(I2, XS, E).

containsS(XS, I) :-
  length(XS, L),
  L2 is L div 2,
  I2 is I+L2,
  elemIndex(I2, XS, E2),
  elemIndex(I, XS, E),
  E = E2.

sum2(XS, S) :-
  length(XS, L),
  L1 is L-1,
  numlist(0, L1, R),
  findall(X, (member(X, R), containsS(XS, X)), Ixs),
  findall(X, (member(Z, Ixs), elemIndex(Z, XS, X)), Es),
  sum_list(Es, S).


% Day 1.A solution
day01a(S) :- from_file("Inputs/day01.txt", F), sumA(F, S), !.

% Day 1.B solution
day01b(S) :- from_file("Inputs/day01.txt", F), sum2(F, S), !.

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [N|_]),
  atom_codes(N, Codes),
  maplist(digit, Codes, F).

digit(X, D) :- number_codes(D, [X]).
