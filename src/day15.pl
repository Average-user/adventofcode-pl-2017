:- use_module(tools).
:- set_prolog_stack(local,  limit(2 000 000 000)).

newA(X, A) :- A is (X*16807) mod 2147483647.
newB(X, B) :- B is (X*48271) mod 2147483647.

special_bin(_, _, 16) :- !.
special_bin(A, B, I) :-
  MA is A mod 2, MA is B mod 2,
  X is A div 2, Y is B div 2,
  succ(I, NI), special_bin(X, Y, NI).

matches_till(_, _, 0, M, M) :- !.
matches_till(A, B, N, M, R) :-
  newA(A, NA), newB(B, NB), succ(N1, N),
  (special_bin(A, B, 0) -> succ(M, M1), matches_till(NA, NB, N1, M1, R)
                         ; matches_till(NA, NB, N1, M, R)).

day15a(A) :-
  from_file("Inputs/day15.txt", (GA, GB)),
  matches_till(GA, GB, 40000001, 0, A).


generateAs(_, 0, Ac, Ac) :- !.
generateAs(A, N, Ac, Xs) :-
  newA(A, NA), succ(N1, N),
  (0 is NA mod 4 -> NAc = [NA|Ac], generateAs(NA, N1, NAc, Xs)
                  ; generateAs(NA, N, Ac, Xs)).

match((A,B)) :- special_bin(A, B, 0).

generateBs(_, 0, Ac, Ac) :- !.
generateBs(B, N, Ac, Xs) :-
  newB(B, NB), succ(N1, N),
  (0 is NB mod 8 -> NAc = [NB|Ac], generateBs(NB, N1, NAc, Xs)
                  ; generateBs(NB, N, Ac, Xs)).

day15b(B) :-
  from_file("Inputs/day15.txt", (GA, GB)),
  generateAs(GA, 5000000, [], As),
  generateBs(GB, 5000000, [], Bs),
  zip(As, Bs, Zipped),
  include(match, Zipped, Matches),
  length(Matches, B), !.

% Formatting input.
from_file(Path, F) :-
  file_to_lines(Path, [A1,B1]),
  atom_codes(A1, CA),
  atom_codes(B1, CB),
  append(_, [X,Y,Z], CA),
  append(_, [J,K,L], CB),
  number_codes(NA, [X,Y,Z]),
  number_codes(NB, [J,K,L]),
  F = (NA, NB).
