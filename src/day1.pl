digit(X, D) :-
  member((A,B), [(48,0),(49,1),(50,2),(51,3),(52,4),(53,5),(54,6),(55,7),(56,8),(57,9)]),
  A = X, D = B, !.

sumH([],  0).
sumH([_], 0).
sumH([X,X|YS], S) :- sumH([X|YS], S2), S is X+S2.
sumH([_|XS], S)   :- sumH(XS, S).

sum(XS, S) :-
  sumH(XS, S2), last(XS, L), XS = [H|_],
  (L = H -> S is L + S2 ; S is S2).

partA(S) :- from_file("Data/day1.txt", F), sum(F, S).


elemIndex(I, XS, E) :- length(XS, L), I2 is I mod L, elemIndexH(I2, XS, E).

elemIndexH(0, [H|_], H).
elemIndexH(I, [_|T], E) :- I2 is I-1, elemIndexH(I2, T, E).

containsS(XS, I) :- length(XS, L), L2 is L div 2, I2 is I+L2,
  elemIndex(I2, XS, E2), elemIndex(I, XS, E), E = E2.

sum2H([], 0).
sum2H([H|T], S) :- sum2H(T, S2), S is S2+H.

sum2(XS, S) :- length(XS, L), L1 is L-1, numlist(0, L1, R),
  findall(X, (member(X, R), containsS(XS, X)), Ixs),
  findall(X, (member(Z, Ixs), elemIndex(Z, XS, X)), Es), sum2H(Es, S).


partB(S) :- from_file("Data/day1.txt", F), sum2(F, S).

% main
main((A,B)) :- partA(A), !, partB(B), !.

%% Reading File (formating the input)
from_file(P,R) :-
  open(P,read,Stream), read_string(Stream,_, X),
  atom_codes(X, C), select(10, C, R1), maplist(digit, R1, R).
