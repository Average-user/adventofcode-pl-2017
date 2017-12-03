max(XS, M) :- member(M, XS), select(M, XS, YS), forall(member(Y, YS), Y =< M).
min(XS, M) :- member(M, XS), select(M, XS, YS), forall(member(Y, YS), Y >= M).

sum([], 0).
sum([H|T], S) :- sum(T, S2), S is S2+H.

dif(XS, X) :- max(XS, Ma), min(XS, Mi), X is Ma-Mi.

partA(S) :- from_file("Data/day2.txt", A), maplist(dif, A, B), sum(B, S).


evenlyDiv(XS, R) :-
  member(A, XS), member(B, XS), not(A=B), Z is A mod B, Z =:= 0, R is A div B.

partB(S) :- from_file("Data/day2.txt", A), maplist(evenlyDiv, A, B), sum(B, S).

% main
main((A,B)) :- partA(A), !, partB(B), !.

%% Reading File (formating the input)
from_file(Path, F) :-
  read_file(Path, A), split_string(A, "\n", "", B), last(B, L), select(L, B, C),
  maplist(split_numbers, C, F).

split_numbers(String, R) :-
  split_string(String, "\t", "", A), maplist(string_to_number, A, R).

read_file(P,X) :- open(P,read,A), read_string(A,_,X).
string_to_number(S,N) :- atom_codes(S, C), number_codes(N, C).
