ring(N, R) :- A is sqrt(N), B is (A+1) / 2, floor(B, C),
              (B =:= C -> R is C ; R is C+1).

numbersH(1, 1) :- !.
numbersH(R, X) :- A is (R-1)*R*8, X is A/2 +1.

numbers(R, Xs) :- numbersH(R, N), findall(X, (between(1, N, X), ring(X, R)), Xs).

distanceY(N, D) :- ring(N, R), D is R-1.

ringOf(N, R) :- ring(N, Rn), numbers(Rn, R).

index([H|_], H, 0) :- !.
index([_|T], X, I) :- index(T, X, I2), I is I2+1.

distanceX(N, D) :-
  ring(N, RN), ringOf(N, R), length(R, L), index(R, N, I), SL is L div 4,
  findall(E, (member(E, R), index(R, E, P), M is (P+1) mod SL, M =:= 0), C),
  member(Cx, C), Cx >= N, index(R, Cx, Ix), Di is Ix-I, Da is RN-1-Di,
  D is abs(Da).

distance(N, D) :- distanceX(N, X), distanceY(N, Y), D is Y+X.

partA(X) :- from_file("Data/day3.txt", N), distance(N, X).

%% Reading File (formating the input)
from_file(Path, F) :- open(Path,read,A), read_string(A,_,X),
  atom_codes(X, F1), select(10, F1, F2), number_codes(F, F2).
