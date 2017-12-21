:- use_module(tools).

ring(N, C) :-
  A is sqrt(N), B is (A+1) / 2, ceiling(B, C).

numbersH(1, 1) :- !.
numbersH(R, X) :- A is (R-1)*R*8, X is A/2 +1.

numbers(R, Xs) :-
  numbersH(R, N), findall(X, (between(1, N, X), ring(X, R)), Xs).

distanceY(N, D) :- ring(N, R), D is R-1.

ringOf(N, R) :- ring(N, Rn), numbers(Rn, R).

index([H|_], H, 1) :- !.
index([_|T], X, I) :- index(T, X, I2), I is I2+1.

distanceX(N, D) :-
  ring(N, RN),      ringOf(N, R),
  length(R, L),     index(R, N, I),
  SL is L div 4,
  findall(E, (member(E, R), index(R, E, P), M is P mod SL, M =:= 0), C),
  member(Cx, C),    Cx >= N,
  index(R, Cx, Ix), Di is Ix-I+1,
  Da is RN-Di,      D is abs(Da).

get_in(Xs, (X, Y), E) :-
  (X < 0 ; Y < 0) -> E = 0,! ;
  nth0(X, Xs, L), nth0(Y, L, E), !;
  E = 0.

replace([_|T], 0, E, [E|T]) :- !.
replace([H|T], N, E, Xs)    :-
  succ(N1, N), replace(T, N1, E, Xs1), Xs = [H|Xs1].

replace_in([H|T], (0, Y), E, Xs) :- replace(H, Y, E, NH), Xs = [NH|T], !.
replace_in([H|T], (X, Y), E, Xs) :-
  succ(X1, X), replace_in(T, (X1, Y), E, Xs1), Xs = [H|Xs1].

add_ceros(Xs, Ys) :- append([0|Xs], [0], Ys).

amplify(Xs, Ys, (X, Y), (NX, NY)) :-
  length(Xs, AL), L is AL+2,
  findall(N, (between(1,L,_), N = 0), Cs),
  maplist(add_ceros, Xs, NXs),
  append([Cs|NXs], [Cs], Ys),
  NX is X+1, NY is Y+1.

next(right, Xs, (X, Y), (A, B), D) :-
  (A is X-1, get_in(Xs, (A, Y), N), N =:= 0, B = Y, D = up), !;
  B is Y+1, A = X, D = right.

next(left, Xs, (X, Y), (A, B), D) :-
  (A is X+1, get_in(Xs, (A, Y), N), N =:= 0, B = Y, D = down), !;
  B is Y-1, A = X, D = left.

next(up, Xs, (X, Y), (A, B), D) :-
  (B is Y-1, get_in(Xs, (X, B), N), N =:= 0, A = X, D = left), !;
  A is X-1, B = Y, D = up.

next(down, Xs, (X, Y), (A, B), D) :-
  (B is Y+1, get_in(Xs, (X, B), N), N =:= 0, A = X, D = right), !;
  A is X+1, B = Y, D = down.

count_neighbors(Xs, (X, Y), N) :-
  Op = [(0,1),(1,0),(0,-1),(-1,0),(1,1),(-1,1),(1,-1),(-1,-1)],
  findall((A, B), (member((J,K), Op), A is X+J, B is Y+K), Ops),
  maplist(get_in(Xs), Ops, Ns), sum_list(Ns, N).

run(N, Xs, C, Dir, CR, Ac, R) :-
  succ(Ac, NAc),  ring(Ac, Rn),
  (Rn = CR -> EXs = Xs, C1 = C; amplify(Xs, EXs, C, C1)),
  next(Dir, EXs, C1, NC, ND),
  count_neighbors(EXs, NC, V),
  (V > N -> R is V ;  replace_in(EXs, NC, V, NXs),
                      run(N, NXs, NC, ND, Rn, NAc, R)).

% Day 3 part A solution
day03a(A) :-
  from_file("Inputs/day03.txt", N),
  distanceX(N, X),
  distanceY(N, Y),
  A is X+Y.

% Day3 part B solution
day03b(B) :-
  from_file("Inputs/day03.txt", N),
  run(N, [[0,0,0],[0,1,1],[0,0,0]], (1,2), right, 2, 3, B).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [N|_]),
  string_to_number(N, F).
