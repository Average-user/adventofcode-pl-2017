:- use_module(tools).

distance([],    X,          X)  :- !.
distance([n|T], v(X, Y, Z), Vs) :-
  Y1 is Y+1, Z1 is Z-1, distance(T, v(X, Y1, Z1), Vs), !.
distance([s|T], v(X, Y, Z), Vs) :-
  Y1 is Y-1, Z1 is Z+1, distance(T, v(X, Y1, Z1), Vs), !.
distance([ne|T], v(X, Y, Z), Vs) :-
  X1 is X+1, Z1 is Z-1, distance(T, v(X1, Y, Z1), Vs), !.
distance([sw|T], v(X, Y, Z), Vs) :-
  X1 is X-1, Z1 is Z+1, distance(T, v(X1, Y, Z1), Vs), !.
distance([nw|T], v(X, Y, Z), Vs) :-
  X1 is X-1, Y1 is Y+1, distance(T, v(X1, Y1, Z), Vs), !.
distance([se|T], v(X, Y, Z), Vs) :-
  X1 is X+1, Y1 is Y-1, distance(T, v(X1, Y1, Z), Vs), !.

day11a(A) :-
  from_file("Inputs/day11.txt", F),
  distance(F, v(0,0,0), v(X,Y,Z)),
  A is (abs(X)+abs(Y)+abs(Z)) div 2.


new_coor(n , 0, 1, -1).
new_coor(s , 0, -1, 1).
new_coor(ne, 1, 0, -1).
new_coor(sw, -1, 0, 1).
new_coor(nw, -1, 1, 0).
new_coor(se, 1, -1, 0).

maxdis([],    _,          Ac, Ac) :- !.
maxdis([E|T], v(X, Y, Z), Ac, Vs) :-
  new_coor(E, A, B, C),
  NX is X+A, NY is Y+B, NZ is Z+C,
  AE is (abs(NX)+abs(NY)+abs(NZ)) div 2,
  Ac1 = [AE|Ac],
  maxdis(T, v(NX, NY, NZ), Ac1, Vs), !.

day11b(B) :-
  from_file("Inputs/day11.txt", F),
  maxdis(F, v(0,0,0), [], Ac),
  max_list(Ac, B).


%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [X|_]),
  split_string(X, ",", ",", F1),
  maplist(string_to_atom, F1, F).
