:- use_module(tools).

new_coor(n , 0, 1, -1).
new_coor(s , 0, -1, 1).
new_coor(ne, 1, 0, -1).
new_coor(sw, -1, 0, 1).
new_coor(nw, -1, 1, 0).
new_coor(se, 1, -1, 0).

change_coor(D, v(X, Y, Z), v(NX, NY, NZ)) :-
  new_coor(D, A, B, C),
  NX is X+A, NY is Y+B, NZ is Z+C.

distance([],    X,  X)  :- !.
distance([D|T], X,  Vs) :-
  change_coor(D, X, NX), distance(T, NX, Vs).

day11a(A) :-
  from_file("Inputs/day11.txt", F),
  distance(F, v(0,0,0), v(X,Y,Z)),
  A is (abs(X)+abs(Y)+abs(Z)) div 2.


maxdis([],    _,          Ac, Ac) :- !.
maxdis([D|T], v(X, Y, Z), Ac, Vs) :-
  change_coor(D, v(X, Y, Z), v(NX, NY, NZ)),
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
