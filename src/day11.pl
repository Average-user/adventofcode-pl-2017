:- use_module(tools).

distance([], X, X) :- !.
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


maxdis([], _, Ac, Ac) :- !.

maxdis([n|T], v(X, Y, Z), Ac, Vs) :-
  Y1 is Y+1, Z1 is Z-1,
  A is (abs(X)+abs(Y1)+abs(Z1)) div 2,
  Ac1 = [A|Ac],
  maxdis(T, v(X, Y1, Z1), Ac1, Vs), !.

maxdis([s|T], v(X, Y, Z), Ac, Vs) :-
  Y1 is Y-1, Z1 is Z+1,
  A is (abs(X)+abs(Y1)+abs(Z1)) div 2,
  Ac1 = [A|Ac],
  maxdis(T, v(X, Y1, Z1), Ac1, Vs), !.

maxdis([ne|T], v(X, Y, Z), Ac, Vs) :-
  X1 is X+1, Z1 is Z-1,
  A is (abs(X1)+abs(Y)+abs(Z1)) div 2,
  Ac1 = [A|Ac],
  maxdis(T, v(X1, Y, Z1), Ac1, Vs), !.

maxdis([sw|T], v(X, Y, Z), Ac, Vs) :-
  X1 is X-1, Z1 is Z+1,
  A is (abs(X1)+abs(Y)+abs(Z1)) div 2,
  Ac1 = [A|Ac],
  maxdis(T, v(X1, Y, Z1), Ac1, Vs), !.

maxdis([nw|T], v(X, Y, Z), Ac, Vs) :-
  X1 is X-1, Y1 is Y+1,
  A is (abs(X1)+abs(Y1)+abs(Z)) div 2,
  Ac1 = [A|Ac],
  maxdis(T, v(X1, Y1, Z), Ac1, Vs), !.

maxdis([se|T], v(X, Y, Z), Ac, Vs) :-
  X1 is X+1, Y1 is Y-1,
  A is (abs(X1)+abs(Y1)+abs(Z)) div 2,
  Ac1 = [A|Ac],
  maxdis(T, v(X1, Y1, Z), Ac1, Vs), !.

day11b(B) :-
  from_file("Inputs/day11.txt", F),
  maxdis(F, v(0,0,0), [], Ac),
  max_list(Ac, B).


%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [X|_]),
  split_string(X, ",", ",", F1),
  maplist(string_to_atom, F1, F).
