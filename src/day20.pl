:- use_module(tools).

next(p(pos(P1,P2,P3), vel(V1,V2,V3), acc(A1,A2,A3)),
     p(pos(Px,Py,Pz), vel(Vx,Vy,Vz), acc(A1,A2,A3))) :-
  Vx is V1+A1, Vy is V2+A2, Vz is V3+A3,
  Px is P1+Vx, Py is P2+Vy, Pz is P3+Vz.

distance((N, p(pos(X,Y,Z), Vel, Acc)), (N, [p(pos(X,Y,Z), Vel, Acc), D])) :- 
  maplist(abs, [X,Y,Z], A), sum_list(A, D).

change((N, P), (N, NP)) :- next(P, NP).

do_times(Ps, 0, Ps) :- !.
do_times(Ps, N, Xs) :-
  succ(N1, N),
  maplist(change, Ps, Ps1),
  do_times(Ps1, N1, Xs).

get_pos((_, p(Pos, _, _)), Pos).

collide(Ps, NPs) :-
  maplist(get_pos, Ps, Poss),
  collideH(Ps, Poss, NPs).

more_than_once(X, Xs) :- include(=(X), Xs, XX), length(XX, L), L > 1.

collideH([],     _,    []) :- !.
collideH([(N, P)|Ps], Poss, Xs) :-
  collideH(Ps, Poss, Xs1),
  get_pos((N, P), Pos),
  (more_than_once(Pos, Poss) -> Xs = Xs1
                              ; Xs = [(N, P)|Xs1]). 

run(Ps, 0, Ps) :- !.
run(Ps, N, Xs) :-
  collide(Ps, NPs),
  maplist(change, NPs, Ps1),
  succ(N1, N),
  run(Ps1, N1, Xs).


day20a(A) :-
  from_file("Inputs/day20.txt", F),
  do_times(F, 500, F1),
  maplist(distance, F1, Ds),
  maplist(snd, Ds, Ps),
  maplist(second, Ps, D),
  min_list(D, M),
  memberchk((A, [_,M]), Ds).

day20b(B) :-
  from_file("Inputs/day20.txt", F),
  run(F, 50, Ps),
  length(Ps, B).


% Formating input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  length(Lines, L),
  numlist(0, L, Ns),
  maplist(line_particule, Lines, Ps),
  zip(Ns, Ps, F).

line_particule(Line, p(pos(X1, Y1, Z1), vel(X2,Y2,Z2), acc(X3,Y3,Z3))) :-
  split_string(Line, " ", " ", [P,V,A]),
  numbersof(P, [X1,Y1,Z1]),
  numbersof(V, [X2,Y2,Z2]),
  numbersof(A, [X3,Y3,Z3]).

numbersof(X, N) :-
  atom_codes(X, [_|T]),
  atom_codes('<>pav=', E),
  exclude(flip(member, E), T, Cs),
  string_codes(Cs, S),
  split_string(S, ",", ",", Ns),
  maplist(flip(number_codes), Ns, N).
  
