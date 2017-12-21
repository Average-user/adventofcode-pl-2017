:- use_module(tools).

acc(p(_, _, acc(X,Y,Z)), N) :- maplist(abs, [X,Y,Z], Xs), sum_list(Xs, N).

change(p(pos(P1,P2,P3), vel(V1,V2,V3), acc(A1,A2,A3)),
       p(pos(Px,Py,Pz), vel(Vx,Vy,Vz), acc(A1,A2,A3))) :-
  maplist(plus, [V1,V2,V3], [A1,A2,A3], [Vx,Vy,Vz]),
  maplist(plus, [P1,P2,P3], [Vx,Vy,Vz], [Px,Py,Pz]).

get_pos(p(Pos, _, _), Pos).

no_collisions(Xs, Ys) :-
  findall(p(P,V,A), (member(p(P,V,A), Xs),
                     select(p(P,V,A), Xs, Xs1),
                     not(member(p(P,_,_), Xs1)))
                  , Ys).

run(Ps, 0, Ps) :- !.
run(Ps, N, Xs) :-
  no_collisions(Ps, NPs),
  maplist(change, NPs, Ps1),
  succ(N1, N),
  run(Ps1, N1, Xs).

% day 20 part A solution
day20a(A) :-
  from_file("Inputs/day20.txt", F),
  maplist(acc, F, As),
  min_list(As, M),
  nth0(A, As, M), !.

% day 20 part B solution
day20b(B) :-
  from_file("Inputs/day20.txt", F),
  run(F, 50, Ps),
  length(Ps, B).


% Formating input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(line_p, Lines, F).

line_p(Line, p(pos(A,B,C), vel(D,E,F), acc(G,H,I))) :-
  atom_codes("<>=pav ", Rm),
  atom_codes(Line, LCs),
  exclude(flip(member, Rm), LCs, Cs),
  atom_codes(S, Cs),
  split_string(S, ",", ",", NS),
  maplist(string_to_number, NS, [A,B,C,D,E,F,G,H,I]).
