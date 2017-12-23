:- use_module(tools).
:- set_prolog_stack(local, limit(100 000 000 000)).
:- set_prolog_stack(global, limit(100 000 000 000)).

replace([_|T], 0, E, [E|T]) :- !.
replace([H|T], N, E, Xs)    :-
  succ(N1, N), replace(T, N1, E, Xs1), Xs = [H|Xs1].

replace_in([H|T], (0, Y), E, Xs) :- replace(H, Y, E, NH), Xs = [NH|T], !.
replace_in([H|T], (X, Y), E, Xs) :-
  succ(X1, X), replace_in(T, (X1, Y), E, Xs1), Xs = [H|Xs1].

get_in(Xs, (X, Y), E) :- nth0(X, Xs, Ys), nth0(Y, Ys, E).

add_ceros(Xs, Ys) :- append([0|Xs], [0], Ys).

amplify(Xs, Ys, (X, Y), (NX, NY)) :-
  length(Xs, AL), L is AL+2,
  findall(N, (between(1,L,_), N = 0), Cs),
  maplist(add_ceros, Xs, NXs),
  append([Cs|NXs], [Cs], Ys),
  NX is X+1, NY is Y+1.

move(up,    X,     X).
move(down,  right, left).
move(down,  left,  right).
move(left,  left,  down).
move(left,  right, up).
move(right, left,  up).
move(right, right, down).

reverse(right, left).
reverse(left, right).
reverse(up, down).
reverse(down, up).

forward(up,    (X, Y), (A, Y)) :- A is X-1.
forward(down,  (X, Y), (A, Y)) :- A is X+1.
forward(left,  (X, Y), (X, B)) :- B is Y-1.
forward(right, (X, Y), (X, B)) :- B is Y+1.

run(_,   _,    _,  0, C, C)  :- !.
run(Xs, Coor, Dir, N, C, R) :-
  (get_in(Xs, Coor, 1) -> move(Dir, right, Ndir),
                          replace_in(Xs, Coor, 0, Xs1),
                          Nc is C
                        ; move(Dir, left, Ndir),
                          replace_in(Xs, Coor, 1, Xs1),
                          succ(C, Nc)),
  forward(Ndir, Coor, (X,Y)),
  (not(get_in(Xs1, (X,Y), _)) -> amplify(Xs1, NXs, (X,Y), NCoor)
                               ; NXs = Xs1, NCoor = (X,Y)),
  succ(Nn, N),
  run(NXs, NCoor, Ndir, Nn, Nc, R).

change(0, Xs, Coor, Dir, Nxs, Ndir, f) :-
  replace_in(Xs, Coor, 2, Nxs), move(Dir, left, Ndir).

change(1, Xs, Coor, Dir, Nxs, Ndir, f) :-
  replace_in(Xs, Coor, 3, Nxs), move(Dir, right, Ndir).

change(2, Xs, Coor, Dir, Nxs, Dir, t) :-
  replace_in(Xs, Coor, 1, Nxs).

change(3, Xs, Coor, Dir, Nxs, Ndir, f) :-
  replace_in(Xs, Coor, 0, Nxs), reverse(Dir, Ndir).

run2(_,   _,    _,  0, C, C) :- !.
run2(Xs, Coor, Dir, N, C, R) :-
  get_in(Xs, Coor, N),
  change(N, Xs, Coor, Dir, Xs1, Ndir, B),
  (B = t -> succ(C, Nc) ; Nc = C),
  forward(Ndir, Coor, (X,Y)),
  (not(get_in(Xs1, (X,Y), _)) -> amplify(Xs1, NXs, (X,Y), NCoor)
  ; NXs = Xs1, NCoor = (X,Y)),
  succ(Nn, N),
  run2(NXs, NCoor, Ndir, Nn, Nc, R).

% Day 22 part A solution
day22a(A) :-
  from_file("Inputs/day22.txt", F),
  length(F, L), X is L div 2,
  run(F, (X,X), up, 10000, 0, A), !.

% For now part B is too slow, replacing is not a strong in Prolog lists
% Day 22 part B solution
day22b(B) :-
  from_file("Inputs/day22.txt", F),
  length(F, L), X is L div 2,
  run(F, (X,X), up, 10000000, 0, B), !.

% formating input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(atom_codes, Lines, Lines1),
  maplist(maplist(change), Lines1, F).

change(46, 0).
change(35, 1).
