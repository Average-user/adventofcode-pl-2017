:- use_module(tools).
:- use_module(library(clpfd)).

chunksof(_, [], []) :- !.
chunksof(N, Xs, Ys) :-
  append(A, B, Xs),
  length(A, N),
  chunksof(N, B, Ys1),
  Ys = [A|Ys1], !.

concat([] ,     []) :- !.
concat([C],     C)  :- !.
concat([A,B|T], Xs) :- concat(T, L), append(A, B, X), append(X, L, Xs).

fst_rest([H|T], (H, T)).

join(_, Xs, []) :- flatten(Xs, []),!.
join(N, Xs, Ys) :-
  maplist(fst_rest, Xs, Ss),
  maplist(fst, Ss, Fs),
  maplist(snd, Ss, Rs),
  chunksof(N, Fs, Y),
  join(N, Rs, Ys1),
  append(Y, Ys1, Ys).

horizontalP([X],   X) :- !.
horizontalP([H|T], Y) :- horizontalP(T, Y1), maplist(append, H, Y1, Y).

div(N, Xs, Ys) :-
  maplist(chunksof(N), Xs, Sps),
  chunksof(N, Sps, Sps1),
  maplist(join(N), Sps1, Ys1),
  concat(Ys1, Ys).

paste(Xs, Ys) :-
  length(Xs, L), Z is floor(sqrt(L)),
  chunksof(Z, Xs, Ys1),
  maplist(horizontalP, Ys1, Ys2),
  concat(Ys2, Ys).

match(Rules, Xs, Ys) :-
  member((Rs, Ys), Rules),
  member(Xs, Rs), !.

convert(Rs, Xs, Ys) :-
  length(Xs, L),
  (L mod 2 =:= 0 -> div(2, Xs, Zs) ; div(3, Xs, Zs)),
  maplist(match(Rs), Zs, Zs1), paste(Zs1, Ys).

run(_ , Xs, 0, Xs) :- !.
run(Rs, Xs, N, Ys) :-
  succ(N1, N),
  convert(Rs, Xs, Xs1),
  run(Rs, Xs1, N1, Ys).

count_ones(Xs, Ones) :- flatten(Xs, Ys), include(=(1), Ys, Os), length(Os, Ones).

% day 21 part A solution
day21a(A) :-
  from_file("Inputs/day21.txt", Rs),
  run(Rs, [[0,1,0],[0,0,1],[1,1,1]], 5, G),
  count_ones(G, A).

% day 21 part B solution
day21b(B) :-
  from_file("Inputs/day21.txt", Rs),
  run(Rs, [[0,1,0],[0,0,1],[1,1,1]], 18, G),
  count_ones(G, B).

% Formating input
from_file(Path, F) :- file_to_lines(Path, Lines), maplist(line_to_rule, Lines, F).

line_to_rule(Line, (Ways, Bgd)) :-
  split_string(Line, " =>", " =>", [A, B]),
  split_string(A, "/", "/", As),
  split_string(B, "/", "/", Bs),
  maplist(f, As, Agd),
  maplist(f, Bs, Bgd),
  all_ways(Agd, Ways).

f(X, Y) :- atom_codes(X, Cs), maplist(change, Cs, Y).

change(35, 1).
change(46, 0).

rotate(Xss, Zss) :- transpose(Xss, Yss), maplist(reverse, Yss, Zss).

all_ways(Xs, Ys) :-
  rotate(Xs, Xs1), rotate(Xs1, Xs2), rotate(Xs2, Xs3),
  Rs = [Xs, Xs1, Xs2, Xs3],
  maplist(flipV, Rs, Vs),
  maplist(flipH, Rs, Hs),
  append(Vs, Hs, Fs),
  append(Fs, Rs, All),
  list_to_set(All, Ys).

flipH(Xs, Ys) :- maplist(reverse, Xs, Ys).

flipV(Xs, Ys) :- reverse(Xs, Ys).
