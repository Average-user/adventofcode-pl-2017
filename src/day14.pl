:- use_module(tools).
:- use_module(day10).
:- use_module(day12).

hex_dig(48,  [0,0,0,0]).
hex_dig(49,  [0,0,0,1]).
hex_dig(50,  [0,0,1,0]).
hex_dig(51,  [0,0,1,1]).
hex_dig(52,  [0,1,0,0]).
hex_dig(53,  [0,1,0,1]).
hex_dig(54,  [0,1,1,0]).
hex_dig(55,  [0,1,1,1]).
hex_dig(56,  [1,0,0,0]).
hex_dig(57,  [1,0,0,1]).
hex_dig(97,  [1,0,1,0]).
hex_dig(98,  [1,0,1,1]).
hex_dig(99,  [1,1,0,0]).
hex_dig(100, [1,1,0,1]).
hex_dig(101, [1,1,1,0]).
hex_dig(102, [1,1,1,1]).

to_bin128(Atom, Bin) :-
  atom_codes(Atom, Cs), maplist(hex_dig, Cs, Bins), flatten(Bins, Bin).

ones_on_line(Atom, X) :-
  to_knot(Atom, Knot),
  to_bin128(Knot, B),
  sum_list(B, X).

day14a(A) :-
  from_file("Inputs/day14.txt", F),
  maplist(ones_on_line, F, Ones),
  sum_list(Ones, A).

get_in(Xs, (X, Y), E) :- nth0(X, Xs, A),  nth0(Y, A, E).

line_coors([],    _, _, []) :- !.
line_coors([H|T], X, Y, Xs) :-
  succ(Y, Y1), line_coors(T, X, Y1, Coors),
  (H = 1 -> Xs = [(X, Y)|Coors] ; Xs = Coors).

grid_coors_one([],    _, []) :- !.
grid_coors_one([H|T], X, Xs) :-
  succ(X, X1),
  grid_coors_one(T, X1, Coors),
  line_coors(H, X, 0, L),
  Xs = [L|Coors].

ad(0,  1).
ad(0, -1).
ad(1,  0).
ad(-1, 0).

valid_neighbors(Coors, (X, Y), Xs) :-
  findall([U, V], (ad(A, B), U is X+A, V is Y+B, member((U, V), Coors)), Xs).

all_edges(Coors, (X, Y), Xs) :-
  valid_neighbors(Coors, (X, Y), Ns),
  findall(([X, Y], J), member(J, Ns), A),
  findall((I, [X, Y]), member(I, Ns), B),
  append(A, B, Xs).

grid_edges(Grid, Edges) :-
  grid_coors_one(Grid, 0, Coors1),
  flatten(Coors1, Coors),
  maplist(all_edges(Coors), Coors, Edges1),
  flatten(Edges1, Edges2),
  list_to_set(Edges2, Edges).

day14b(B) :-
  from_file("Inputs/day14.txt", F),
  maplist(to_knot, F, Knots),
  maplist(to_bin128, Knots, Grid),
  grid_coors_one(Grid, 0, Ones1),
  flatten(Ones1, Ones),
  grid_edges(Grid, Edges),
  nodes_of(Edges, Nodes),
  length(Nodes, LN),
  length(Ones, LO),
  groups(Nodes, Edges, 0, Gps),
  B is (LO-LN)+Gps.

% Formatting input.
from_file(Path, F) :-
  file_to_lines(Path, [X|_]),
  atom_codes(X, Codes1),
  append(Codes1, [45], Codes),
  construct(Codes, F).

list(X, [X]).

construct(Key_Word, Words) :-
  numlist(0, 127, Ns),
  maplist(number_codes, Ns, Ns1),
  maplist(append(Key_Word), Ns1, Words1),
  maplist(flip(atom_codes), Words1, Words).
