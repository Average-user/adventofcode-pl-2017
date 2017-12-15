:- module(day12, [nodes_of/2, groups/4]). % comment this if you are
                                          % trying this exercise
:- use_module(tools).

connected_to(Edges, Xs, R) :-
 member(X, Xs), member((X,Y), Edges), not(member(Y, Xs))
   -> Xs1 = [Y|Xs], connected_to(Edges, Xs1, R)
    ; R = Xs.

day12a(A) :-
  from_file("Inputs/day12.txt", F),
  connected_to(F, [0], Connected0),
  length(Connected0, A).


groups([],        _,     Ac, Ac) :- !.
groups([N|Nodes], Edges, Ac, R)  :-
  connected_to(Edges, [N], Cs),
  subtract(Nodes, Cs, NNodes),
  NAc is Ac+1,
  groups(NNodes, Edges, NAc, R).

nodes_of(Edges, Nodes) :-
  maplist(fst, Edges, F), maplist(snd, Edges, S),
  append(F, S, Ns), list_to_set(Ns, Nodes).

day12b(B) :-
  from_file("Inputs/day12.txt", Edges),
  nodes_of(Edges, Nodes),
  groups(Nodes, Edges, 0, B).

% Formating Input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(connections, Lines, Fs),
  maplist(to_relations, Fs, FF),
  flatten(FF, F1),
  list_to_set(F1, F), !.

all_relations(_, [],     []) :- !.
all_relations(A, [R|Rs], Xs) :-
  all_relations(A, Rs, Xs1),
  Xs = [(A, R), (R, A)| Xs1].

to_relations([H,T], Rs) :- all_relations(H, T, Rs).

connections(X, Xs) :-
  split_string(X, "<->", "<->", [H,T]),
  atom_codes(H, HC),
  select(32, HC, H1C),
  number_codes(H1, H1C),
  split_string(T, ", ", ", ", Xs1),
  maplist(string_to_number, Xs1, Xs2),
  Xs = [H1, Xs2].
