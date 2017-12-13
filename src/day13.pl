:- use_module(tools).

go_trough([],           0) :- !.
go_trough([(D, R)|T],   S) :-
  go_trough(T, S1),
  Z is D mod (2*(R-1)),
  (Z = 0 -> S is S1+(D*R); S = S1).

day13a(A) :- from_file("Inputs/day13.txt", Layers), go_trough(Layers, A).


not_caught_at(Delay, (D, R)) :- X is (Delay+D) mod (2*(R-1)), not(X = 0).

find_delay(N, Layers, X) :-
  maplist(not_caught_at(N), Layers), X = N, !;
  N1 is N+1, find_delay(N1, Layers, X).

day13b(B) :- from_file("Inputs/day13.txt", Layers), find_delay(0, Layers, B).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(line_to_layer, Lines, F).

line_to_layer(Line, (Depth, Range)) :-
  split_string(Line, ": ", ": ", [A, B]),
  number_string(Depth, A),
  number_string(Range, B).
