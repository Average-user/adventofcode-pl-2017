:- use_module(tools).

increment_layer((D, nil, _, _), (D, nil, nil, nil)) :- !.
increment_layer((D, R, H, up), (D, R, NH, ND)) :-
  R = H -> NH is H-1, ND = down, ! ; NH is H+1, ND = up.
increment_layer((D, R, H, down), (D, R, NH, ND)) :-
  0 = H, NH is H+1, ND = up, ! ; NH is H-1, ND = down.

go_trough([],                    Ac, Ac)  :- !.
go_trough([(D, R, H, _)|Layers], Ac, NAc) :-
  maplist(increment_layer, Layers, NLayers),
  (H = 0 -> R1 is R+1, Ac1 = [(D, R1)|Ac]
          ; Ac1 = Ac),
  go_trough(NLayers, Ac1, NAc).

multiply([],        0) :- !.
multiply([(A,B)|T], X) :-
  multiply(T, N), X is N+(A*B).

day13a(A) :-
  from_file("Inputs/day13.txt", Layers),
  go_trough(Layers, [], Seen),
  multiply(Seen, A), !.

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(line_to_layer, Lines, Layers),
  maplist(fst, Layers, Depths),
  max_list(Depths, M),
  fill(0, M, Layers, F).

fill(N, N, Layers, [(N, R, 0, up)]) :- member((N, R), Layers), ! .
fill(N, M, Layers, NLayers)          :-
  N1 is N+1, fill(N1, M, Layers, NL1),
  (member((N, R), Layers) -> NLayers = [(N, R, 0, up)|NL1]
                           ; NLayers = [(N, nil, nil, nil)|NL1]).

line_to_layer(Line, (Depth, Range)) :-
  split_string(Line, ": ", ": ", [A, B]),
  number_string(Depth, A),
  number_string(Range1, B),
  Range is Range1-1.
