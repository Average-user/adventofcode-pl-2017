:- module(tools, [file_to_lines/2,
                  flip/3,
                  string_to_number/2,
                  split_spaces/2,
                  split_tabs/2,
                  map_to_numbers/2]).

file_to_lines(Path, Lines) :-
  open(Path, read, File),
  read_string(File, _, F),
  split_string(F, "\n", "\n", Lines).

flip(Pred, A, B) :- call(Pred, B, A).

string_to_number(S,N) :- atom_codes(S, C), number_codes(N, C).
map_to_numbers(XS, YS) :- maplist(string_to_number, XS, YS).

split_spaces(S, XS) :- split_string(S, " ", " ", XS).
split_tabs(S, XS) :- split_string(S, "\t", "\t", XS).
