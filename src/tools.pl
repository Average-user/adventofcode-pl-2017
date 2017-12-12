:- module(tools, [file_to_lines/2,
                  flip/3,
                  string_to_number/2,
                  split_spaces/2,
                  split_tabs/2,
                  map_to_numbers/2,
                  zip_index/2,
                  zip/3,
                  snd/2,
                  fst/2,
                  splitAt/4,
                  sum/2,
                  to_bin/2]).

file_to_lines(Path, Lines) :-
  open(Path, read, File),
  read_string(File, _, F),
  split_string(F, "\n", "\n", Lines).

flip(Pred, A, B) :- call(Pred, B, A).

string_to_number(S,N) :- atom_codes(S, C), number_codes(N, C).
map_to_numbers(XS, YS) :- maplist(string_to_number, XS, YS).

split_spaces(S, XS) :- split_string(S, " ", " ", XS).
split_tabs(S, XS) :- split_string(S, "\t", "\t", XS).
splitAt(XS, N, L1, L2) :- length(L1, N), append(L1, L2, XS).

zip_index(XS, YS) :-
  length(XS, L),
  L1 is L-1,
  numlist(0,L1,NS),
  zip(NS, XS, YS).

zip([],      _,       []) :- !.
zip(_,       [],      []) :- !.
zip([H1|T1], [H2|T2], YS) :-
  zip(T1, T2, YS1),
  YS = [(H1, H2) | YS1].

snd((_, Y), Y).
fst((X, _), X).

sum([]   , 0) :- !.
sum([H|T], N) :-
  sum(T, N1), N is H+N1.

to_bin(0, []) :- !.
to_bin(N, Xs) :- A is N mod 2, B is N div 2, to_bin(B, Zs), Xs = [A|Zs].
