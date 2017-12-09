:- use_module(tools).

filter([], []).
filter([H|T], R) :-
  list_to_set(H, H) ->
    filter(T, R1),
    R = [H|R1]
    ; filter(T, R).

% Day 4.A solution
day04a(X) :- from_file("Inputs/day4.txt", F), filter(F, Fi), length(Fi, X).


anagram(X, XS) :-
  maplist(atom_codes, XS, AS),
  atom_codes(X, A),
  member(AA, AS),
  forall(member(A1, A), member(A1, AA)),
  forall(member(A2, AA), member(A2, A)).

no_anagrams(XS) :-
  forall(member(X, XS), (select(X, XS, YS), not(anagram(X, YS)))).

filter2([], []).
filter2([H|T], R) :-
  no_anagrams(H)
  -> filter2(T, R1), R = [H|R1]
  ;  filter2(T, R).

% Day 4.B solution
day04b(X) :- from_file("Inputs/day4.txt", F), filter2(F, Fi), length(Fi, X).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(split_spaces, Lines, F).
