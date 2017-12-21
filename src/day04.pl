:- use_module(tools).

anagram(X, XS) :-
  maplist(atom_codes, XS, AS),
  atom_codes(X, A),
  member(AA, AS),
  forall(member(A1, A), member(A1, AA)),
  forall(member(A2, AA), member(A2, A)).

no_anagrams(XS) :-
  forall(member(X, XS), (select(X, XS, YS), not(anagram(X, YS)))).


% Day 4.A solution
day04a(X) :-
  from_file("Inputs/day4.txt", F), include(is_set, F, Fi), length(Fi, X).

% Day 4.B solution
day04b(X) :-
  from_file("Inputs/day4.txt", F), include(no_anagrams, F, Fi), length(Fi, X).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(split_spaces, Lines, F).
