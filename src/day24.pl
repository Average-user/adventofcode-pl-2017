:- use_module(tools).

find_path(Cs, [(A,B)|T], (W,L), F) :-
  member((B,B), Cs),
  selectchk((B,B), Cs, NCs),
  NW is B+B+W,
  succ(L, NL),
  find_path(NCs, [(B,B), (A,B)|T], (NW,NL), F).

find_path(Cs, [(A, B)|T], (W,L), F) :-
 (member((X, B), Cs), R = (X,B) ; member((B,X), Cs), R = (B,X)),
 not(X = B),
 selectchk(R, Cs, NCs),
 NW is X+B+W,
 succ(L, NL),
 find_path(NCs, [(B,X), (A, B)|T], (NW,NL), F);
 F = (W,L).

correct_path(Cs, Ps) :-
  member((A,B), Cs),
  (  A = 0, selectchk((A,B), Cs, Cs1),
            find_path(Cs1, [(A,B)], (B,1), Ps)
   ; B = 0, selectchk((A,B), Cs, Cs1),
            find_path(Cs1, [(B,A)], (A,1), Ps)).

eqsnd(L, (_, L)).

% Day 24 part A solution
day24a(A) :-
  from_file("Inputs/day24.txt", F),
  findall(P, correct_path(F, P), Wls),
  maplist(fst, Wls, Fs),
  max_list(Fs, A).


% Day 24 part B solution
day24b(B) :-
  from_file("Inputs/day24.txt", F),
  findall(P, correct_path(F, P), Wls),
  maplist(snd, Wls, Ss),
  max_list(Ss, L),
  include(eqsnd(L), Wls, Wls1),
  maplist(fst, Wls1, Fs),
  max_list(Fs, B). 


% formating input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(get_lines, Lines, F).

get_lines(X, (A, B)) :-
  split_string(X, "/", "/", [A1,B1]),
  string_to_number(A1, A),
  string_to_number(B1, B).
