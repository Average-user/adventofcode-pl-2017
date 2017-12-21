:- use_module(tools).

move(0, 1).
move(0, -1).
move(-1, 0).
move(1, 0).

isletter(X) :- atom_codes("ABCDEFGHIJKLMNOPQRSTUVWXYZ", L), member(X, L).

get_in(Xs, (X, Y), E) :-
  (X < 0 ; Y < 0) -> E = nil;
  nth0(X, Xs, Ys), nth0(Y, Ys, E1),
  (E1 = 32 -> E = nil ; E = E1),
  ! ; E = nil.

options(Xs, (X, Y), Ops) :-
  findall((A, B), (move(I, J), A is X+I, B is Y+J), COps),  
  findall(e(E, C), (member(C, COps), get_in(Xs, C, E)), Ops).  

next(down, P, Coor, Xs, C, E, Di) :-
  options(Xs, Coor, [R, L, _, D]),
  member(e(E, C), [D, R, L]),
  not(E = nil), not(C = P),
  (e(E, C) = D -> Di = down;
   e(E, C) = R -> Di = right;
   e(E, C) = L -> Di = left).

next(up, P, Coor, Xs, C, E, Di) :-
  options(Xs, Coor, [R, L, U, _]),
  member(e(E, C), [U, R, L]),
  not(E = nil), not(C = P),
  (e(E, C) = U -> Di = up;
   e(E, C) = R -> Di = right;
   e(E, C) = L -> Di = left).

next(left, P, Coor, Xs, C, E, Di) :-
  options(Xs, Coor, [_, L, U, D]),
  member(e(E, C), [L, U, D]),
  not(E = nil), not(C = P),
  (e(E, C) = D -> Di = down;
   e(E, C) = U -> Di = up;
   e(E, C) = L -> Di = left).

next(right, P, Coor, Xs, C, E, Di) :-
  options(Xs, Coor, [R, _, U, D]),
  member(e(E, C), [R, D, U]),
  not(E = nil), not(C = P),
  (e(E, C) = D -> Di = down;
   e(E, C) = R -> Di = right;
   e(E, C) = U -> Di = up).

find_letters(Xs, P, D, C, Ls, Fs, A, B) :-
  Ls = [] -> A = Fs, B = 1;
  NP = C,
  next(D, P, C, Xs, NC, E, ND),
  (member(E, Ls) -> select(E, Ls, NLs), NFs = [E|Fs] 
                  ; NLs = Ls, NFs = Fs),
  find_letters(Xs, NP, ND, NC, NLs, NFs, A, B1), B is B1+1.


% day 19 part A solution
day19a(A) :-
  from_file("Inputs/day19.txt", (Xs, Ls, Y)),
  find_letters(Xs, (0,0), down, (0, Y), Ls, [], R, _),
  reverse(R, RR), atom_codes(A, RR), !.

% day 19 part B solution
day19b(B) :-
  from_file("Inputs/day19.txt", (Xs, Ls, Y)),
  find_letters(Xs, (0,0), down, (0, Y), Ls, [], _, B), !.


% formating input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(atom_codes, Lines, Lines1),
  flatten(Lines1, Chars),
  include(isletter, Chars, L),
  Lines1 = [H|_],
  length(H, HL),
  between(0, HL, Y),
  get_in(Lines1, (0, Y), 124),
  F = (Lines1, L, Y).
