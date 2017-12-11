:- use_module(tools).

nth([H|_], 0, H) :- !.
nth([_|T], N, E) :- N1 is N-1, nth(T, N1, E).

nth1(XS, N, E) :- N1 is N mod 256, nth(XS, N1, E).

replaceIn([_|T], 0, E, [E|T]) :- !.
replaceIn([H|T], N, E, Xs) :-
  N1 is N-1,
  replaceIn(T, N1, E, Xs1),
  Xs = [H|Xs1].

replaceIn1(Xs, N, E, Rs) :-
  N1 is N mod 256, replaceIn(Xs, N1, E, Rs).

replaceAll([],        []          , Xs, Xs) :- !.
replaceAll([I|Index], [E|Elements], Xs, Rs) :-
  replaceIn1(Xs, I, E, Xs1),
  replaceAll(Index, Elements, Xs1, Rs).

process(Xs, []         , _, _   , _ , 1, Xs) :- !.
process(Xs, []         , I, Skip, LS, C, Rs) :-
  C1 is C-1,
  process(Xs, LS, I, Skip, LS, C1, Rs).
process(Xs, [L|Lengths], I, Skip, Ls, C, Rs) :-
  (L = 0 -> L1 = I ; L1 is (L+I-1)),
  numlist(I, L1, ToTake),
  maplist(nth1(Xs), ToTake, Ns),
  reverse(Ns, Ns1),
  replaceAll(ToTake, Ns1, Xs, NXs),
  NSkip is Skip+1,
  NI    is Skip+I+L,
  process(NXs, Lengths, NI, NSkip, Ls, C, Rs).

day10a(A) :-
  from_file("Inputs/day10.txt", F1),
  split_string(F1, ",", ",", F2),
  maplist(string_to_number, F2, F),
  numlist(0,255, Xs),
  process(Xs, F, 0, 0, F, 1, [X,Y|_]),
  A is X*Y.

to_bin(0, []) :- !.
to_bin(N, Xs) :- A is N mod 2, B is N div 2, to_bin(B, Zs), Xs = [A|Zs].

xor([], [],       []) :- !.
xor([], [A|T]   , Xs) :- xor([], T, Xs1), Xs = [A|Xs1], !.
xor([A|T], []   , Xs) :- xor(T, [], Xs1), Xs = [A|Xs1], !.
xor([0|A], [0|B], Xs) :- xor(B, A, Xs1),  Xs = [0|Xs1], !.
xor([0|A], [1|B], Xs) :- xor(A, B, Xs1),  Xs = [1|Xs1], !.
xor([1|A], [0|B], Xs) :- xor(A, B, Xs1),  Xs = [1|Xs1], !.
xor([1|A], [1|B], Xs) :- xor(A, B, Xs1),  Xs = [0|Xs1], !.

xor_dec(A, B, C) :- to_bin(A, BA), to_bin(B, BB), xor(BA, BB, X), to_dec(X, C).

to_dec([],    0) :- !.
to_dec([H|T], X) :-
  to_dec(T, X1), X is H+(2*X1).

chunksof16([], []) :- !.
chunksof16(Xs, Ys) :-
  length(A, 16),
  append(A, B, Xs),
  chunksof16(B, Xs1),
  Ys = [A|Xs1].

apply_xor([],    0).
apply_xor([H|T], X) :-
  apply_xor(T, X1), xor_dec(H, X1, X).

hex_dig(A, B) :-
  A < 10 -> atom_number(B, A)
  ; HD = [(10, 'a'), (11, 'b'), (12, 'c'), (13, 'd'), (14, 'e'), (15, 'f')],
  member((A, B), HD), !.

to_hex(N, H) :- to_hexH(N, H1), reverse(H1, H).

to_hexH(N, H) :-
  N < 16
    -> hex_dig(N, Hd), H = [Hd]
    ; R is N mod 16,
      NN is N div 16,
      hex_dig(R, A), to_hexH(NN, B),
      H = [A|B].

ceros_on(N, Xs, Ys) :-
  length(Xs, L), Z is N-L,
  length(Y, Z),
  findall(X, (member(X, Y), X = 0), R),
  append(R , Xs, Ys).

day10b(B) :-
  from_file("Inputs/day10.txt", F),
  atom_codes(F, Codes),
  append(Codes, [17, 31, 73, 47, 23], Lengths),
  numlist(0,255, Xs),
  process(Xs, Lengths, 0, 0, Lengths, 64, Set256),
  chunksof16(Set256, Groups),
  maplist(apply_xor, Groups, Xors),
  maplist(to_hex, Xors, Hexs),
  maplist(ceros_on(2), Hexs, Hexs1),
  flatten(Hexs1, Hex),
  atomic_list_concat(Hex, B).


%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [F|_]).
