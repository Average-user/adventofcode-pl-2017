:- module(day10, [to_knot/2]). % comment this if you are trying this exercise
:- use_module(tools).

nth256(XS, N, E) :- N1 is N mod 256, nth0(N1, XS, E).

mod256(X, Y) :- Y is X mod 256.

replaceAllH([],         Xs,    _, Xs).
replaceAllH([(I,E)|ZT], [H|T], N, Ys) :-
  succ(N, N1),
  (I = N -> replaceAllH(ZT, T, N1, Ys1), Ys = [E|Ys1]
          ; replaceAllH([(I,E)|ZT], T, N1, Ys1), Ys = [H|Ys1]).

replaceAll(Indexes, Elements, Xs, Ys) :-
   maplist(mod256, Indexes, NI),
   zip(NI, Elements, Zipped),
   sort(Zipped, SZ),
   replaceAllH(SZ, Xs, 0, Ys).

process(Xs, []         , _, _   , _ , 1, Xs) :- !.
process(Xs, []         , I, Skip, LS, C, Rs) :-
  C1 is C-1, process(Xs, LS, I, Skip, LS, C1, Rs).
process(Xs, [L|Lengths], I, Skip, Ls, C, Rs) :-
  (L = 0 -> L1 = I ; L1 is (L+I-1)),
  numlist(I, L1, ToTake),
  maplist(nth256(Xs), ToTake, Ns),
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


chunksof16([], []) :- !.
chunksof16(Xs, Ys) :-
  length(A, 16),
  append(A, B, Xs),
  chunksof16(B, Xs1),
  Ys = [A|Xs1].

apply_xor(Xs,  X) :- foldl(my_xor, Xs, 0, X).

my_xor(A, B, X) :- X is A xor B.

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

to_knot(Atom, Knot) :-
  atom_codes(Atom, Codes),
  append(Codes, [17, 31, 73, 47, 23], Lengths),
  numlist(0,255, Xs),
  process(Xs, Lengths, 0, 0, Lengths, 64, Set256),
  chunksof16(Set256, Groups),
  maplist(apply_xor, Groups, Xors),
  maplist(to_hex, Xors, Hexs),
  maplist(ceros_on(2), Hexs, Hexs1),
  flatten(Hexs1, Hex),
  atomic_list_concat(Hex, Knot), !.

day10b(B) :-
  from_file("Inputs/day10.txt", F),
  to_knot(F, B).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [F|_]).
