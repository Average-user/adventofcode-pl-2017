:- use_module(tools).

insert([H|T], 0, E, [H,E|T]) :- !.
insert([H|T], N, E, Ys) :- succ(N1, N), insert(T, N1, E, Ys1), Ys = [H|Ys1].

run(Xs, _, _, N, N, Xs) :- !.
run(Xs, I, T, N, M, Ys) :-
  length(Xs, L),
  NI is (I+T+1) mod L,
	succ(N, N1),
	insert(Xs, NI, N, NXs),
 	run(NXs, NI, T, N1, M, Ys).

run2(_, _, N, M, M, N) :- !.
run2(I, T, N, C, M, O) :-
  NI is (I + T) mod C,
	(NI = 0 -> NN = C ; NN = N),
	succ(C, NC),
	succ(NI, N1),
	run2(N1, T, NN, NC, M, O).
		

day17a(A) :-
  from_file("Inputs/day17.txt", F),
	run([0], 0, F, 1, 2018, Xs),
	append(_, [2017, A|_], Xs), !.

day17b(B) :-
  from_file("Inputs/day17.txt", F), run2(0, F, 0, 1, 50000002, B), !.

% Formating Input:
from_file(Path, F) :-
  file_to_lines(Path, [N|_]),
  string_to_number(N, F).  
