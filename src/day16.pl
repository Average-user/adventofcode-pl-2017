:- use_module(tools).

elem_of([H|_], 0, H) :- !.
elem_of([_|T], N, E) :- succ(N1, N), elem_of(T, N1, E), !.

replace([H|T], 0, E, [E|T], H) :- !.
replace([H|T], N, E, Ys, R) :-
  succ(N1, N), replace(T, N1, E, Ys1, R), Ys = [H|Ys1], !.

spin(Xs, N, Ys) :-
  append(A, B, Xs),
  length(B, N),
  append(B, A, Ys), !.

partner([H|T], A, B, X, Ys) :-
  partner(T, A, B, X, Ys1),
  Ys = [H|Ys1], !.
partner([A|T], A, B, 0, Ys) :-
  partner(T, A, B, 1, Ys1),
  Ys = [B|Ys1], !.
partner([B|T], A, B, 0, Ys) :-
  partner(T, A, B, 1, Ys1),
  Ys = [A|Ys1], !.
partner([A|T], A, B, 1, Ys) :-
  Ys = [B|T], !.
partner([B|T], A, B, 1, Ys) :-
  Ys = [A|T], !.

exchange(Xs, A, B, Ys) :-
  elem_of(Xs, B, EB),
  replace(Xs, A, EB, Ys1, EA),
  replace(Ys1, B, EA, Ys, _).

dance(Xs, []      ,   Xs) :- !.
dance(Xs, [s(N)|I],   Ys) :- spin(Xs, N, NXs), dance(NXs, I, Ys), !.
dance(Xs, [p(A,B)|I], Ys) :- partner(Xs, A, B, 0, NXs), dance(NXs, I, Ys), !.
dance(Xs, [x(A,B)|I], Ys) :- exchange(Xs, A, B, Nxs), dance(Nxs, I, Ys), !.

dance_cycle(Xs, Ins, Ac, Ys) :-
  not(list_to_set(Ac, Ac))
    -> Ys = Ac
    ;  dance(Xs, Ins, Ds),
       Ac1 = [Ds|Ac],
       dance_cycle(Ds, Ins, Ac1, Ys).

cycle_deductions(C, R) :-
  length(C, Cl),
  M is 1000000000 mod (Cl-1),
  append(_, [R|B], C),
  length([R|B], M).

day16a(A) :-
  from_file("Inputs/day16.txt", F),
  dance([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p], F, D),
  atomic_list_concat(D, A), !.

day16b(B) :-
  from_file("Inputs/day16.txt", F),
  Start = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p],
  dance_cycle(Start, F, [], Cycle),
  cycle_deductions(Cycle, R),
  atomic_list_concat(R, B), !.
  
% Formating Input.
from_file(Path, F) :-
  file_to_lines(Path, [In|_]),
  split_string(In, ",", ",", A),
  maplist(atom_codes, A, B),
  maplist(exp_instruction, B, F).

exp_instruction([115|Xs], I) :-
  number_codes(N, Xs),
  I = s(N).
exp_instruction([120|Xs], I) :-
  string_codes(S, Xs),
  split_string(S, "/", "/", Ns),
  maplist(string_codes, Ns, Ns1),
  maplist(flip(number_codes), Ns1, [A,B]),
  I = x(A, B).
exp_instruction([112|Xs], I) :-
  string_codes(S, Xs),
  split_string(S, "/", "/", Ns),
  maplist(string_codes, Ns, Ns1),
  maplist(flip(atom_codes), Ns1, [A,B]),
  I = p(A,B).
