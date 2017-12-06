:- use_module(tools).

max(XS, (I, M)) :-
  member((I, M), XS),
  forall(member((_, X), XS), M >= X), !.

distributeH([]        , N, [], N) :- !.
distributeH(XS        , 0, XS, 0) :- !.
distributeH([(I, V)|T], N, YS, Z) :-
  V1 is V+1, N1 is N-1,
  distributeH(T, N1, YS1, Z),
  YS = [(I, V1)|YS1], !.

distribute(XS, N, RS) :-
  distributeH(XS, N, NXS, NN),
  (N =:= 0 -> RS = NXS ;
   distributeH(NXS, NN, RS, _)).

iteration(XS, YS) :-
  max(XS, (I, M)),
  splitAt(XS, I, A, [_|B]),
  append(A, [(I, 0)], C),
  append(B, C, Rest),
  distribute(Rest, M, ZS),
  sort(ZS, YS).

loop(XS, XSS, R) :-
  iteration(XS, NXS),
  maplist(snd,NXS,Rec),
  (member(Rec, XSS), R = [Rec|XSS],!;
     NXSS = [Rec|XSS],
     loop(NXS, NXSS, R)).

run(X, S) :- zip_index(X, XS),  loop(XS, [], S).

partA(A) :-
  from_file("Inputs/day6.txt", F),
  run(F, X),
  length(X, A).

dropWhileB([X|T], X, [X|T]) :- !.
dropWhileB([_|T], X, XS) :- dropWhileB(T, X, XS).

partB(B) :-
  from_file("Inputs/day6.txt", F),
  run(F, [H|T]),
  reverse(T, X),
  dropWhileB(X, H, Cycle),
  length(Cycle, B).

main((A,B)) :-
  from_file("Inputs/day6.txt", F),
  run(F, [H|T]),
  length([H|T], A),
  reverse(T, X),
  dropWhileB(X, H, Cycle),
  length(Cycle, B).


% "Input/day6.txt"
%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, [X|_]),
  split_tabs(X,S),
  maplist(string_to_number, S, F).
