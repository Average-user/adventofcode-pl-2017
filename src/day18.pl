:- use_module(tools).

elem_of([H|_], 0, H) :- !.
elem_of([_|T], N, E) :- succ(N1, N), elem_of(T, N1, E), !.

get(A, Vars, X) :- member((A, X), Vars), ! ; A = X.

eval(set(A,B), Vars, Nvars) :-
  select((A,_), Vars, Vars1), !,
  get(B, Vars, VB),
  Nvars = [(A,VB)|Vars1].

eval(Action, Vars, Nvars) :-
  (Action = add(A,B) -> get(B, Vars, VB), member((A,X), Vars), Z is X+VB;
   Action = mul(A,B) -> get(B, Vars, VB), member((A,X), Vars), Z is X*VB;
   Action = mOd(A,B) -> get(B, Vars, VB), member((A,X), Vars), Z is X mod VB),
  select((A, X), Vars, Vars1), !, Nvars = [(A, Z)|Vars1].

run(Ins, Ix, Vars, Rec, L)   :-
  elem_of(Ins, Ix, I),
  ((I = rcv(A), get(A, Vars, VA), not(VA = 0), L = Rec), !;
   (I = snd(A) -> get(A, Vars, VA), NRec = [VA|Rec] ; NRec = Rec),
   (I = jgz(A, B), get(A, Vars, VA), VA > 0, get(B, Vars, VB)
     -> NIx is Ix+VB ; NIx is Ix+1),
   (eval(I, Vars, NVars), ! ; NVars = Vars),
   run(Ins, NIx, NVars, NRec, L)).

nextStateH(snd(X), Ix, Vs, Q, NIx, Vs, Q, S) :-
  get(X, Vs, VX), S = VX, succ(Ix, NIx).

nextStateH(rcv(X), Ix, Vs, Q, NIx, NVs, NQ, nil) :-
  Q = [H|NQ], get(H, Vs, VH), eval(set(X, VH), Vs, NVs), NIx is Ix+1
  ; NVs = Vs, NQ = Q, NIx = Ix.

nextStateH(jgz(A, B), Ix, Vs, Q, NIx, Vs, Q, nil) :-
  get(A, Vs, Va), Va > 0, get(B, Vs, Vb), NIx is Ix+Vb ; succ(Ix, NIx).

nextStateH(X, Ix, Vs, Q, NIx, NVs, Q, nil) :-
  eval(X, Vs, NVs), succ(Ix, NIx).

nextState(P, Ix, Vs, Q, NIx, NVs, NQ, S) :-
  elem_of(P, Ix, I), nextStateH(I, Ix, Vs, Q, NIx, NVs, NQ, S).

coordinate(P, Vs1, Vs2, Ix1, Ix2, Q1, Q2, R) :-
  nextState(P, Ix1, Vs1, Q1, NIx1, NVs1, NQ1, S1),
  nextState(P, Ix2, Vs2, Q2, NIx2, NVs2, NQ2, S2),
  ( (Ix1 = NIx1, Ix2 = NIx2) -> R is 0 ;
    (S1 = 'nil' -> FQ2 = NQ2 ; append(NQ2, [S1], FQ2)),
    (S2 = 'nil' -> FQ1 = NQ1, Z is 0 ; append(NQ1, [S2], FQ1), Z is 1),
    coordinate(P, NVs1, NVs2, NIx1, NIx2, FQ1, FQ2, AR), R is AR+Z).


% day 18 part A solution
day18a(A) :-
  from_file("Inputs/day18.txt", (Vs, Ins)),
  run(Ins, 0, Vs, [], [A|_]).

% day 18 part B solution
day18b(B) :-
  from_file("Inputs/day18.txt", (Vs, Ins)),
  member((p, X), Vs), select((p, X), Vs, NVs), Vs1 = [(p, 1)|NVs],
  coordinate(Ins, Vs, Vs1, 0, 0, [], [], B), !.

% Formating Input:
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  vars(Lines, Vs),
  flatten(Vs, Vs1),
  list_to_set(Vs1, Vs2),
  maplist(set0, Vs2, Vs3),
  maplist(line_to_in, Lines, Ins),
  exclude(=(nil), Ins, Ins1),
  F = (Vs3, Ins1).

line_to_in(S, I) :-
  split_string(S, " ", " ", S1),
  maplist(string_to_atom, S1, As),
  maplist(val, As, [In|R]),
  (In = 'snd' -> R = [A]  , I = snd(A);
   In = 'set' -> R = [A,B], I = set(A,B);
   In = 'add' -> R = [A,B], I = add(A,B);
   In = 'mul' -> R = [A,B], I = mul(A,B);
   In = 'mod' -> R = [A,B], I = mOd(A,B);
   In = 'rcv' -> R = [A],   I = rcv(A);
   In = 'jgz' -> R = [A,B], I = jgz(A,B)).

val(X, V) :- atom_number(X, V), ! ; V = X.

set0(X, (X, 0)).

vars([],     [])   :- !.
vars([H|Xs], Vars) :-
  vars(Xs, Vars1),
  atom_codes('abcdefghijklmnopqrstuvwxyz', Abc),
  split_string(H, " ", " ",  S),
  maplist(string_to_atom, S, [_|R]),
  findall(X, (member(X, R), atom_codes(X, [C|_]), member(C, Abc)), Vs),
  Vars = [Vs|Vars1].
