:- use_module(tools).

elem_of([H|_], 0, H) :- !.
elem_of([_|T], N, E) :- succ(N1, N), elem_of(T, N1, E), !.
  
get(A, Vars, X) :-
  member((A, X), Vars), ! ; A = X.
  
eval(set(A,B), Vars, Nvars) :-
  select((A,_), Vars, Vars1), !,
  get(B, Vars, VB),
  Nvars = [(A,VB)|Vars1].

eval(add(A,B), Vars, Nvars) :-
  get(B, Vars, VB),
  member((A, X), Vars),
  Z is X+VB,
  select((A, X), Vars, Vars1), !,
  Nvars = [(A, Z)|Vars1].

eval(mul(A,B), Vars, Nvars) :-
  get(B, Vars, VB),
  member((A, X), Vars),
  Z is X*VB,
  select((A, X), Vars, Vars1), !,
  Nvars = [(A, Z)|Vars1].

eval(mOd(A,B), Vars, Nvars) :-
  get(B, Vars, VB),
  member((A, X), Vars),
  Z is X mod VB,
  select((A, X), Vars, Vars1), !,
  Nvars = [(A, Z)|Vars1].

run(Ins, Ix, Vars, Rec, L)   :-
  elem_of(Ins, Ix, I),
  ((I = rcv(A), get(A, Vars, VA), not(VA = 0), L = Rec), !;
   (I = snd(A) -> get(A, Vars, VA), NRec = [VA|Rec] ; NRec = Rec),
   (I = jgz(A, B), get(A, Vars, VA), VA > 0, get(B, Vars, VB)
     -> NIx is Ix+VB ; NIx is Ix+1),
   (eval(I, Vars, NVars),! ; NVars = Vars),
   run(Ins, NIx, NVars, NRec, L)).
  
day18a(A) :-
  from_file("Inputs/day18.txt", (Vs, Ins)),
  run(Ins, 0, Vs, [], [A|_]).

day18b(B) :-
  from_file("Inputs/day18.txt", B).

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
    
  
