:- use_module(tools).

get(A, Vars, X) :- member((A, X), Vars), ! ; A = X.

eval(set(A,B), Vars, Nvars, f) :-
  select((A,_), Vars, Vars1), !,
  get(B, Vars, VB),
  Nvars = [(A,VB)|Vars1].

eval(Action, Vars, Nvars, M) :-
  (Action = sub(A,B) -> get(B, Vars, VB), member((A,X), Vars), Z is X-VB, M = f;
   Action = mul(A,B) -> get(B, Vars, VB), member((A,X), Vars), Z is X*VB, M = t),
  select((A, X), Vars, Vars1), !, Nvars = [(A, Z)|Vars1].

run(Ins, Ix, Vars, C, R) :-
  nth0(Ix, Ins, I)
    -> (I = jnz(A, B), get(A, Vars, VA), not(VA =  0), get(B, Vars, VB)
         -> NIx is Ix+VB ; NIx is Ix+1),
       (eval(I, Vars, NVars, M), ! ; NVars = Vars, M = f),
       (M = t -> succ(C, NC),! ; NC = C),
       run(Ins, NIx, NVars, NC, R)
     ; R = C.

isPrime(A):-
  A1 is ceiling(sqrt(A)),
  not((between(2,A1,N), 0 is mod(A,N))),not(A is 1).

nums(A, T, Ac, Ac) :- A > T, !.
nums(A, T, Ac, Rc) :-
  NAc = [A|Ac],
  Na  is A+17,
  nums(Na, T, NAc, Rc).

primes(B, L) :-
  Z is B*100+100000,
  T is Z+17000,
  nums(Z, T, [], Ns),
  exclude(isPrime, Ns, Rs),
  length(Rs, L).


% Day 23 part A solution
day23a(A) :-
  from_file("Inputs/day23.txt", F),
  zip([a,b,c,d,e,f,g,h], [0,0,0,0,0,0,0,0], Vars),
  run(F, 0, Vars, 0, A).

% Day 23 part B solution
day23b(B) :-
  from_file("Inputs/day23.txt", F),
  member(set(b, X), F),
  primes(X, B), !.

% formating input
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(line_to_in, Lines, Ins),
  exclude(=(nil), Ins, F).

line_to_in(S, I) :-
  split_string(S, " ", " ", S1),
  maplist(string_to_atom, S1, As),
  maplist(val, As, [In|R]),
  (In = 'set' -> R = [A,B], I = set(A,B);
   In = 'sub' -> R = [A,B], I = sub(A,B);
   In = 'mul' -> R = [A,B], I = mul(A,B);
   In = 'jnz' -> R = [A,B], I = jnz(A,B)).

val(X, V) :- atom_number(X, V), ! ; V = X.
