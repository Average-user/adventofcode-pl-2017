:- use_module(tools).

infix(A, B) :-
  append(X, _, A),
  append(_, B, X), !.

fun_of(XS, F) :-
  atom_codes('inc', I),
  infix(XS, I) -> F = inc ;
   F = dec.

condition_of(XS, C) :-
  atom_codes('<',  Less),
  atom_codes('>',  More),
  atom_codes('==', Eq),
  atom_codes('!=', Dif),
  atom_codes('>=', MoreEq),
  atom_codes('<=', LessEq),
  (infix(XS, Eq)     -> C = '==' ;
   infix(XS, Dif)    -> C = '!=' ;
   infix(XS, MoreEq) -> C = '>=' ;
   infix(XS, LessEq) -> C = '<=' ;
   infix(XS, Less)   -> C = '<'  ;
   infix(XS, More)   -> C = '>').

fromatI(Xs, i(A, Fun, B, C, Cond, D)) :-
  atom_codes(Xs, Codes),
  fun_of(Codes, Fun),
  split_spaces(Xs, [A|[_,B1,_,C,_,D1]]),
  condition_of(Codes, Cond),
  atom_number(B1, B),
  atom_number(D1, D).

all_vars(Xs, Vs) :-
  findall((V, 0), member(i(V, _, _, _, _, _), Xs), Vs1),
  list_to_set(Vs1, Vs).

value_of(Vars, X, Z) :-
  member((X, N), Vars) -> Z = N.

satisfy(Vs, A, '<' , B) :- value_of(Vs, A, A1), A1  <  B, !.
satisfy(Vs, A, '>' , B) :- value_of(Vs, A, A1), A1  >  B, !.
satisfy(Vs, A, '==', B) :- value_of(Vs, A, A1), A1 =:= B, !.
satisfy(Vs, A, '!=', B) :- value_of(Vs, A, A1), A1 =\= B, !.
satisfy(Vs, A, '>=', B) :- value_of(Vs, A, A1), A1 >=  B, !.
satisfy(Vs, A, '<=', B) :- value_of(Vs, A, A1), B  >= A1, !.

apply_fun(Vars, A, inc, B, R) :- member((A, N), Vars), R is N+B, !.
apply_fun(Vars, A, dec, B, R) :- member((A, N), Vars), R is N-B, !.

insert_val(Vars, V, NV, NVars) :-
  select((V, _), Vars, NVars1),
  NVars = [(V, NV)|NVars1].

run_exp(Vars, i(A, Fun, B, C, Cond, D), NVars) :-
  satisfy(Vars, C, Cond, D)
  -> apply_fun(Vars, A, Fun, B, R),
     insert_val(Vars, A, R, NVars)
  ;  NVars = Vars.

run(Vars, []      , Vars)  :- !.
run(Vars, [E|Exps], RVars) :-
  run_exp(Vars, E, NVars),
  run(NVars, Exps, RVars).

run2(_   , []      , AC, _    , AC)  :- !.
run2(Vars, [E|Exps], AC, RVars, RAC) :-
  run_exp(Vars, E, NVars),
  maplist(snd, NVars, Ns),
  max_list(Ns, M),
  NAC = [M|AC],
  run2(NVars, Exps, NAC, RVars, RAC).

% Part 8.A solution
partA(A) :-
  from_file("Inputs/day8.txt", F),
  all_vars(F, Vars),
  run(Vars, F, NVars),
  maplist(snd, NVars, Ns),
  max_list(Ns, A), !.

% Part 8.B solution
partB(B) :-
  from_file("Inputs/day8.txt", F),
  all_vars(F, Vars),
  run2(Vars, F, [], _, AC),
  max_list(AC, B), !.

% Complete Day 8 solution
main((A, B)) :- partA(A), partB(B).

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(fromatI, Lines, F).
