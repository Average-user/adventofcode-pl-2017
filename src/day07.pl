:- use_module(tools).

sub_program(p(_,_,P), P).

sub_programs(XS, Prs) :-
  maplist(sub_program, XS, PrsL),
  flatten(PrsL, Prs).

find_bottom(XS, Bottom) :-
  sub_programs(XS, SP),
  member(p(Bottom, _, _), XS),
  not(member(Bottom, SP)).

% Day 7.A solution
day07a(A) :- from_file("Inputs/day7.txt", F), find_bottom(F, A), !.


total_weight(XS, Name, S) :-
  member(p(Name, N1, Programs), XS),
  (Programs = nil -> S = N1 ;
   maplist(total_weight(XS),Programs, Ss),
   sum(Ss, N2),
   S is N1+N2).

sub_weights(XS, Name, Ss) :-
  member(p(Name, N, Programs), XS),
  (Programs = nil -> Ss = [(Name, N)] ;
   maplist(total_weight(XS),Programs, S),
   zip(Programs, S, Ss)).

unique(XS, (N, X)) :-
  member((N, X), XS),
  select((N, X), XS, XS1),
  not(member((_, X), XS1)), !.

balanced(XS, Name) :-
  sub_weights(XS, Name, S),
  maplist(snd, S, S1),
  list_to_set(S1, Set),
  length(Set, 1).

find_problem(XS, Name, P) :-
  balanced(XS, Name) -> P = Name ;
  sub_weights(XS, Name, Ss),
  unique(Ss, (Uname, _)),
  find_problem(XS, Uname, P).

new_weight(XS, Weight) :-
  find_bottom(XS, B),
  find_problem(XS, B, P),
  sub_weights(XS, B, S),
  unique(S, (_, W1)),
  member((_, W2), S),
  not(W1 = W2),
  D is W2-W1,
  member(p(P, N, _), XS),
  Weight is N+D.

% Day 7.B solution
day07b(B) :- from_file("Inputs/day7.txt", F), new_weight(F, B), !.

%% Reading File (formating the input)
from_file(Path, F) :-
  file_to_lines(Path, Lines),
  maplist(formatS, Lines, F), !.

formatS(String, p(Name, N, Program)) :-
  split_string(String, "(", "(", [Name1,R]),
  string_to_list(Name1, NL),
  select(32, NL, NL2),
  string_to_list(Name, NL2),
  split_string(R, ")", ")", [NS|R2]),
  number_codes(N, NS),
  (R2 = [] -> Program = nil;
   R2 = [R2H|_],
   string_to_list(R2H, [_,_,_,_|P]),
   string_to_list(P2, P),
   split_string(P2, ", ", ", ", Program)).
