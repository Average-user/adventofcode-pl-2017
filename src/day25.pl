rules('A', 0, 1, '->', 'B').
rules('A', 1, 1, '<-', 'E').
rules('B', 0, 1, '->', 'C').
rules('B', 1, 1, '->', 'F').
rules('C', 0, 1, '<-', 'D').
rules('C', 1, 0, '->', 'B').
rules('D', 0, 1, '->', 'E').
rules('D', 1, 0, '<-', 'C').
rules('E', 0, 1, '<-', 'A').
rules('E', 1, 0, '->', 'D').
rules('F', 0, 1, '->', 'A').
rules('F', 1, 1, '->', 'C').

mk_change(Blank, tape(L, C, R), S, tape(Nl, Nc, Nr), Ns) :-
  rules(S, C, Cc, Dir, Ns), !,
  (R = [] -> Rx = [Blank] ; Rx = R),
  (L = [] -> Lx = [Blank] ; Lx = L),
  (Dir = '->' -> Nl = [Cc|Lx], Rx = [Nc|Nr];
   Dir = '<-' -> Nr = [Cc|Rx], Lx = [Nc|Nl]). 

run_TM(Blank, Tape, S, Limit, Final_Tape) :-
  Limit = 0 -> Tape = tape(L, C, R), reverse(L, L1),
               append(L1, [C], L2), append(L2, R, Final_Tape)

             ; mk_change(Blank, Tape, S, NTape, Ns),
               succ(Limit1, Limit),
               run_TM(Blank, NTape, Ns, Limit1, Final_Tape).

/* I haven't done the formatting input function yet,
   but the solution is working perfectly              

   To Trye your solution, will be necessary to change
   the number of steps and the rules.
                   |
                   |------------------
                                     |              
                                     v                      */
day25a(A) :-
  run_TM(0, tape([], 0, []), 'A', 12523873, F_tape),
  include(=(1), F_tape, Ones),
  length(Ones, A).
