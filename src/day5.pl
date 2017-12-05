modifyA([H|T], 0, [H1|T], H) :- H1 is H+1, !.
modifyA([H|T], N, XS,     X) :- N1 is N-1, modifyA(T, N1, XS1, X), XS = [H|XS1].

changeA(I, XS, NI, NXS) :- modifyA(XS, I, NXS, E), NI is I+E.

changesA(XS, I, AC, Z) :-
  changeA(I, XS, NI, NXS), AC1 = AC+1, changesA(NXS, NI, AC1, Z), ! ;
  Z is AC-1.

% | Part 5.A solution.
%   | This tooks about 4-5 minutes in my machine :(
%   | But I couldn't find any mutable or with better index acces
%   | container in Prolog.
partA(A) :- from_file("Inputs/day5.txt", I), changesA(I, 0, 1, A).


/* | For PartB It would be definetively necesary to use a better container
   | than lists, since this goes out of stack really fast.
   | I tried increasing the stack, but after being running partB for a while,
   | my computer got freezed and I had to reboot it                           */
modifyB([H|T], 0, [H1|T], H) :- H < 3 -> H1 is H+1 ; H1 is H-1.
modifyB([H|T], N, XS,     X) :- N1 is N-1, modifyB(T, N1, XS1, X), XS = [H|XS1].

changeB(I, XS, NI, NXS) :- modifyB(XS, I, NXS, E), NI is I+E.

changesB(XS, I, AC, Z) :-
  changeB(I, XS, NI, NXS), AC1 = AC+1, changesB(NXS, NI, AC1, Z), ! ;
  Z is AC-1.

partB(B) :- from_file("Inputs/day5.txt", I), changesB(I, 0, 1, B).


%% Reading File (formating the input)
from_file(Path, F) :-
  read_file(Path, A), split_string(A, "\n", "", Lines),
  maplist(atom_codes, Lines, L),
  select([], L, F1), maplist(to_numbers,F1,F).

to_numbers(A,B) :- number_codes(B,A).
read_file(P,X) :- open(P,read,A), read_string(A,_,X).
