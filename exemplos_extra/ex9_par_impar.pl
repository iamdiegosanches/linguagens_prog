pares([],[]).
pares([K|T], [K|P]) :- K mod 2 =:= 0, pares(T, P).
pares([K|T], P) :- K mod 2 =\= 0, pares(T, P).

impares([],[]).
impares([K|T], [K|P]) :- K mod 2 =\= 0, impares(T, P).
impares([_|T], P) :-  impares(T, P).
