pares(L, P) :- verifica_pares(L, [], P).

verifica_pares([], P, P).
verifica_pares([H|T], Aux, P) :- par(H), !, verifica_pares(T, [H|Aux] , P).
verifica_pares([_|T], Aux, P) :- verifica_pares(T, Aux, P).

par(N):- N mod 2 =:= 0.
