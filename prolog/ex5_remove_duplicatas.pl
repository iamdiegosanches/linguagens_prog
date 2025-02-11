%Escreva um programa Prolog remove_duplicatas que remova todas as duplicatas de
%uma lista. Por exemplo, a consulta remove_duplicatas([a,b,a,a,c,b,b,a], X) deve
%retornar X = [a,b,c]

remove_duplicatas(Lista, X) :- verifica(Lista, [], X).
% Caso base
verifica([], L, L).

% Passo indutivo
verifica([H|T], L, X) :- busca(H, L), !, verifica(T, L, X).
% Se o elemento nao esta na lista extra adicionar
verifica([H|T], L, X) :- verifica(T, [H|L], X).

% Verifica na lista adicional se o elemento está presente
busca(H, [H|_]).
busca(Y, [_|T]) :- busca(Y, T).
