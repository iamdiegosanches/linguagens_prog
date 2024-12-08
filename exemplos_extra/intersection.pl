%Escreva uma função em Prolog “intersecção” que retorna uma lista que contenha
%somente uma instancia de cada átomo que é membro de suas listas de argumentos. Por exemplo:
%intersection([a,b,a], [c,b,d,a], Resposta) deve retornar a resposta [b,a] ou [a,b].

intersection(L1, L2, Resposta) :- busca_intersection(L1, L2, [], Resposta).

busca_intersection([], _, Resposta, Resposta).

busca_intersection([H|T], Lista, Aux, Resposta) :- busca(H, Lista), \+ busca(H, Aux), !, busca_intersection(T, Lista, [H|Aux], Resposta).
busca_intersection([_|T], Lista, Aux, Resposta) :- busca_intersection(T, Lista, Aux, Resposta).

busca(H, [H|_]).
busca(H, [_|T]) :- busca(H, T).
