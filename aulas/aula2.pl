% Listas

% Lista com tamanho par
par([]).
par([_,_ | X]) :- p(X).

% Escrever itens de uma lista
escreve([]).
escreve([X|T]) :- write(X), p(T).

% Busca elemento em uma lista
busca(H, [H|_]).
busca(X,[_|T]) :- p(X, T).

% Inverte uma lista
p(Lista, ListaInvertida) :- inverte(Lista, ListaInvertida, []).
inverte([], Aux, Aux). % Caso base lista vazia
inverte([Head|Tail], Invertida, Aux) :- inverte(Tail, Invertida, [Head|Aux]).
