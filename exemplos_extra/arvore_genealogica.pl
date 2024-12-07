% Considere a arvore genealogica

mae(sue, nancy).
mae(sue, jeff).
mae(mary, sue).
mae(mary, bill).
pai(bill, ted).

% Faça regras para irmão, avó e primo

irmao(X, Y) :- mae(Z, X), mae(Z, Y), X \== Y.
irmao(X, Y) :- pai(Z, X), pai(Z, Y), X \== Y.

avo(Quem, T) :- mae(Quem, Z), mae(Z, T).
avo(Quem, T) :- mae(Quem, Z), pai(Z, T).

primo(G, T) :- irmao(X, Y), pai(X, G), pai(Y, T).
primo(G, T) :- irmao(X, Y), mae(X, G), pai(Y, T).
primo(G, T) :- irmao(X, Y), pai(X, G), mae(Y, T).
primo(G, T) :- irmao(X, Y), mae(X, G), mae(Y, T).
