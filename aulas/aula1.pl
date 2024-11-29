fala(ted, ingles).
fala(mary, ingles).
fala(tom, ingles).
fala(tom, russo).
fala(tom, portugues).
fala(mary, russo).

fala_com(Pessoa1, Pessoa2) :- fala(Pessoa1, X), fala(Pessoa2, X), Pessoa1 \= Pessoa2.

pai(tom, mary).
pai(tom, joao).

pai(tom, breno).
pai(breno, alvaro).

mae(joana, pedro).

irmoes(Pessoa1, Pessoa2) :- pai(X, Pessoa1), pai(X, Pessoa2), Pessoa1 \= Pessoa2.
irmoes(Pessoa1, Pessoa2) :- mae(Y, Pessoa1), mae(Y, Pessoa2), Pessoa1 \= Pessoa2.

avo(Avo, Neto) :- pai(Pai, Neto), pai(Avo, Pai).
avo(Avo, Neto) :- mae(Mae, Neto), pai(Avo, Mae).
avo_mulher(Avo, Neto) :- pai(Pai, Neto), mae(Avo, Pai).
avo_mulher(Avo, Neto) :- mae(Mae, Neto), mae(Avo, Mae).
