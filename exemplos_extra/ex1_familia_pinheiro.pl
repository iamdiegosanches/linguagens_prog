% exercicio extra familia pinheiro

progenitor(jose, joao).
progenitor(jose, ana).
progenitor(maria, joao).
progenitor(maria, ana).
progenitor(ana, helena).
progenitor(ana, joana).
progenitor(joao, mario).
progenitor(helena, carlos).
progenitor(mario, carlos).

sexo(ana, feminino).
sexo(maria, feminino).
sexo(joana, feminino).
sexo(helena, feminino).

sexo(mario, masculino).
sexo(joao, masculino).
sexo(jose, masculino).
sexo(carlos, masculino).

irma(Pessoa1, Pessoa2) :- progenitor(X, Pessoa1), progenitor(X, Pessoa1), Pessoa1 \== Pessoa2 , sexo(Pessoa1, feminino).

irmao(Pessoa1, Pessoa2) :- progenitor(X, Pessoa1), progenitor(X, Pessoa1), Pessoa1 \== Pessoa2 , sexo(Pessoa1, masculino).

descendente(Pessoa1, Pessoa2):- progenitor(Pessoa1, Pessoa2).
descendente(Pessoa1, Pessoa2):- progenitor(Pessoa1, A), descendente(A, Pessoa2).

pai(Pai, Pessoa) :- progenitor(Pai, Pessoa), sexo(Pai, masculino).

mae(Mae, Pessoa) :- progenitor(Mae, Pessoa), sexo(Mae, feminino).

avo(Avo, Pessoa) :- pai(Avo, Pai), pai(Pai, Pessoa).
avo(Avo, Pessoa) :- pai(Avo, Mae), mae(Mae, Pessoa).

avoF(Avo, Pessoa) :- mae(Avo, Pai), pai(Pai, Pessoa).
avoF(Avo, Pessoa) :- mae(Avo, Mae), mae(Mae, Pessoa).

tio(Tio, Pessoa) :- irmao(Tio, Pais), progenitor(Pais, Pessoa).
tio(Tio, Pessoa) :- irma(Tio, Pais), progenitor(Pais, Pessoa).

primo(Primo, Pessoa) :- tio(Tio, Pessoa), progenitor(Tio, Primo).


% Formule em Prolog as seguintes questões:
% 1. O João é filho do José?
% 2. Quem são os filhos da Maria?
% 3. Quem são os primos do Mário?
% 4. Quantos sobrinhos/sobrinhas com um Tio existem na família Pinheiro?
% 5. Quem são os ascendentes do Carlos?
% 6. A Helena tem irmãos? E irmãs?


% questoes:
q1 :- progenitor(jose, joao).
q2(X) :- mae(maria, X).
q3(X) :- primo(mario, X).
q4(X) :- tio(_, X).
q5(X) :- descendente(X, carlos).
q6a(L) :- findall(X, irmao(X, helena), L).
q6b(L) :- findall(X, irma(X, helena), L).
