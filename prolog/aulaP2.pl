% Aula de negação por falha

mae(mary).
tia(mary).
mae(any).

filha_unica(X):- \+ tia(X).