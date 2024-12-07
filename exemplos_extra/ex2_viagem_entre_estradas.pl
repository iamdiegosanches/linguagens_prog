% Dada uma base de conhecimento representando uma rede de cidades conectadas por estados. Você precisa definir as conexões entre cidades usando regras e resoluções SLD para verificar se é possível viajar entre duas cidades
% 1. Definir os fatos a seguir
% * Há uma estrada de A para B
% * uma de B para C
% * uma de C para D
% * uma de E para F

estrada(a, b).
estrada(b, c).
estrada(c, d).
estrada(e, f).

% 2. Crie uma regra para determinar se é possível viajar entre cidades considerando conexões diretas e indiretas
viagem(A, B) :- estrada(A, B).
viagem(A, D) :- estrada(A, B), viagem(B, D).


% 3. Análise a para d | a para e

t1 :- viagem(a, d).
t2 :- viagem(a, e).
