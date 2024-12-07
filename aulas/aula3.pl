% Corte em programas lógicos

% Nesse caso o backtracking faz com que a computação proceda para a segunda cláusula o qual passara o valor de r(X)  para s(X)
% Podemos evitar essa situação inserindo um corte (!), depois de p(X) na primeira clausula para obter a seguinte implementacao. "if p(x) then q(x) else r(x)"

s(X) :- p(X), !, q(X). 
s(X) :- r(X).
p(a).
q(e).
r(a).
