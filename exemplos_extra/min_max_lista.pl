% Encontra o maximo e o minimo de uma lista de numeros

% modo de consulta min_max(Lista, Min, Max).

min_max([H|T], Min, Max) :- vmin_max(T, H, H, Min, Max).
% caso base
vmin_max([], Min, Max, Min, Max).
% passo recursivo

% if H < min then min = H else if H > max them max = H else continue
vmin_max([H|T], CurrentMin, CurrentMax, Min, Max) :-
    H < CurrentMin, !, vmin_max(T, H, CurrentMax, Min, Max).
vmin_max([H|T], CurrentMin, CurrentMax, Min, Max) :-
    H > CurrentMax, !, vmin_max(T, CurrentMin, H, Min, Max).
vmin_max([_|T], CurrentMin, CurrentMax, Min, Max) :-
    vmin_max(T, CurrentMin, CurrentMax, Min, Max).


% Usando min e max padrao do prolog
%vmin_max([H|T], CurrentMin, CurrentMax,Min, Max) :- NMin is min(H, CurrentMin), 
%                                                    NMax is max(H, CurrentMax),
%                                                    vmin_max(T, NMin, NMax, Min, Max).
