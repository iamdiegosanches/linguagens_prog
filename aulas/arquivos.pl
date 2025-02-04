principal :- open('numeros.txt', read, F),
                read(F, C1), read(F, C2), read(F, C3),
                close(F), C4 is C1 + C2 + C3, salvar(C4), write(C4), nl.

salvar(C4) :- open('salvar.txt', write, Out), write(Out, C4), nl(Out), close(Out).

