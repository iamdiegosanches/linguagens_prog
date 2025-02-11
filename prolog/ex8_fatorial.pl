fatorial(0, 1).
fatorial(K, F) :- K > 0, K1 is K - 1, fatorial(K1, F1), F is K * F1.
