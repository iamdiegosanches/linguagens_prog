----------------------------------- Exercícios -----------------------------------

---- Recursão

-- Faça uma função recursiva que calcule e retorne o N-ésimo termo da sequência Fibonacci. Alguns números desta sequência são: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89...
fibo :: (Integral a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- Crie uma função recursiva que receba um número inteiro positivo N e calcule o somatório dos números de 1 a N.
somaN :: Integer -> Integer
somaN 0 = 0
somaN n = n + somaN (n-1)

-- Produto dos elementos de uma lista
prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod (xs)

-- Escreva uma função recursiva que inverta ordem dos elementos presentes no vetor.
inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

-- O máximo divisor comum dos inteiros x e y é o maior inteiro que é divisível por x e y. Escreva uma função recursiva mdc, que retorna o máximo divisor comum de x
-- e y. O mdc de x e y é definido como segue: se y é igual a 0, então mdc(x,y) é x; caso contrário, mdc(x,y) é mdc (y, x%y), onde % é o operador resto.
mdc :: (Ord t, Num t) => t -> t -> t
mdc x y
  | x == y = x
  | x > y = mdc (x-y) y
  | otherwise = mdc (y-x) x

mdc' :: Integral t => t -> t -> t
mdc' x 0 = x
mdc' x y = mdc' y (mod x y)

-- Escreva uma função recursiva que determine quantas vezes um item K ocorre em um vetor N.
conta :: (Num a, Eq t) => t -> [t] -> a
conta _ [] = 0
conta a (x:xs) = if a == x then 1 + conta a xs else conta a xs

-- A multiplicação de dois números inteiros pode ser feita através de somas sucessivas. Proponha um algoritmo recursivo Multip_Rec(n1,n2) que calcule a
-- multiplicação de dois inteiros.
multipRec :: Integer -> Integer -> Integer
multipRec 0 _ = 0
multipRec a b = b + multipRec (a - 1) b

-- Retorne o número do meio de um vetor
meio :: [a] -> a
meio [] = error "Lista tamanho par ou vazia"
meio [x] = x
meio xs = meio (init (tail xs))

-- ponto no meio da lista
meio' :: (Floating a) => [a] -> a
meio' [] = error "Lista vazia"
meio' [x] = x
meio' [a, b] = (a + b) / 2
meio' xs = meio' (init (tail xs))

-- maior elemento
maior :: (Ord a, Fractional a) => [a] -> a
maior [x] = x
maior (x:xs) | x > maior xs = x 
             | otherwise = maior xs

-- calcular a mediana de uma lista de valores
mediana :: (Fractional a, Ord a) => [a] -> a
mediana [] = error "Lista vazia"
mediana [x] = x
mediana [a, b] = (a + b) / 2
mediana xs = mediana (init (tail (bubbleSort xs)))

bubbleSort :: (Fractional a, Ord a) => [a] -> [a]
bubbleSort xs = go xs (length xs)
  where
    go lst 0 = lst
    go lst n = go (pass lst) (n - 1)

    pass (x:y:xs) | x > y     = y : pass (x:xs)
                  | otherwise = x : pass (y:xs)
    pass xs = xs

ocorrencia :: Ord t => t -> [t] -> Bool
ocorrencia _ [] = False
ocorrencia y (x:xs) = if x == y then True else ocorrencia y xs

-- Elementos da lista maiores que a
maiorQue' :: Ord t => t -> [t] -> [t]
maiorQue' _ [] = []
maiorQue' a (x:xs) = if x > a then x : maiorQue' a xs else maiorQue' a xs

duplica :: [a] -> [a]
duplica [] = []
duplica (x:xs) = x : x : duplica xs

concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (x:xs) l = x:concatena xs l

-- unica ocorrencia
unicaOcorrencia :: Eq t => t -> [t] -> Bool
unicaOcorrencia _ [] = False
unicaOcorrencia a (x:xs) | x == a = if (unicaOcorrencia a xs) then False else True
                         | otherwise = unicaOcorrencia a xs

---- List comprehension

quadradoNumero :: Num a => [a] -> [a]
quadradoNumero x = [f*f | f <- x]

maiorQue :: Ord a => a -> [a] -> [a]
maiorQue a x = [f | f <- x, f > a]


pontos :: (Ord a1, Num a1, Num a2) => a1 -> a2
pontos p
    | p >= 1 && p <= 10 = 100
    | p >= 11 && p <= 20 = 200
    | p >= 21 && p <= 30 = 300
    | p >= 31 && p <= 40 = 400
    | otherwise = 500

par :: Integral a => a -> Bool
par p = mod p 2 == 0

menor :: Ord a => a -> a -> a
menor x y
    | x < y = x
    | otherwise = x

menor' :: Ord a => a -> a -> a -> a
menor' x y z
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z

duplica' :: [a] -> [a]
duplica' x = [y | f <- x, y <- [f,f]]
