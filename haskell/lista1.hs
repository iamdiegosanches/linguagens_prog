-- Primeira lista de Haskell

-- 1) Escreva a assinatura das seguintes funções:

dobro :: Num a => a -> a
dobro x = x * 2

primeiro :: (a, b) -> a
primeiro (x, y) = x

maiorQue10 :: (Ord a, Num a) => a -> Bool
maiorQue10 x = x > 10

somaLista :: Num a => [a] -> a
somaLista xs = sum xs

multiplicaLista :: Num a => [a] -> a
multiplicaLista xs = product xs


-- 2) Defina e implemente uma função chamada ehPar que recebe um número inteiro e retorna um booleano indicando se ele é par. Escreva a assinatura da função.

ehPar :: Integral a => a -> Bool
ehPar x | mod x 2 == 0 = True
        | otherwise = False

ehPar' :: Integral a => a -> Bool
ehPar' = even

ehPar2 :: (Integral a, Eq a) => a -> Bool
ehPar2 x = mod x 2 == 0

-- 3) Implemente uma função quadrado que recebe um número x e retorna x ao quadrado. Escreva a assinatura da função.

quadrado :: Num a => a -> a
quadrado x = x^2

-- 4) Implemente uma função recursiva fatorial :: Integer -> Integer que calcula o fatorial de um número.

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

-- 5) Implemente uma função recursiva somaN :: Integer -> Integer que retorna a soma de todos os números de 1 até n.

somaN :: Integer -> Integer
somaN 0 = 0
somaN n = n + somaN (n-1)

-- 6) Implemente uma função recursiva fibonacci :: Integer -> Integer que calcula o n-ésimo termo da sequência de Fibonacci.
-- 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- 7) Implemente uma função contaElementos :: [a] -> Int que conta quantos elementos há em uma lista. Não use length.

contaElementos :: [a] -> Int
contaElementos [] = 0
contaElementos (_:xs)  = 1 + contaElementos xs

-- 8) Implemente uma função reverter :: [a] -> [a] que inverte uma lista. Não use reverse.

reverter :: [a] -> [a]
reverter [] = []
reverter (x:xs) =  reverter xs ++ [x]

-- 9) Defina um novo tipo chamado DiaSemana representando os dias da semana.

data DiaSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
  deriving (Show, Eq, Enum, Bounded)

-- 10) Implemente a função ehFimDeSemana :: DiaSemana -> Bool que retorna True se for sábado ou domingo e False caso contrário.

ehFimDeSemana :: DiaSemana -> Bool
ehFimDeSemana x | x == Sabado || x == Domingo = True
                | otherwise = False

-- 11) Implemente a função produtoLista :: [Integer] -> Integer que calcula o produto de todos os elementos de uma lista. Não use product.

produtoLista :: [Integer] -> Integer
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

-- 12) Implemente a função recursiva elementoN :: [a] -> Int -> a que retorna o n-ésimo elemento de uma lista (o primeiro elemento tem índice 0). Caso o
-- índice seja inválido, retorne um erro.

elementoN :: [a] -> Int -> a
elementoN [] i = error "index too large"
elementoN (x:_) 0 = x
elementoN (x:xs) i = elementoN xs (i-1)

-- 13) Implemente a função somaImpares :: [Integer] -> Integer que recebe uma lista de inteiros e retorna a soma dos números ímpares.

somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (x:xs) | odd x = x + somaImpares xs
                   | otherwise = somaImpares xs

-- 14) Implemente a função contaOcorrencias :: Eq a => a -> [a] -> Int que conta quantas vezes um determinado elemento aparece em uma lista.

contaOcorrencias :: Eq a => a -> [a] -> Int
contaOcorrencias _ [] = 0
contaOcorrencias n (x:xs) | n == x = 1 + contaOcorrencias n xs
                          | otherwise = contaOcorrencias n xs

-- 15) Implemente a função removeElemento :: Eq a => a -> [a] -> [a] que remove todas as ocorrências de um elemento da lista.

removeElemento :: Eq a => a -> [a] -> [a]
removeElemento _ [] = []
removeElemento n (x:xs) | n == x = removeElemento n xs
                        | otherwise = x:removeElemento n xs

-- 16) Implemente a função duplicarElementos :: [a] -> [a] que duplica cada elemento da lista.

duplicarElementos :: [a] -> [a]
duplicarElementos [] = []
duplicarElementos (x:xs) = x:x:duplicarElementos xs

duplicarElementos' :: [a] -> [a]
duplicarElementos' x = [y | f <- x, y <- [f,f]]

-- 17) Implemente a função intercalar :: [a] -> [a] -> [a] que intercala os elementos de duas listas. Se uma lista for maior que a outra, os elementos
-- extras devem ser adicionados ao final.

intercalar :: [a] -> [a] -> [a]
intercalar [] [] = []
intercalar [] x = x
intercalar x [] = x
intercalar (x1:xs1) (x2:xs2) = x1:x2:intercalar xs1 xs2

-- 18) Implemente a função removerDuplicatas :: Eq a => [a] -> [a] que remove elementos repetidos consecutivos de uma lista.

-- remove as duplicatas em sequência de dois a dois
removerDuplicatas :: Eq a => [a] -> [a]
removerDuplicatas [a] = [a]
removerDuplicatas (x1:xs) | x1 == head xs = x1:removerDuplicatas (tail xs)
                          | otherwise = x1:removerDuplicatas xs

-- remove as ocorrências em sequência
removerDuplicatas' :: Eq a => [a] -> [a]
removerDuplicatas' [a] = [a]
removerDuplicatas' (x1:xs) | x1 == head xs = removerDuplicatas' (x1:tail xs)
                           | otherwise = x1:removerDuplicatas' xs
