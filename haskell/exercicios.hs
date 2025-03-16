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

-- Ocorre um item na lista
ocorrencia :: Ord t => t -> [t] -> Bool
ocorrencia _ [] = False
ocorrencia y (x:xs) = if x == y then True else ocorrencia y xs

-- Elementos da lista maiores que a
maiorQue' :: Ord t => t -> [t] -> [t]
maiorQue' _ [] = []
maiorQue' a (x:xs) = if x > a then x : maiorQue' a xs else maiorQue' a xs

-- Duplica itens de uma lista
duplica :: [a] -> [a]
duplica [] = []
duplica (x:xs) = x : x : duplica xs

-- Concatena listas com recursão
concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (x:xs) l = x:concatena xs l

-- unica ocorrencia
unicaOcorrencia :: Eq t => t -> [t] -> Bool
unicaOcorrencia _ [] = False
unicaOcorrencia a (x:xs) | x == a = if (unicaOcorrencia a xs) then False else True
                         | otherwise = unicaOcorrencia a xs

-- Faça uma função que calcule a soma dos dígitos de um número.
somaDigitos :: Integral t => t -> t
somaDigitos 0 = 0
somaDigitos x = mod x 10 + somaDigitos (div x 10)

-- Faça uma função que gere uma matriz identidade de tamanho n.
matrizIdentidade :: Int -> [[Int]]
matrizIdentidade n = [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

-- Faça uma função que calcule a soma da diagonal principal de uma matriz.
somaDiagonal :: [[Int]] -> Int
somaDiagonal [] = 0
somaDiagonal ([]:_) = 0
somaDiagonal ((x:_):ys) = x + somaDiagonal (map tail ys)

-- Faça uma função que calcule a soma da diagonal secundária de uma matriz.
somaDiagonalSecundaria :: [[Int]] -> Int
somaDiagonalSecundaria [] = 0
somaDiagonalSecundaria ([]:_) = 0
somaDiagonalSecundaria (m:ms) = last m + somaDiagonalSecundaria (map init ms)

-- Calcular potencia
potencia :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
potencia _ 0 = 1
potencia x n = x * potencia x (n-1)

-- Fatorial duplo
fatDuplo 2 = 2
fatDuplo 1 = 1
fatDuplo x = x * fatDuplo (x-2)

-- Produto do intervalo dos números
intervalo :: Int -> Int -> Int
intervalo a b | a == b = a
              | a > b = 1
              | otherwise = a*b*intervalo (a+1) (b-1)

-- Defina uma funcao que dada uma lista de numeros calcula a sua media.
soma :: [Double] -> Double
soma [] = 0
soma (x:xs) = x + soma xs

tamanho :: [Double] -> Double
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

media :: [Double] -> Double
media [] = error "Lista vazia!"
media xs = soma xs / tamanho xs

-- Defina uma função somaQuadrados que recebe uma lista de números e retorna a soma dos quadrados dos elementos da lista.
somaQuadrados :: (Num a) => [a] -> a
somaQuadrados [] = 0
somaQuadrados (x:xs) = x*x + somaQuadrados xs

-- Verificar palindromos
palindromo :: String -> Bool
palindromo [] = True
palindromo [a] = True
palindromo (x:xs) | x == last xs = palindromo (init xs)
                  | otherwise = False

-- Faça uma função de ordenação que utilize recursão
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let (left, right) = splitAt (div (length xs) 2) xs
    in merge (mergeSort left) (mergeSort right)


-- Escreva a função rotacionarEsq que recebe uma lista e um número n, e retorna a lista rotacionada n vezes para a esquerda.


-- Implemente a função ehSublista que recebe duas listas e retorna True se a primeira for uma sublista contínua da segunda.


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

-- Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.
mult35 :: Integral a => a -> Bool
mult35 x | mod x 3 == 0 && mod x 5 == 0 = True
         | otherwise = False

-- Faça uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade:
-- sin(x/2) = +- sqrt((1-cos(x))/2)
senoM :: Floating b => b -> (b, b)
senoM x = (sqrt ((1 - cos x) / 2), -sqrt ((1 - cos x) / 2))

-- Encontre os 10 primeiros anos bissextos.
bissexto10 :: [Integer]
bissexto10 = [4*x | x <- [0..9]]

-- Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer
stringData :: [Char] -> [Int]
stringData x = [fromEnum t - 48 | t  <- x]

-- Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
ehTriangulo :: (Ord a, Fractional a) => a -> a -> a -> Bool
ehTriangulo x y z | x + y > z && x + z > y && y + z > x = True
                  | otherwise = False

-- Faça uma função para calcular o produto escalar entre dois vetores. 
produtoEscalar :: Num a => (a, a, a) -> (a, a, a) -> a
produtoEscalar (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

produtoEscalar' :: Num a => [a] -> [a] -> a
produtoEscalar' v1 v2
    | length v1 == length v2 = sum (zipWith (*) v1 v2)
    | otherwise = error "Os vetores devem ter o mesmo tamanho!"

-- Defina uma função que receba dois pares de inteiros e retorne um par de inteiros, sendo o primeiro elemento do par resultado 
-- a soma dos primeiros elementos dos pares de entrada, e o segundo elemento do par, o produto dos segundos elementos dos pares de entrada.
func1 :: Num b => b -> b -> (b, b)
func1 a b = (a+b, a*b)

-- Escreva uma função que, dados três números inteiros, retorne um par contendo no primeiro elemento o maior dos números, e no 
-- segundo elemento o segundo maior dos números.
func2 :: (Ord a, Integral a) => a -> a -> a -> (a, a)
func2 a b c
    | a >= b && b >= c = (a, b)
    | a >= c && c >= b = (a, c)
    | b >= a && a >= c = (b, a)
    | b >= c && c >= a = (b, c)
    | c >= a && a >= b = (c, a)
    | otherwise        = (c, b)

-- Escreva uma função que receba um triplo de números inteiros e retorne um triplo em que os mesmos números estão ordenados 
-- por ordem decrescente.
func3 :: (Ord a, Integral a) => a -> a -> a -> [a]
func3 a b c
    | a >= b && b >= c = [c, b, a]
    | a >= c && c >= b = [b, c, a]
    | b >= a && a >= c = [c, a, b]
    | b >= c && c >= a = [a, c, b]
    | c >= a && a >= b = [b, a, c]
    | otherwise        = [a, b, c]

-- Escreva uma função abrev que receba uma string contendo nome de uma pessoa e retorne uma string com o primeiro nome e apelido1
-- (e.g. (abrev ‘‘Joao Carlos Martins Sarmento’’)=’’Joao Sarmento’’) As funçõoes, pré-definidas, words e unwords poderão ser-lhe uteis
-- • words :: String -> [String], dá como resultado a lista das palavras (strings) de um texto (uma string)
-- • unwords :: [String] -> String, constroi um texto (uma string) a partir de uma
-- lista de palavras (strings)
abrev :: String -> String
abrev nome = unwords [head palavras, last palavras]
  where palavras = words nome

-----------------------------------------------------------------------------------------------------------------------------------

-- Escreva uma função que aplique uma função f n vezes a um valor x.  
-- Exemplo: applyN (+1) 3 5 deve retornar 8 (5+1+1+1).  
-- applyN :: (a -> a) -> Int -> a -> a

applyN :: (a -> a) -> Int -> a -> a
applyN _ 0 x = x
applyN func n x = func (applyN func (n-1) x)

-- Crie uma função que conte quantos elementos de uma lista satisfazem dois predicados simultaneamente.  
-- Exemplo: contarDuplo even (>5) [1..10] retorna 2 (6 e 8 são pares e maiores que 5). 
-- contarDuplo :: (a -> Bool) -> (a -> Bool) -> [a] -> Int

contarDuplo :: (a -> Bool) -> (a -> Bool) -> [a] -> Int
contarDuplo f1 f2 = foldr (\element acc -> if f1 element && f2 element then acc + 1 else acc) 0

-- Implemente uma versão do `map` que aplica a função apenas aos elementos em posições ímpares (1ª, 3ª, 5ª...).  
-- Exemplo: mapImpares (*2) [1,2,3,4,5] retorna [2,6,10].
-- mapImpares :: (a -> a) -> [a] -> [a]

mapImpares :: (a -> a) -> [a] -> [a]
mapImpares _ [] = []
mapImpares f [x] = [f x]
mapImpares f (x:_:xs) = f x:mapImpares f xs

-- Use `foldr` ou `foldl` para calcular o produtório dos elementos de uma lista.  
-- Exemplo: produtorio [2,3,4] retorna 24.  
-- produtorio :: (Foldable t, Num a) => t a -> a

produtorio :: (Foldable t, Num a) => t a -> a
produtorio = foldr (*) 1

-- Escreva uma função que retorne os quadrados dos números ímpares de uma lista, mas apenas se o quadrado for maior que 20.  
-- Exemplo: quadradosFiltrados [1..10] retorna [25,49,81].
-- quadradosFiltrados :: [Int] -> [Int]

quadradosFiltrados :: [Int] -> [Int]
quadradosFiltrados lista = filter (>20) $ map (^2) $ filter odd lista

-- Implemente uma versão simplificada de `groupBy` que agrupa elementos consecutivos de uma lista conforme um predicado.  
-- Exemplo: groupBy (\x y -> x == y) [1,1,2,3,3] retorna [[1,1],[2],[3,3]].  
-- groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f (x:xs) = foldl (\(x:xs) item -> if f item (head x) then (item:x):xs
                                    else [item]:x:xs) [[x]] xs

-- Calcule a média de uma lista de números usando apenas um fold (sem sum ou length separados).  
-- Dica: Acompanhe a soma e a contagem durante o fold.  
-- mediaFold :: (Fractional a, Foldable t) => t a -> a

mediaFold :: (Fractional a, Foldable t) => t a -> a
mediaFold x = soma / contagem
            where (soma,contagem) = foldr (\i (s, c) -> (i + s, 1 + c)) (0,0) x

-- Crie uma função que insira um elemento entre todos os elementos de uma lista.  
-- Exemplo: intercalar 0 [1,2,3] retorna [1,0,2,0,3].
-- intercalar :: a -> [a] -> [a]

intercalar :: a -> [a] -> [a]
intercalar _ [a] = [a]
intercalar i (x:xs) = x:i:intercalar i xs

-- usando foldr
intercalar' :: a -> [a] -> [a]
intercalar' i lista = foldr (\x acc-> x:i:acc) [last lista] (init lista)

-- usando foldr sem usar last e init
intercalar'' :: a -> [a] -> [a]
intercalar'' i = foldr (\x acc -> x : if null acc then [] else i : acc) []

-- Implemente uma função zipWith3 que aplique uma função a três listas simultaneamente, retornando uma lista com os resultados. Exemplo:
-- zipWith3 (\x y z -> x + y * z) [1,2,3] [4,5,6] [7,8,9]

-- Crie uma função composeAll que receba uma lista de funções e as componha da direita para a esquerda. Exemplo:
-- composeAll [(*2), (+3), (^2)] 5

-- Implemente o algoritmo quicksort usando apenas funções de alta ordem (filter, map, etc.) sem recursão explícita.

-- Escreva uma função scanr usando foldr. A função scanr é como foldr, mas retorna uma lista de resultados intermediários.

-- Implemente uma versão polimorfa da função partition que separe elementos de uma lista em dois grupos com base em um predicado.

-- Escreva a função mapAccumL que combina características de map e fold, permitindo manter um estado enquanto transforma uma lista.

-- Crie funções any e all que verificam se pelo menos um elemento ou todos os elementos de uma lista satisfazem um predicado, 
-- mas usando foldl para otimização de tail recursion.
