-- 1- O que são funções de ordem superior em Haskell? Dê um exemplo simples.

-- Resposta: Funções de ordem superior são funções que recebem outras funções como argumentos ou as retornam como resultado

exemploOrdemSuperior :: [Int]
exemploOrdemSuperior = map (+5) [1,2,3] -- resultado: [6,7,8]

-- 2- Explique o funcionamento da função map. O que acontece ao aplicá-la na lista [1,2,3,4] com a função (2*)?

-- Resposta: Map aplica uma função a cada elemento da lista, criando uma nova lista com os resultados
resultadoMap :: [Int]
resultadoMap = map (2*) [1,2,3,4] -- Resultado: [2,4,6,8]

-- 3- O que a função filter faz? Como usá-la para obter apenas os números pares de [1..10]?

-- Resposta: Filter seleciona elementos que satisfazem um predicado (condição lógica).
numerosPares :: [Integer]
numerosPares = filter (\x -> mod x 2 == 0) [1..10]

-- 4- Como a função foldr pode ser usada para calcular a soma dos elementos de uma lista?

somaLista :: (Foldable t, Num a) => t a -> a
somaLista = foldr (+) 0 -- 1 + (2 + (3 + 0))

-- 5- Qual a diferença entre foldr e foldl? Dê um exemplo prático para ilustrar.

-- Resposta: foldr acumula da direita para esquerda, foldl da esquerda para direita.

dividirFoldr :: (Foldable t, Fractional a) => t a -> a
dividirFoldr = foldr (/) 1 

dividirFoldl :: (Foldable t, Fractional a) => t a -> a
dividirFoldl = foldl (/) 1 

-- 6- Escreva uma função usando map que transforme uma lista de strings em uma lista de seus respectivos comprimentos.

comprimentos :: [String] -> [Int]
comprimentos = map length -- ["Haskell", "map"] → [7,3]

-- 7- Implemente a função all :: (a -> Bool) -> [a] -> Bool usando foldr.

-- foldr (a -> b -> b) -> b -> [a] -> b

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\elemento acc -> f elemento && acc) True xs

-- 8- Como você pode utilizar map e filter em conjunto para obter os quadrados dos números ímpares de 1 a 10?

quadradosImpares :: [Integer]
quadradosImpares = map (^2) $ filter (\x -> mod x 2 /= 0) [1..10]

-- 9- Reescreva a função reverse usando foldl.

-- foldl (b -> a -> b) -> b -> t a -> b

reverse' :: Foldable t => t a -> [a]
reverse' xs = foldl (\acc elemento -> elemento:acc) [] xs