

-- 1- O que são funções de ordem superior em Haskell? Dê um exemplo simples.

-- funções de ordem superior são aquelas que recebem uma função como argumento ou retornam uma funçãordem
-- Exemplo: map (+5) [1, 2, 3]


-- 2- Explique o funcionamento da função map. O que acontece ao aplicá-la na lista [1,2,3,4] com a função (2*)?

-- Map aplica a função passada como argumento a todos os elementos da lista .
-- multiplca todos os elementos por 2 -> [2,4,6,8]

-- 3- O que a função filter faz? Como usá-la para obter apenas os números pares de [1..10]?

-- Vai retornar a lista que correspondem aos argumentos passados.
numerosPares :: [Integer]
numerosPares = filter (\x -> mod x 2 == 0) [1..10]

-- 4- Como a função foldr pode ser usada para calcular a soma dos elementos de uma lista?

sumar :: (Foldable t, Num a) => t a -> a
sumar = foldr (+) 0 

-- 5- Qual a diferença entre foldr e foldl? Dê um exemplo prático para ilustrar.

-- O fold r começa na lista da direita para a esquerda e o foldl começa da esquerda para a direita

dividir :: (Foldable t, Fractional a) => t a -> a
dividir = foldr (/) 1 

dividil :: (Foldable t, Fractional a) => t a -> a
dividil = foldl (/) 1 

-- 6- Escreva uma função usando map que transforme uma lista de strings em uma lista de seus respectivos comprimentos.

stringLength :: [String] -> [Int]
stringLength = map length 

-- 7- Implemente a função all :: (a -> Bool) -> [a] -> Bool usando foldr.

-- foldr    (a -> b -> b) -> b -> [a] -> b

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\x a -> f x && a) True xs

-- 8- Como você pode utilizar map e filter em conjunto para obter os quadrados dos números ímpares de 1 a 10?

quadradosdosnumerosimpares :: [Integer]
quadradosdosnumerosimpares = map (^2) $ filter (\x -> mod x 2 /= 0) [1..10]

-- 9- Reescreva a função reverse usando foldl.

-- foldl    (b -> a -> b) -> b -> t a -> b

reverse' :: Foldable t => t a -> [a]
reverse' xs = foldl (\a b -> b:a) [] xs