-- 1) Faça um programa em haskell que ordene um vetor (ordem crescente).

verifica :: Ord t => t -> [t] -> [t]
verifica element [] = [element]
verifica element (x:xs) | element > x = x:verifica element xs
                        | otherwise = element:x:xs

ordena :: (Num a, Ord a) => [a] -> [a]
ordena = foldr (\element acc -> verifica element acc) [] 


-- 2) Diga por que o código abaixo é inválido:
-- [[1,2],3,[4,5]]

-- Resposta: porque itens de uma lista devem ter o mesmo tipo de dados. Ou seja, a lista deveria ser uma lista de listas [[a]] ou 
-- uma lista comum [a]

-- 3) Use head e tail para escrever uma função que retorne o quinto elemento de uma lista.

quintoElemento :: [a] -> a
quintoElemento lista = head $ tail $ tail $ tail $ tail lista

-- 4) Escreva um código que peça para o usuário entrar com a base e a altura de um retângulo e que
-- calcule sua área. Ele deve apresentar o resultado na tela.

area :: Num a => a -> a -> a
area base altura = base * altura

areaRetangulo = do
    putStrLn "Digite a base do retangulo:"
    input <- getLine
    let base = read input
    putStrLn "Digite a altura do retangulo:"
    input <- getLine
    let altura = read input
    let resultado = area base altura
    putStrLn $ "A área do retângulo é: " ++ show resultado ++ "cm"

-- 5) Converta os seguintes comandos utilizando lambda:
-- map f xs ,onde f x = x * 1 + 2

conversao = map (\x -> x * 1 + 2)  

-- 6) Use o comando foldl para definir uma função “reverso :: [a] -> [a]”, ele deverá retornar uma lista
-- de elementos na ordem inversa.

reverso :: [a] -> [a]
reverso = foldl (\acc elemento -> elemento : acc ) []

-- 7) Escreva uma função com dois argumentos x e n, na saída da função x deve ser replicado n vezes.
-- Exemplo: replica 3 10 = [10,10,10]

replica :: (Eq t1, Num t1) => t1 -> t2 -> [t2]
replica 0 x = [] 
replica n x = x : replica (n-1) x

-- 8) Escreva uma função que retorne os n primeiros elementos de uma lista. Ex:
-- Primeiros 2 [10,33,44,61,99] = [10,33]

primeiros :: (Eq t, Num t) => t -> [a] -> [a]
primeiros 0 xs = []
primeiros _ [] = error "List not big enough"
primeiros n (x:xs) = x : primeiros (n-1) xs

-- 9) Escreva uma função que ignore os n primeiros elementos de uma lista e retorne o restante. Ex:
-- ignore 2 [10,33,44,61,99] = [44,61,99]

ignore :: (Eq t, Num t) => t -> [a] -> [a]
ignore 0 xs = xs
ignore _ [] = []
ignore n (x:xs) = ignore (n-1) xs

-- 10) Os seguintes comandos são equivalentes?
-- (a) fmap (+2) Just 3
-- (b) (+2) <*> Just 3

-- Não. O comando (a) é do tipo functor. O comando (b) é um applicative e neste caso ainda gera um erro, 
-- pois o (+2) também precisaria estar encapsulado com o Just assim como o Just 3, para que ambos tenham o tipo Maybe
-- O equivalente ao fmap é o <$>
