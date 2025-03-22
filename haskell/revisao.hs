-- 1. Tipos Algébricos e Classes
{- 
   Defina um tipo FormaGeometrica que pode ser Circulo (com raio) 
   ou Retangulo (com largura e altura).
   Implemente a função area :: FormaGeometrica -> Float.
   Derive as classes Show e Eq para o tipo.
-}

data FormaGeometrica = Circulo {raio::Float} | Retangulo {base::Float, altura::Float} deriving (Show, Eq, Ord)

area ::FormaGeometrica -> Float
area (Circulo raio) = pi*raio^2
area (Retangulo base altura) = base*altura

-- Exemplos:
-- area (Circulo 3.0)    -- 28.27433
-- area (Retangulo 4 5)  -- 20.0

----------------------------------------------------------------
-- 2. Functors e Maybe
{-
   Crie aplicarSeguro :: (a -> b) -> Maybe a -> Maybe b.
   Use fmap implicitamente.
-}

aplicarSeguro :: (a -> b) -> Maybe a -> Maybe b
aplicarSeguro = fmap

-- Exemplos:
-- aplicarSeguro (+2) (Just 3)  -- Just 5
-- aplicarSeguro (*2) Nothing   -- Nothing

----------------------------------------------------------------
-- 3. Fold e Listas Infinitas
{-
   Implemente somaQuadradosPares :: Integer -> Integer que retorna 
   a soma dos quadrados dos primeiros n números pares.
-}

somaQuadradosPares :: Integer -> Integer
somaQuadradosPares 0 = 0
somaQuadradosPares n = (2 * n)^2 + somaQuadradosPares (n-1)

-- Exemplo:
-- somaQuadradosPares 3  -- 56 (2² + 4² + 6²)

----------------------------------------------------------------
-- 4. Tratamento de Erros com Either
{-
   Crie dividirSeguro :: Float -> Float -> Either String Float.
   Retorne Left se divisor for zero.
-}

dividirSeguro :: Float -> Float -> Either String Float
dividirSeguro _ 0 = Left "Divisão por zero"
dividirSeguro x y = Right (x / y)

-- Exemplos:
-- dividirSeguro 10 2  -- Right 5.0
-- dividirSeguro 5 0   -- Left "Divisão por zero"

----------------------------------------------------------------
-- 5. IO e Interatividade
{-
   Escreva um programa que pergunte nome e idade e imprima 
   se é maior de idade.
-}

maiorDeIdade = do
    putStrLn "Digite o seu nome:"
    input <- getLine
    let nome = input
    putStrLn "Digite a sua idade: "
    input <- getLine
    let idade = read input
    if idade >= 18 then
        putStrLn $ nome ++ ", você é maior de idade"
    else
        putStrLn $ nome ++ ", você não é maior de idade"


-- Exemplo de execução:
-- Nome: João
-- Idade: 20
-- "João, você é maior de idade!"

----------------------------------------------------------------
-- 6. Applicatives e Listas
{-
   Implemente combinarListas :: [a -> b] -> [a] -> [b] usando <*>.
-}

combinarListas :: [a -> b] -> [a] -> [b]
combinarListas funcoes elementos = funcoes <*> elementos

-- Exemplo:
-- combinarListas [(*2), (+3)] [1, 2]  -- [2,4,4,5]

----------------------------------------------------------------
-- 7. Mônadas e Maybe
{-
   Implemente mediaSegura :: [Int] -> Maybe Double.
   Use >>= para evitar divisão por zero.
-}


-- Exemplos:
-- mediaSegura [10, 20, 30]  -- Just 20.0
-- mediaSegura []            -- Nothing

----------------------------------------------------------------
-- 8. Funções de Alta Ordem e Lambdas
{-
   Converta dobrarPares para usar map e lambda.
-}

-- Exemplo:
-- dobrarPares [1,2,3,4]  -- [4,8]

----------------------------------------------------------------
-- 9. Tipos Sinônimos e Registros
{-
   Defina CPF como sinônimo de String e o registro Pessoa.
   Implemente ehValido para verificar CPF com 11 dígitos.
-}

-- Exemplo:
-- ehValido (Pessoa "Ana" 25 "12345678901")  -- True

----------------------------------------------------------------
-- 10. Lazy Evaluation e Listas Infinitas
{-
   Implemente primos :: [Integer] usando Crivo de Eratóstenes.
-}

-- Exemplo:
-- take 5 primos  -- [2,3,5,7,11]

----------------------------------------------------------------
-- 11. Pattern Matching Avançado
{-
   Implemente contarPadroes :: [Int] -> Int para contar [1,3].
-}

-- Exemplo:
-- contarPadroes [1,3,1,3,5,1,3]  -- 3

----------------------------------------------------------------
-- 12. Classe Functor Personalizada
{-
   Crie o tipo Box e sua instância de Functor.
-}

-- Exemplo:
-- fmap (+2) (Box 3)  -- Box 5

