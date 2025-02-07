-- estudos haskel
  ----------------------------------- Introdução -----------------------------------

fact1 0 = 1 -- se n for 0 retorno 1
fact1 n = n * fact1 (n-1) -- passo indutivo: retorna n * fatorial(n-1)

fact2 n = if n == 0 then 1 else n * fact2 (n-1) -- mais próximo das outras linguagens

-- uso de guards -> como se fossem switch/case
fact3 n
    |n == 0 = 1
    |otherwise = n * fact3 (n-1)


-- Operações

double1 x = 2 * x

double2 = (2 *) -- função restringida, ele sabe que essa função recebe 1 argumento

-- notação prefixada
-- (-) ((*) 5 ((+) 4 6)) 2 
-- (-) ((*) 5 10) 2
-- (-) (50) 2
-- 48

-- Operações básicas

somar :: Int -> Int -> Int
somar a b = a + b

subtrair :: Int -> Int -> Int
subtrair a b = a - b

multiplicar :: Int -> Int -> Int
multiplicar a b = a * b

dividir :: Int -> Int -> Double
dividir a b = if b == 0 then error "Isso é um erro criado por mim, não vá dividir um número por zero em!!!!" else fromIntegral (a) / fromIntegral (b)

-- Área de um círculo com variaveis locais

areaCirculo :: Double -> Double
areaCirculo r =
    let pi = 3.14159
    in pi * r * r

----------------------------------- Listas -----------------------------------

pares = [0, 2, 6, 8]
impares = [1, 3 .. 9] -- gera a lista até 9, sem precisar colocar os valores identifica um padrão

pares2 = [2*x | x <- [0 .. 10]] -- para cada x em  multiplica ele por 2

pares3 = [2*x | x <- [0,1 ..]] -- vai até o infinito

-- Números que dividem n
func n = [f | f <- [1 .. n], mod n f == 0]


-- 8:[] -- retorna [8] (concatena oito dentro de uma lista)

-- head pares --(cabeça da lista)
-- tail pares --restos dos elementos 2 4 6 8
-- head (tail pares) -- 2
-- tail (tail pares) -- 4 6 8
-- head [6,8] -- 6
-- tail [6,8] -- 8
-- tail [9] -- []

-- [1,2]++[3,4]++[5] -- retorna [1,2,3,4,5]
-- pares++impares -- retorna [0,2,6,8,1,3,5,7,9]

-- testar se lista vazia
-- null pares -- retorna false

--Exemplos

elem' c xs = [x | x <- xs, x == c]
removeNonUppercase st = [c | c <- st, elem' c ['A' .. 'Z'] == [c]]

length' xs = sum [1 | _ <- xs ]

boomBangs xs = [ if x < 10 then " BOOM ! " else " BANG ! " | x <- xs , odd x ]

xEy = [ x * y | x <- [2 ,5 ,10] , y <- [8 ,10 ,11] , x * y > 50]

-- let xxs = [[1 ,3 ,5 ,2 ,3 ,1 ,2 ,4 ,5] ,[1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9] ,[1 ,2 ,4 ,2 ,1 ,6 ,3 ,1 ,3 ,2 ,3 ,6]]
evenNoFlattering xxs = [[x | x <- xs, even x] | xs <- xxs]

----------------------------------- Tuples -----------------------------------

-- zip [1 ,2 ,3 ,4 ,5] [5 ,5 ,5 ,5 ,5]
-- zip [1 .. 5] [ " one " , " two " , " three " , " four " , " five " ]
-- saída: [(1 , " one " ) ,(2 , " two " ) ,(3 , " three " ) ,(4 , " four " ) ,(5 , " five " )]
-- Problema: qual triangulo retângulo tem inteiros em cada lado e todos os lados menores e iguais a 10 tem um perímetro igual a 24?
triangles = [ (a ,b , c) | c <- [1..10] , b <- [1..c] , a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24 ]

----------------------------------- Tipos -----------------------------------

--tipos:
-- Boll, Int e Integer, Char, String cadeias de caracteres, Float
-- Integer é infinito, Int somente 32 bits

-- ghci > : t ’a ’
-- ’a ’ :: Char
-- ghci > : t True
-- True :: Bool
-- ghci > : t " HELLO ! "
-- " HELLO ! " :: [ Char ]
-- ghci > : t ( True , ’a ’)
-- ( True , ’a ’) :: ( Bool , Char )
-- ghci > : t 4 == 5
-- 4 == 5 :: Bool

-- not tipo booleano

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

compn :: Char -> Bool
compn n = n == 'k'

-- Num -> tipos diferentes Float, Double, Int e Integer

-- Num é uma typeclass, um grupo de tipos, que consiste em todos os tipos considerados como números.

-- Questão 3 da prova: falar a parte do Fractional ao dividir. (1) e (5) são polimórficos e podem assumir valores Double.
-- Todo fractional é um Num mas nem todo Num é um Fractional

-- Não pode usar 5 / length([1,2,3]) pois length retorna um inteiro
-- O resultado é Int, ou seja, o resultado não é polimórfico. Como um Int não é um Fractional, Haskell não deixa usá-lo com (/).

-- Solução usar o fromIntegral: 5 / fromIntegral (length [1,2,3])

-- string pode ser 
hello = ['h','e','l','l','o']
-- é equivalente a
hello2 = "hello"

---- Tuplas
-- fst -> primeiro elemento
-- snd -> segundo elemento
t1 :: (String, Integer)
t1 = ("Num", 1)

t2 :: ((Integer, Integer), ([Integer], Integer))
t2 = ((1,2), ([1,2],5))

---- Integral
-- Inclui todos os números incluindo reais e integrais, nesse tipo temos  Int e Integer

---- Floating
-- Inclui apenas os números de ponto flutuante, Float e Double

---- fromIntegral 
-- fromIntegral :: (Num b, Integral a) => a -> b
-- A partir dessa assinatura pega um número Integral e transforma em um número mais geral
-- bom para trabalhar com Float e Integer juntos

----------------------------------- Funções -----------------------------------

---- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = " LUCKY NUMBER SEVEN ! "
lucky x = " Sorry , you ’ re out of luck , pal ! "

-- 
sayMe :: ( Integral a ) => a -> String
sayMe 1 = " One ! "
sayMe 2 = " Two ! "
sayMe 3 = " Three ! "
sayMe 4 = " Four ! "
sayMe 5 = " Five ! "
sayMe x = " Not between 1 and 5 "

-- sempre incluir um catch
charName :: Char -> String
charName 'a' = " Albert "
charName 'b' = " Broseph "
charName 'c' = " Cecil "

-- Exception : tut . hs :(53 ,0) -(55 ,21): Non - exhaustive patterns in function charName

-- 
addVectors :: ( Num a ) => (a , a ) -> (a , a ) -> (a , a )
addVectors ( x1 , y1 ) ( x2 , y2 ) = ( x1 + x2 , y1 + y2 )

-- Ignorando elementos com _
first :: (a , b , c ) -> a
first (x , _ , _ ) = x -- faz o mesmo que fst
second :: (a , b , c ) -> b
second (_ , y , _ ) = y -- faz o mesmo que snd
third :: (a , b , c ) -> c
third (_ , _ , z ) = z -- não existe um trd

-- length com funções
length2' [] = 0
length2' xs = 1 + length2' (tail xs)

-- sum
sum2:: Num a => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

---- Guards

fact3' n
    |n == 0 = 1
    |otherwise = n * fact3 (n-1)


bmiTell :: ( RealFloat a ) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = " You're underweight , you emo , you ! "
    | weight / height ^ 2 <= 25.0 = " You're supposedly normal . Pffft , I betyou're ugly ! "
    | weight / height ^ 2 <= 30.0 = " You're fat ! Lose some weight , fatty ! "
    | otherwise = " You're a whale , congratulations ! "

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

---- Where

-- melhorando bmi
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

---- Let

-- let <bindings> in <expression>
cylinder :: Double -> Double -> Double
cylinder  r h =
    let sideArea = 2 * pi * r * h
        topArea = 2 * pi * r
    in sideArea + 2 * topArea

calcBmis :: ( RealFloat a ) => [( a , a )] -> [ a ]
calcBmis xs = [ bmi | (w , h ) <- xs , let bmi = w / h ^ 2 , bmi >= 25.0]

---- Case expressions


-- case expression of pattern -> result
--                    pattern -> result
--                    pattern -> result
--                            ...


----------------------------------- Recursão -----------------------------------

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum (xs)

len :: Num a => [a] -> a
len [] = 0
len (_:xs) = 1 + len (xs)

penultimo :: (Num a) => [a] -> a
punultimo [a, b] = a
penultimo (_:xs) = penultimo (xs)

doisUltimos :: (Num a) => [a] -> [a]
doisUltimos [] = error "Minimo dois elementos necessarios"
doisUltimos [a,b] = [a,b]
doisUltimos (_:xs) = doisUltimos (xs)

