import Data.List
import Data.Char (isDigit)

-- 1. Use o fmap para dobrar os elementos de uma lista
dobrarElementos :: (Functor f, Num b) => f b -> f b
dobrarElementos = fmap (*2)

-- 2. Escreva um programa que receba m valor do teclado depois escreva o fatorial do numero
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

fatorialN :: IO Integer
fatorialN = getLine >>= (\input -> return (read input)) >>=  (\numero -> return (fatorial numero))

fatorialN' :: IO ()
fatorialN' = do
    putStrLn "Digite um número para calcular o fatorial:"
    input <- getLine
    let numero = read input
    let resultado = fatorial numero
    putStrLn ("O fatorial de " ++ show numero ++ " é " ++ show resultado)

-- 3. Escreva um programa que receba uma função lambda pelo teclado e aplique essa função em uma lista
auxiliar :: String -> Maybe (Int -> Int)
auxiliar funcString =
    case words funcString of
        [lambda, "->", var, op, numStr]
            | lambda == "\\" ++ var && all isDigit numStr -> case op of
                "+" -> Just (\x -> x + read numStr)
                "-" -> Just (\x -> x - read numStr)
                "*" -> Just (\x -> x * read numStr)
                "/" -> Just (\x -> x `div` read numStr)
                _   -> Nothing

        [lambda, "->", var1, op, var2]
            | lambda == "\\" ++ var1 && var1 == var2 -> case op of
                "+" -> Just (\x -> x + x)
                "-" -> Just (\x -> x - x)
                "*" -> Just (\x -> x * x)
                "/" -> Just (\x -> x `div` x)
                _   -> Nothing

        _ -> Nothing

aplicar :: [Int] -> IO ()
aplicar lista = do
    putStrLn "Escreva a função a ser calculada:"
    input <- getLine
    case auxiliar input of
        Nothing -> putStrLn "Função inválida!"
        Just func -> do
            let resultado = func <$> lista
            putStrLn $ "Resultado: " ++ show resultado


-- 4. Escreva um programa que leia uma coleção de funções em um arquivo texto e aplique em uma lisa (utilize >>=, <*>, <$>)
aplicarArquivo :: [Int] -> IO ()
aplicarArquivo lista = do
    putStrLn "Escreva o nome do arquivo:"
    getLine >>= \nomeArquivo -> 
        readFile nomeArquivo >>= \conteudo ->
            let funcoes = map auxiliar (lines conteudo)
                funcoesValidas = sequence funcoes
            in case funcoesValidas of
                Nothing -> putStrLn "Uma função está inválida!"
                Just fs -> do
                    let resultado = aplicarFuncoes lista fs
                    putStrLn $ "Resultado: " ++ show resultado

aplicarFuncoes :: [Int] -> [Int -> Int] -> [Int]
aplicarFuncoes lista funcoes = foldl (\acc f -> f <$> acc) lista funcoes
