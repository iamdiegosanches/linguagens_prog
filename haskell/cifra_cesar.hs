import System.IO (hFlush, stdout)

letrasMaiuscula :: [Char]
letrasMaiuscula = ['A'..'Z']
letrasMinuscula :: [Char]
letrasMinuscula = ['a'..'z']

indexElemento :: (Eq a) => a -> [a] -> Int
indexElemento _ [] = -1
indexElemento a (x:xs)
    | a == x    = 0
    | otherwise = 1 + indexElemento a xs

cript :: String -> Int -> String
cript string desloc = map (\c ->
    if c `elem` letrasMaiuscula
        then letrasMaiuscula !! ((indexElemento c letrasMaiuscula + desloc) `mod` 26)
    else if c `elem` letrasMinuscula
        then letrasMinuscula !! ((indexElemento c letrasMinuscula + desloc) `mod` 26)
    else c) string

decript :: String -> Int -> String
decript string desloc = map (\c ->
    if c `elem` letrasMaiuscula
        then letrasMaiuscula !! ((indexElemento c letrasMaiuscula - desloc) `mod` 26)
    else if c `elem` letrasMinuscula
            then letrasMinuscula !! ((indexElemento c letrasMinuscula - desloc) `mod` 26)
    else c) string

chave :: IO Int
chave = do
    putStr "Digite a chave (1-26): "
    hFlush stdout
    input <- getLine
    let desloc = read input :: Int -- Converte a entrada para Int
    if desloc >= 1 && desloc <= 26
        then return desloc
        else do
            putStrLn "Chave inválida. Insira um valor entre 1 e 26."
            chave

menu :: IO ()
menu = do
    putStrLn "\n--- Menu ---"
    putStrLn "1. Criptografar"
    putStrLn "2. Descriptografar"
    putStrLn "3. Sair"

    putStr "Escolha uma opção: "
    hFlush stdout
    choice <- getLine

    case choice of
        "1" -> do
            putStr "Digite a mensagem para criptografar: "
            hFlush stdout
            msg <- getLine
            chave <- chave
            let encrypted = cript msg chave
            putStrLn $ "Mensagem criptografada: " ++ encrypted
            menu
        "2" -> do
            putStr "Digite a mensagem para descriptografar: "
            hFlush stdout
            msg <- getLine
            chave <- chave
            let decrypted = decript msg chave
            putStrLn $ "Mensagem descriptografada: " ++ decrypted
            menu
        "3" -> putStrLn "Saindo..."
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            menu

main :: IO ()
main = do
    menu