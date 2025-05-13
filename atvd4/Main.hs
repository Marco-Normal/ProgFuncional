-- Augusto Fernandes Ildefonso 15441810
-- Marco Túlio Mello Silva - 12548657
-- Lucas Lombardi Castro - 13672978 
main :: IO ()
main = do
    la <- getLine
    let a = map read (words la) :: [Int]            --Lendo o input do usuario
    let frames = tuplas a                           --Criando a lista com as tuplas dos frmaes
    let (primeiros9, restante) = splitAt 9 frames   --Seperando os 9 primeiros frames do ultimo frame

    putStrLn $ printPlacarTuplasDefault primeiros9 ++ printUltimoFrame restante ++ " " ++ show (soma a)

tuplas :: [Int] -> [(Int, Int)]
tuplas [] = []
tuplas [x] = [(x, 0)]
tuplas (x : y : xs)
    | x == 10 = (10, 0) : tuplas (y : xs)
    | otherwise = (x, y) : tuplas xs

soma :: [Int] -> Int
soma rolagens = somaAux rolagens 0 1
  where
    somaAux _ pontuacao 11 = pontuacao -- Quando tem 10 frames completos
    somaAux (10:xs) pontuacao frame = somaAux xs (pontuacao + 10 + bonus) (frame + 1)
      where bonus = sum $ take 2 xs
    somaAux (a:b:xs) pontuacao frame
      | a + b == 10 = somaAux xs (pontuacao+ 10 + head xs) (frame + 1) -- Caso seja spare
      | otherwise   = somaAux xs (pontuacao + a + b) (frame + 1)
    somaAux _ pontucao _ = pontucao


printUltimoFrame :: [(Int, Int)] -> String
printUltimoFrame [] = ""
printUltimoFrame [(10,0),(10,0),(10,0)] = "X X X |"               --Tres strikes seguidos
printUltimoFrame [(10,0),(10,0),(a,_)] = "X X " ++ show a ++ " |" --Dois strikes e uma jogada

printUltimoFrame [(10,0),(x,y)] 
 | x + y == 10 = "X " ++ show x ++ " / |"                         --Strike seguido de spare
 | otherwise = "X " ++ show x ++ " " ++ show y ++ " |"            --Strike seguido de jogada normal

printUltimoFrame [(a,b),(c,d)]                   
  | a + b == 10 && c == 10 = show a ++ " / " ++ "X |"             --Spare seguido de strike
  | otherwise = show a ++ " / " ++ show c ++ " |"                 --Spare seguido de jogada normal

printUltimoFrame [(a,b)] = show a ++ " " ++ show b ++ " |"              --Caso default


printPlacarTuplasDefault :: [(Int, Int)] -> String
printPlacarTuplasDefault [] = ""
printPlacarTuplasDefault ((x,y):xs) --Caso default (9 primeiros casos)
  | x == 10      = "X _ | " ++ printPlacarTuplasDefault xs
  | x + y == 10  = show x ++ " / | " ++ printPlacarTuplasDefault xs
  | otherwise    = show x ++ " " ++ show y ++ " | " ++ printPlacarTuplasDefault xs
