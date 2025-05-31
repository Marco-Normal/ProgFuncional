-- Augusto Fernandes Ildefonso - 15441810
-- Marco Túlio Mello Silva - 12548657
-- Lucas Lombardi Castro - 13672978 

import Data.List
import Data.Ord
import System.IO
data PandemicData = PandemicData { --Declaring the object
    country :: [Char],
    confirmed :: Integer,
    deaths :: Integer,
    recovery :: Integer,
    active :: Integer
    } deriving (Show, Read, Eq)


main :: IO()
main = do
    la <- getLine --Gettint all the line (Impute Function)    
    
    let [a, b, c, d] = words la -- Criando variável
        n1 = read a :: Integer
        n2 = read b :: Int
        n3 = read c :: Int
        n4 = read d :: Int

    handle <- openFile "dados.csv" ReadMode
    contents <- hGetContents handle
    let line = lines contents
    let pandemicData = map csvSplit line
    --First Answer
    putStrLn $ show $ sum $ map active $ filter ((>n1).confirmed) pandemicData
    --Second Answer
    putStrLn $ show $ sum $ map deaths $ take n3 $ sortBy (comparing confirmed) $ take n2 $ sortBy (flip (comparing active)) pandemicData
    --Third Answer
    impressao $ sort $ map country $ take n4 $ sortBy (flip (comparing confirmed)) pandemicData


-- https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally
-- Stack overflow de 14 anos atrás. Esse é o sentido da vida honestamente.
-- Nossas strings vão sair na forma
-- [",a,b,c", ",b,c" ",c"]
-- Vamos pegar a cauda
-- ["a,b,c", "b,c", "c"]
-- Vamos pegar enquanto for diferente de delim
-- ["a", "b", "c"]
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delimiter list =
  map (takeWhile (/= delimiter) . tail) -- pegue enquanto for diferente do delimitador.
    (filter (isPrefixOf [delimiter]) -- Pegamos aquela que tem o delimitador como primeiro elemento
      (tails -- Retorna a cauda
           (delimiter : list))) -- Adiciona um delimitador para conseguirmos o primeiro elemento
csvSplit :: String -> PandemicData
csvSplit s =
    case splitOn ',' s of
        [n, c, d, r, a] ->
            PandemicData
                { country = n
                , confirmed = read c
                , deaths = read d
                , recovery = read r
                , active = read a
                }
        _ -> error "Impossivel"

impressao [] = return()
impressao (x:xs)
    | x /= "" = do
        putStrLn x
        impressao xs
    | otherwise = impressao xs
