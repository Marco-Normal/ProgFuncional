
import Data.List
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
    let n1 = read la --Read is converting to a number (Pure function)
    
    lb <- getLine     
    let n2 = read lb
    lc <- getLine 
    let n3 = read lc 
    ld <- getLine       
    let n4 = read ld
    handle <- openFile "dados.csv" ReadMode
    contents <- hGetContents handle
    let line = lines contents
    let pandemicData = map csvSplit line
    --First Answer
    putStrLn $ show $ sum $ map active $ filter ((>n1).confirmed) pandemicData
    --Second Answer
    putStrLn $ show $ sum $ map deaths $ take n3 $ sortBy (comparing confirmed) $ take n2 $ sortBy (flip (comparing active)) pandemicData
    --Third Answer
    putStrLn $ show $ sort $ map country $ take n4 $ sortBy (flip (comparing confirmed)) pandemicData




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

