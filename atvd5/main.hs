import Data.List
main = do
    print "Hello Haskell"

data Paises = Paises
    { nome :: [Char]
    , confirmed :: Integer
    , deaths :: Integer
    , recovery :: Integer
    , active :: Integer
    }
    deriving (Show, Eq)
-- https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally
-- Stack overflow de 14 anos atrás. Esse é o sentido da vida honestamente.
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delimiter list =
  map (takeWhile (/= delimiter) . tail)
    (filter (isPrefixOf [delimiter])
       (tails
           (delimiter : list)))
csvSplit :: String -> Paises
csvSplit s =
    case splitOn ',' s of
        [n, c, d, r, a] ->
            Paises
                { nome = n
                , confirmed = read c
                , deaths = read d
                , recovery = read r
                , active = read a
                }
	_ -> error "Impossivel"
