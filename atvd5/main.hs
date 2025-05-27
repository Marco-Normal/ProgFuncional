import Data.List.Split
import Data.Map

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
csvSplit :: [Char] -> Paises
csvSplit s =
    case splitOn "," s of
        [n, c, d, r, a] ->
            Paises
                { nome = n
                , confirmed = read c
                , deaths = read d
                , recovery = read r
                , active = read a
                }
	_ -> error "Impossivel"
