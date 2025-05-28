--Funcionando com o modulo Text.CSV
import qualified Text.CSV as CSV
import Text.Read (readMaybe) --Will help us reading the CSV File and Parsing to int the strings
import Data.Maybe
import Data.List
import Data.Ord


data PandemicData = PandemicData { --Declaring the object
    country :: [Char],
    confirmed :: Integer,
    deaths :: Integer,
    recovery :: Integer,
    active :: Integer
    } deriving (Show, Read, Eq)

--Taking a Record (list of strings in a CSV) and returning maybe a object pandemicData
parseRecord :: CSV.Record -> Maybe PandemicData 
parseRecord [country, confirmedStr, deathsStr, recoveryStr, activeStr] = do --We still need to convert the necessary Strings to Int
     --Converting the necessaries String to String with readMaybe (to handle problematic cases)
    confirmed <- readMaybe confirmedStr
    deaths <- readMaybe deathsStr
    recovery <- readMaybe recoveryStr
    active <- readMaybe activeStr

    Just $ PandemicData country confirmed deaths recovery active --Constructing a pandemicData if everything was right
parseRecord _ = Nothing --If one maybe returned nothing, the parseRecord will return nothing 

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

    --Reading and saving the records
    Right csv <- CSV.parseCSVFromFile "dados.csv"
    let pandemicData = [p | Just p <- map parseRecord csv]  --For each row in CSV, applie parseRecord. Than, include in a list comprehension
    --First Answer
    putStrLn $ show $ sum $ map active $ filter ((>n1).confirmed) pandemicData
    --Second Answer
    putStrLn $ show $ sum $ map deaths $ take n3 $ sortBy (comparing confirmed) $ take n2 $ sortBy (flip (comparing active)) pandemicData
    --Third Answer
    putStrLn $ show $ sort $ map country $ take n4 $ sortBy (flip (comparing confirmed)) pandemicData