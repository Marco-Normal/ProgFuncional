module Main where

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    la <- getLine
    let a = map read (words la) :: [Int]
    putStrLn $ show $ tuplas a

tuplas :: [Int] -> [(Int, Int)]
tuplas [] = []
tuplas [x] = [(x, 0)]
tuplas (x : y : xs)
    | x == 10 = (10, 0) : tuplas (y : xs)
    | otherwise = (x, y) : tuplas xs
