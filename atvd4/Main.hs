main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    la <- getLine
    let a = map read (words la) :: [Int]
    putStrLn $ show $ soma $ tuplas a

tuplas :: [Int] -> [(Int, Int)]
tuplas [] = []
tuplas [x] = [(x, 0)]
tuplas (x : y : xs)
    | x == 10 = (10, 0) : tuplas (y : xs)
    | otherwise = (x, y) : tuplas xs

soma :: [(Int, Int)] -> Int
soma [] = 0
soma [(a, b)] 
  | a /= 10 = a + b
  | otherwise = 0
soma ((a, b) : (c, d) : []) 
  | a /= 10 && a + b < 10 = a + b
  | a /= 10 && a + b == 10 = a + b + c
  | a == 10 && c /= 10 = a + c + d
  | a == 10 && c == 10 = a + c
  | otherwise = 0
soma ((a, b) : (c, d) : (e, f) : xs)
  | a /= 10 && a + b < 10 = a + b + soma ((c, d) : (e, f) : xs)
  | a /= 10 && a + b == 10 = a + b + c + soma ((c, d) : (e, f): xs)
  | a == 10 && c /= 10 = a + c + d + soma ((c, d) : (e, f): xs)
  | a == 10 && c == 10 = a + c + e + soma ((c, d) : (e, f) : xs)
  | otherwise = soma ((c, d) : (e, f) : xs)