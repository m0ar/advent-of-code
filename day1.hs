module Main where

count :: String -> Int
count [x]    = upOrDown x
count (x:xs) = upOrDown x + count xs

upOrDown :: Char -> Int
upOrDown c | c == '(' = 1
           | c == ')' = -1

basement :: String -> Int -> Int -> Int
basement _ i (-1)   = i
basement (x:xs) i f = basement xs (i+1) (f+upOrDown x)

main :: IO ()
main = do
    input <- readFile "input1.txt"
    putStrLn $ "Santa ends up at floor " ++ (show . count $ input)
    putStrLn $ "Santa enters the basement at floor " ++ (show $ basement input 0 0)
