module Main where

import Data.List

positions :: String -> [(Int,Int)] -> [(Int,Int)]
positions (c:cs) prev@((x,y):l) = case c of
    '^' -> positions cs ((x, y+1):prev)
    'v' -> positions cs ((x, y-1):prev)
    '<' -> positions cs ((x-1, y):prev)
    '>' -> positions cs ((x+1, y):prev)
positions [] prev = prev

onlyDupes :: [(Int,Int)] -> [(Int,Int)]
onlyDupes (x:xs) | x `elem` xs = x:onlyDupes xs
                 | otherwise   = onlyDupes xs
onlyDupes []     = []


main :: IO ()
main = do
    input <- readFile "input3.txt"
    let stops = positions input [(0,0)]
    let moreThanOne = length . nub . onlyDupes $ stops
    putStrLn $ show moreThanOne ++ " houses gets more than one present"