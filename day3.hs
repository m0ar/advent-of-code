module Main where

import Data.List

positions :: String -> [(Int,Int)] -> [(Int,Int)]
positions (c:cs) prev@((x,y):l) = case c of
    '^' -> positions cs ((x, y+1):prev)
    'v' -> positions cs ((x, y-1):prev)
    '<' -> positions cs ((x-1, y):prev)
    '>' -> positions cs ((x+1, y):prev)
positions [] prev = prev

everyOther :: String -> String
everyOther (x:y:xs) = x : everyOther xs
everyOther (x:xs)   = [x]
everyOther []       = []

main :: IO ()
main = do
    input <- readFile "input3.txt"
    let stops = positions input [(0,0)]
    let moreThanOne = length . nub $ stops
    putStrLn $ show moreThanOne ++ " houses gets more than one present"

    let posSanta = positions (everyOther input) [(0,0)]
    let posRobo  = positions (everyOther $ tail input) [(0,0)]
    let moreThanOneRobo = length . nub $ posSanta ++ posRobo
    putStrLn $ show moreThanOneRobo ++ " houses get more than one present, with the robot"

