module Main where

import Data.List
import Data.List.Split

vowels :: String -> Bool
vowels s = 3 <= (length . filter (== True) . map (`elem` "aeiou") $ s)

twice :: String -> Bool
twice s = not $ null [True | x <- [0..length s - 2], s !! x == s !! (x+1)]

hasForbidden :: String -> Bool
hasForbidden s = True `elem` map (($ s) . isInfixOf) ["ab", "cd", "pq", "xy"]

gap :: String -> Bool
gap s = not $ null [True | x <- [0..length s - 3], s !! x == s !! (x+2)]

twoTwice :: String -> Bool
twoTwice s = any ((>2) . length) $ 
    map (`splitOn` s) [[x,y] | x <- ['a'..'z'], y <- ['a'..'z']]

main :: IO ()
main = do
    input <- lines <$> readFile "input5.txt"
    
    let check s =  vowels s && twice s && not (hasForbidden s)
    let nice = length . filter (== True) $ map check input
    putStrLn $ "There are " ++ show nice ++ " strings."

    let check' s = gap s && twoTwice s
    let nice' = length . filter (== True) $ map check' input
    putStrLn $ "With the new rules, there are " ++ show nice' ++ " nice strings."


