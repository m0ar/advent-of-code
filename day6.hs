module Main where
import Data.List.Split
import Control.DeepSeq

off :: Int -> Int
off _ = 0

on :: Int -> Int
on _ = 1

toggle :: Int -> Int
toggle 1 = 0
toggle 0 = 1

s2f :: String -> Int -> Int
s2f s = case s of
            "on" -> on
            "off" -> off
            "toggle" -> off


-- Takes two coordinates and a grid, applies 
-- function to the specified rectangle
-- and returns the resulting grid
applyGrid :: (Int -> Int) -> [(Int,Int)] -> [[Int]] -> [[Int]]
applyGrid f [(x1,y1),(x2,y2)] g = [[apply (x,y) | y <- [0..999]] | x <- [0..999]]
    where 
        apply (x,y) = if x >= x1 && x <= x2 && y >= y1 && y <= y2
                      then f $ (g !! x) !! y
                      else (g !! x) !! y

-- Takes "x,y" and returns (x,y) shamelessly.
createPoint :: String -> (Int,Int)
createPoint s = (head pair, last pair)
    where pair = map (read :: String -> Int) (splitOn "," s)


applyRec :: [[String]] -> [[Int]] -> [[Int]]
applyRec [x]    g = applyGrid (s2f $ head x) 
                              [createPoint $ x !! 1, createPoint $ x !! 2] 
                              g
applyRec (x:xs) g = applyRec xs $!! applyRec [x] g

clean :: String -> [String]
clean ('t':'u':'r':'n':' ':s) = clean s
clean s                       = [head parts, parts !! 1, parts !! 3]
    where parts = words s


main :: IO ()
main = do
    content <- fmap lines (readFile "input6.txt")
    let input = map clean content
    let grid = applyRec input [[0 | x <- [0..999]] | y <- [0..999]]
    let countOn = sum $ map sum grid
    putStrLn $ "Total lights on: " ++ show countOn
