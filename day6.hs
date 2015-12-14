module Main where
import Data.List.Split
import Control.DeepSeq
import Data.Matrix

off :: Int -> Int
off i | i > 0 = i-1
      | otherwise = 0 

on :: Int -> Int
on i = i+1

toggle :: Int -> Int
toggle i = i+2

s2f :: String -> Int -> Int
s2f s = case s of
            "on" -> on
            "off" -> off
            "toggle" -> toggle


-- Takes two coordinates and a grid, applies 
-- function to the specified rectangle
-- and returns the resulting grid
applyGrid :: (Int -> Int) -> [(Int,Int)] -> Matrix Int -> Matrix Int
applyGrid f [(x1,y1),(x2,y2)] g = matrix 1000 1000 apply
    where 
        apply (x,y) = if x >= x1 && x <= x2 && y >= y1 && y <= y2
                      then f $ g ! (x,y)
                      else g ! (x,y)


-- Takes "x,y" and returns (x,y) shamelessly.
createPoint :: String -> (Int,Int)
createPoint s = (head pair, last pair)
    where pair = map (read :: String -> Int) (splitOn "," s)


applyRec :: [[String]] -> Matrix Int -> Matrix Int
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
    let grid = applyRec input (matrix 1000 1000 (const 0))
    let count = sum grid
    putStrLn $ "Total brightness: " ++ show count
