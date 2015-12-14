module Main where
import Data.List.Split
import Control.DeepSeq
import Data.Matrix

off :: Bool -> Bool
off _ = False

on :: Bool -> Bool
on _ = True

toggle :: Bool -> Bool
toggle = not

s2f :: String -> Bool -> Bool
s2f s = case s of
            "on" -> on
            "off" -> off
            "toggle" -> toggle


-- Takes two coordinates and a grid, applies 
-- function to the specified rectangle
-- and returns the resulting grid
applyGrid :: (Bool -> Bool) -> [(Int,Int)] -> Matrix Bool -> Matrix Bool
applyGrid f [(x1,y1),(x2,y2)] g = matrix 1000 1000 apply
    where 
        apply (x,y) = if x >= x1 && x <= x2 && y >= y1 && y <= y2
                      then f $ g ! (x,y)
                      else g ! (x,y)


-- Takes "x,y" and returns (x,y) shamelessly.
createPoint :: String -> (Int,Int)
createPoint s = (head pair, last pair)
    where pair = map (read :: String -> Int) (splitOn "," s)


applyRec :: [[String]] -> Matrix Bool -> Matrix Bool
applyRec (x:[])    g = applyGrid (s2f $ head x) 
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
    let grid = applyRec input (matrix 1000 1000 (const False))
    let count = foldl (\acc b -> if b then acc + 1 else acc) 0 grid
    putStrLn $ "Total lights on: " ++ show count
