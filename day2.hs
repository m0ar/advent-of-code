module Main where
import Data.List
import Data.List.Split

wrapping :: [[Int]] -> Int
wrapping lwh = sum $ map (\[l,w,h] -> surface l w h + extra [l,w,h]) lwh
    where surface l w h = 2*l*w + 2*w*h + 2*h*l
          extra = product . take 2 . sort

ribbon :: [[Int]] -> Int
ribbon lwh = sum $ map (\[l,w,h] -> bow [l,w,h] + l*w*h) lwh
    where bow = sum . map (*2) . take 2 . sort

main :: IO ()
main = do
    input <- readFile "input2.txt"
    let lwh_s = map (splitOn "x") $ lines input
    let lwh = map (map (read :: String -> Int)) lwh_s
    putStrLn $ "Total sq feet of wrapping paper: " ++ show (wrapping lwh)
    putStrLn $ "Total feet of ribbon: " ++ show (ribbon lwh)

