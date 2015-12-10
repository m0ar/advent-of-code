module Main where

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack)

main :: IO ()
main = do    
    let key = "bgvyzdsv"
    let hashish = show . md5 . pack
    let hashes = [(x, hashish (key ++ show x)) | x <- [0..]]
    let win = head $ filter ((=="000000") . take 6 . snd) hashes
                        
    putStrLn $ show (fst win) ++ " is the earliest!"
    putStrLn $ show (snd win) ++ " was the resulting hash."