module Main.Utility where

groupInp g f = filter g . groupBy (\a b -> f a && f b)
parseInts = map read . groupInp (isDigit . head) isDigit :: String -> [Int]