import Data.Map (Map, mapKeys, findWithDefault, delete, insertWith, fromList, empty, elems)
import Data.Char (isDigit)
import Data.List (nubBy, groupBy)

main = do
    input <- readFile "input.txt" >>= return . createMap . parseInts
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = foldl (+) 0 . elems . runGenerations 80
part2 = foldl (+) 0 . elems . runGenerations 256

runGenerations :: Int -> Map Int Int -> Map Int Int
runGenerations 0 = id
runGenerations n = runGenerations (n - 1) . decrementTimer

decrementTimer :: Map Int Int -> Map Int Int
decrementTimer = flipZeroes . mapKeys (subtract 1)

flipZeroes :: Map Int Int -> Map Int Int
flipZeroes m = let
    zs = findWithDefault 0 (-1) m
    m8s = insertWith (+) 8 zs m
    in insertWith (+) 6 zs (delete (-1) m8s)

createMap :: [Int] -> Map Int Int
createMap = foldl (\b a -> insertWith (+) a 1 b) empty

groupInp g f = filter g . groupBy (\a b -> f a && f b)
parseInts = map read . groupInp (isDigit . head) isDigit :: String -> [Int]