import Data.Map (Map, notMember, insert, findWithDefault, empty)
import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Array.IArray

import Debug.Trace (trace, traceShow)

type Point = (Int, Int)

main = do
    input <- readFile "input.txt" >>= return . map (map digitToInt) . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

-- Part 2 chugs a bit, but still finishes in a reasonable time in GHCI
-- Compiled, it runs a decent bit quicker
part1 = flip (dijkstra empty) [(0, (1, 1))] . inpToArr
part2 = flip (dijkstra empty) [(0, (1, 1))] . inpToArr . expandCave

-- I'm not used to writing Dijkstra's in haskell, so its a bit messy
dijkstra :: Map Point Int -> Array Point Int -> Heap -> Int
dijkstra v g hp
    | c == t    = d
    | d > v ? c = dijkstra v g $ popH hp
    | otherwise = dijkstra v' g hp'
    where
        (d, c@(cy, cx)) = minH hp
        t = snd $ bounds g
        
        neighbs = filter (and .
            sequence [(bounds g `inRange`), (\p -> (d + (g ! p)) < (v ? p))])
            [(cy + y, cx + x) | y <- [-1..1], x <- [-1..1], abs y /= abs x]
        
        v' = foldr (\p -> insert p (d + g ! p)) v neighbs
        hp' = foldr (\p -> insertH (d + g ! p, p)) (popH hp) neighbs
        
        m ? p = findWithDefault maxSize p m
        
        maxSize = (*) 10 $ uncurry (*) t

expandCave :: [[Int]] -> [[Int]]
expandCave = transpose . map (expandRow) . transpose . map (expandRow)

expandRow :: [Int] -> [Int]
expandRow = concat . take 5 . iterate (map ((% 9) . (+1)))

a % m = ((a - 1) `mod` m) + 1

inpToArr :: [[Int]] -> Array Point Int
inpToArr xs = listArray ((1,1),(length xs, length $ head xs)) $ concat xs

-- Emulate a heap by keeping a list in sorted order
-- This isn't as time efficient, but is easier to implement
type Heap = [(Int, Point)]

insertH :: (Int, Point) -> Heap -> Heap
insertH x []    = [x]
insertH a@(x, _) (b@(y, _):ys)
    | x <= y    = a : b : ys
    | otherwise = b : insertH a ys

minH :: Heap -> (Int, Point)
minH (x:_) = x

popH :: Heap -> Heap
popH (_:xs) = xs