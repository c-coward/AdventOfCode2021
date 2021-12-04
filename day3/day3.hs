import Data.List (transpose)
import Data.Char (digitToInt)

import Debug.Trace (traceShow)

main = do
    input <- readFile "input.txt" -- Do the common input processing
        >>= return . map (map (negate0 . digitToInt)) . lines

    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = complProd [id, map (1-)] . map (reverseN0 . sum) . transpose
part2 = complProd [filterReduce id, filterReduce negate]

filterReduce :: (Int -> Int) -> [[Int]] -> [Int]
filterReduce f xs = case length xs == 1 of
    True -> map reverseN0 $ head xs
    _    -> let
        fBit = f $ negate0 $ (1-) $ reverseN0 $ sum $ map head xs
        xs' = map tail $ filter ((== fBit) . head) xs
        in (reverseN0 fBit : filterReduce f xs')

bToI :: [Int] -> Int
bToI = foldl (\b a -> 2*b + a) 0

-- By converting 0s to -1s, summing over the list gives the "count" of each bit
negate0, reverseN0 :: Int -> Int
negate0 = (subtract 1) . (2 *)
-- reverseN0 provides a convenient way to get back to the binary representation
reverseN0 = fromEnum . (>= 0)

complProd :: [a -> [Int]] -> a -> Int
complProd fs = product . map bToI . sequence fs