import Data.List (transpose)
import Data.Char (digitToInt)

import Debug.Trace (traceShow)

type BitString = [Int] -- List of 0s and 1s

main = do
    input <- readFile "input.txt" -- Do the common input processing
        >>= return . map (map (digitToInt)) . lines

    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = seqProd [id, map (1-)] . map (reverseN0 . sum . map negate0) . transpose
part2 = seqProd [filterReduce id, filterReduce (1-)]

filterReduce :: (Int -> Int) -> [BitString] -> BitString
filterReduce f xs = case length xs == 1 of
    True -> head xs
    _    -> let
        fBit = f $ reverseN0 $ sum $ map (negate0 . head) xs
        xs' = map tail $ filter ((== fBit) . head) xs
        in (fBit : filterReduce f xs')

-- Convert string a bits to decimal integer
bToI :: BitString -> Int
bToI = foldl (\b a -> 2*b + a) 0

negate0, reverseN0 :: Int -> Int
-- By converting 0s to -1s, summing over the list gives the "count" of each bit
negate0 = (subtract 1) . (2 *)
-- reverseN0 provides a convenient way to get back to the binary representation
-- however, 0s are converted to 1s due to the demands of the problem
reverseN0 = fromEnum . (>= 0)

-- Product over a sequencing of bitstring functions
seqProd :: [a -> BitString] -> a -> Int
seqProd fs = product . map bToI . sequence fs