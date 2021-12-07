import Data.Char (isDigit)
import Data.List (groupBy, inits)

main = do
    input <- readFile "input.txt" >>= return . parseInts
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 xs = minimum $ map (sum . flip reposition xs) $ extractBounds xs
part2 xs = minimum $ map (sum . flip reposition2 xs) $ extractBounds xs

reposition, reposition2 :: Int -> [Int] -> [Int]
reposition x = map (abs . subtract x)
reposition2 x = map (triangles !!) . reposition x

-- An array is faster, but needs an explicit upper bound
-- A list is slower, but it could potentially handle any index
triangles = map sum $ inits [1..]
-- triangles :: Array Int Int
-- triangles = listArray (0, 10000) $ (0) : [i + triangles ! (i-1) | i <- [1..1000000]]

extractBounds :: [Int] -> [Int]
extractBounds xs = [minimum xs..maximum xs]

groupInp g f = filter g . groupBy (\a b -> f a && f b)
parseInts = map read . groupInp (isDigit . head) isDigit :: String -> [Int]