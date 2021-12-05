import Data.Char (isDigit)
import Data.List (groupBy, intersect, nub)
import qualified Data.Map as M (insertWith, filter, empty)

type Point = (Int, Int)
type LineSeg = (Point, Point)

main = do
    input <- readFile "input.txt" >>= return . map (mkLineSeg . parseInts) . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = length . M.filter (>= 2) . findOverlap . concat . map getLineSegPoints . filter (straightLine)
part2 = length . M.filter (>= 2) . findOverlap . concat . map getLineSegPoints

findOverlap = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

getLineSegPoints :: LineSeg -> [Point]
getLineSegPoints (a, b) = go a b where
    m = slope (a, b)

    go :: Point -> Point -> [Point]
    go l r
        | l == r    = [r]
        | otherwise = l : go (l .+. m) r

    (x0, y0) .+. (x1, y1) = (x0 + x1, y0 + y1)

slope :: LineSeg -> Point
slope ((x0, y0), (x1, y1)) = reduce (x1 - x0, y1 - y0)

straightLine :: LineSeg -> Bool
straightLine ((x0, y0), (x1, y1)) = (x0 == x1) || (y0 == y1)

reduce :: Point -> Point
reduce (a, b) = let g = gcd a b in (a `div` g, b `div` g)

mkLineSeg :: [Int] -> LineSeg
mkLineSeg [x0,y0,x1,y1] = ((x0, y0), (x1, y1))

groupInp g f = filter g . groupBy (\a b -> f a && f b)
parseInts = map read . groupInp (isDigit . head) isDigit :: String -> [Int]