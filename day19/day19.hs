import Data.List (groupBy, intersect, transpose, nub, partition, uncons, union)
import Data.Char (isDigit)
import Data.Function (on)
import Data.Set (Set, fromList, toList, insert, empty, intersection, unions)
import Data.Maybe (fromJust)

type Point = (Int, Int, Int)
type Scanner = [Point]
type Dists = [[Int]]

main = do
    input <- readFile "input.txt" >>= return . parseScanners . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = length . unions . map (fromList . fst) . lineups
part2 = maximum . manDists . map snd . lineups

manDists :: [Point] -> [Int]
manDists xs = [((\(a, b, c) -> abs a + abs b + abs c) . diff x) y |
            x <- xs, y <- xs, x /= y]

lineup :: [Scanner] -> [Scanner] -> [(Scanner, Point)]
lineup _ [] = []
lineup (x:xs) ys = zs ++ lineup xs' ys' where
    (xs'', ys') = partition (overlaps x) ys
    zs = map (recenter x) xs''
    xs' = (map fst zs) ++ xs

lineups (x:xs) = (x, (0, 0, 0)) : lineup [x] xs

recenter :: Scanner -> Scanner -> (Scanner, Point)
recenter a b = go a $ permute b where
    go x (y:ys) = case tryMatch x y of
        Nothing -> go x ys
        Just y' -> y'

tryMatch :: Scanner -> Scanner -> Maybe (Scanner, Point)
tryMatch xs ys = go ds where
    ds = [diff x y | x <- xs, y <- ys]
    go [] = Nothing
    go (z:zs)
        | (>= 12) $ length $ (`intersect` xs) $ ys' = Just (ys', z)
        | otherwise = go zs
        where ys' = map (padd z) ys

overlap :: Dists -> Dists -> Bool
overlap as bs = any (>= 11) [length $ intersect a b | a <- as, b <- bs]

overlaps :: Scanner -> Scanner -> Bool
overlaps as bs = any (overlap $ distSet as) $ map distSet $ permute bs

distSet :: Scanner -> Dists
distSet xs = [[sqDiff x y | y <- xs, x /= y] | x <- xs]

sqDiff (a, b, c) (d, e, f) = ((a - d)^2) + ((b - e)^2) + ((c - f)^2)
diff (a, b, c) (d, e, f) = (a - d, b - e, c - f)
padd (a, b, c) (d, e, f) = (a + d, b + e, c + f)

permute = transpose . map perms

perms :: Point -> [Point]
perms (x, y, z) = [
    (x, y, z), (z, y, -x), (-x, y, -z), (-z, y, x),
    (y, -x, z), (z, -x, -y), (-y, -x, -z), (-z, -x, y),
    (-x, -y, z), (z, -y, x), (x, -y, -z), (-z, -y, -x),
    (-y, x, z), (y, x, -z), (z, x, y), (-z, x, -y),
    (y, z, x), (y, -z, -x), (-y, -z, x), (-y, z, -x),
    (x, z, -y), (-x, -z, -y), (-x, z, y), (x, -z, y)]

parseScanners :: [String] -> [Scanner]
parseScanners = map (map parsePoint . tail) . filter (/= [""]) . groupBy ((&&) `on` (/= ""))
parsePoint = (\[a,b,c] -> (a, b, c)) . map read . filter (any isIntChar) . groupBy ((&&) `on` isIntChar)
isIntChar = or . sequence [isDigit, (== '-')]