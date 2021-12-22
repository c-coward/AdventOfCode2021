import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Function (on)
import Data.Maybe (catMaybes, fromJust)

-- My main issue when trying to solve this was figuring out how to get all the possible ways two cubes could intersect
-- This person's method (subdividing all the ranges and combining afterwards) is very clever and simple to implement
-- https://github.com/Eckankar/AdventOfCode/blob/master/2021/22/solve2.hs

type Point = (Int, Int)
type Range = (Point, Point, Point)
type Instr = (Bool, Range)

main = do
    input <- readFile "input.txt" >>= return . map parsePts . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = sum . map volR . catMaybes . map (clampR . snd) . foldl runInstr []
part2 = sum . map volR . map snd . foldl runInstr []

runInstr :: [Instr] -> Instr -> [Instr]
runInstr is i@(c, rs) = if c then (i : is') else is' where
    is' = concatMap (subdivide i) is

vol (l, u) = u - l + 1
volR (x, y, z) = product . map vol $ [x, y, z]

clamp (l, u)
    | l' > u' = Nothing
    | otherwise = Just (l', u')
    where (l', u') = (max (-50) l, min 50 u)
clampR (x, y, z)
    | any null [x', y', z'] = Nothing
    | otherwise = Just (fromJust x', fromJust y', fromJust z')
    where (x', y', z') = (clamp x, clamp y, clamp z)

overlap :: Point -> Point -> Bool
overlap (la, ua) (lb, ub) = la <= ub && lb <= ua

-- Split a range into subintervals based on overlapping intervals
-- This is at most 3 intervals
fixOverlap :: Point -> Point -> [Point]
fixOverlap (la, ua) (lb, ub)
    | la < lb = (la, lb - 1) : fixOverlap (lb, ua) (lb, ub)
    | ua > ub = (ub + 1, ua) : fixOverlap (la, ub) (lb, ub)
    | otherwise = [(la, ua)]

-- Split b into all non-overlapping sub intervals with a
subdivide :: Instr -> Instr -> [Instr]
subdivide (ac, (ax, ay, az)) (bc, (bx, by, bz))
    | overlap bx ax && overlap by ay && overlap bz az = 
        [(bc, (x, y, z)) | x <- fixOverlap bx ax,
                           y <- fixOverlap by ay,
                           z <- fixOverlap bz az,
                           -- Filter for subcubes that don't overlap with cube a
                           (not . all (uncurry overlap)) [(x, ax), (y, ay), (z, az)]]
    | otherwise = [(bc, (bx, by, bz))]

parsePts :: String -> Instr
parsePts xs = let
    a = head . words $ xs
    pts = (\[x,x',y,y',z,z'] -> ((x,x'), (z,z'), (y,y')) )
        . map read . filter (all isIntChar) . groupBy ((==) `on` isIntChar) $ xs
    in case a of
        "on" -> (True, pts)
        "off" -> (False, pts)
isIntChar = or . sequence [isDigit, (== '-')]