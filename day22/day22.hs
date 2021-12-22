import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Function (on)

-- My main issue when trying to solve this was figuring out how to get all the possible ways two cubes could intersect
-- This person's method (subdividing all the ranges and combining afterwards) is very clever and simple to implement
-- https://github.com/Eckankar/AdventOfCode/blob/master/2021/22/solve2.hs

type Point = (Int, Int)
type Range = (Point, Point, Point)
type Instr = (Bool, Range)

main = do
    input <- readFile "input.txt" >>= return . map parseInstr . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = sum . map (volR . clampR) . foldl runInstr []
part2 = sum . map volR . foldl runInstr []

runInstr :: [Instr] -> Instr -> [Instr]
runInstr is i@(c, _) = if c then (i : is') else is'
    where is' = concatMap (subdivide i) is

vol (l, u) | l > u = 0
vol (l, u) = u - l + 1
volR (_, (x, y, z)) = product . map vol $ [x, y, z]

clamp (l, u) = (max (-50) l, min 50 u)
clampR (c, (x, y, z)) = (c, (clamp x, clamp y, clamp z))

overlap ((la, ua), (lb, ub)) = la <= ub && lb <= ua

-- Split a range into subintervals based on overlapping intervals, at most 3
fixOverlap :: Point -> Point -> [Point]
fixOverlap (la, ua) (lb, ub)
    | la < lb = (la, lb - 1) : fixOverlap (lb, ua) (lb, ub)
    | ua > ub = (ub + 1, ua) : fixOverlap (la, ub) (lb, ub)
    | otherwise = [(la, ua)]

subdivide :: Instr -> Instr -> [Instr]
subdivide (_, (ax, ay, az)) (bc, (bx, by, bz))
    | all overlap [(ax, bx), (ay, by), (az, bz)] = 
        [(bc, (x, y, z)) | x <- fixOverlap bx ax,
                           y <- fixOverlap by ay,
                           z <- fixOverlap bz az,
                           -- Filter for subcubes that don't overlap with cube a
                           (not . all overlap) [(x, ax), (y, ay), (z, az)]]
    | otherwise = [(bc, (bx, by, bz))] -- B doesn't overlap with a, so simply return it

parseInstr :: String -> Instr
parseInstr xs = (a == "on", pts) where
    a = head . words $ xs
    pts = (\[x,x',y,y',z,z'] -> ((x,x'), (z,z'), (y,y')) )
        . map read . filter (all isIntChar) . groupBy ((==) `on` isIntChar) $ xs
isIntChar = or . sequence [isDigit, (== '-')]