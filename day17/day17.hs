import Data.Char (isDigit)
import Data.List (groupBy, nub)
import Data.Maybe (catMaybes)
import Data.Ix (inRange)

type Point = (Int, Int)

main = do
    input <- readFile "input.txt" >>= return . parseInp
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = maximum . map (maximum . map snd) . catMaybes . map (uncurry (step (0, 0))) . velocities
part2 = length . catMaybes . map (uncurry (step (0, 0))) . velocities

-- Velocities includes the target range for convenience
velocities :: (Point, Point) -> [(Point, (Point, Point))]
velocities t@((lx, ly), (ux, uy)) = [((x, y), t) | x <- [0..ux], y <- [ly..ux]]

step :: Point -> Point -> (Point, Point) -> Maybe [Point]
step c@(cx, cy) (dx, dy) t@((lx,ly), (ux,uy))
    | inRange t c = Just [c]
    | tooFar t c  = Nothing
    | dx == 0 && (not $ inRange (lx, ux) cx)
                  = Nothing
    | otherwise   = case step' of
        Nothing -> Nothing
        Just ps -> Just (c:ps)
    where
        step' = step (cx + dx, cy + dy) ((-) dx $ signum dx, dy - 1) t

tooFar :: (Point, Point) -> Point -> Bool
tooFar ((_, ly), (ux, _)) (x, y) = x > ux || y < ly

-- Parse input to an Ix Range
parseInp = makeTups . splitAt 2 . map read . filter (all isIntChar) . groupBy (\a b -> isIntChar a && isIntChar b)
makeTups ([a,b], [c,d]) = ((a,c), (b,d)) :: ((Int, Int), (Int, Int))
isIntChar = or . sequence [isDigit, (== '-')]