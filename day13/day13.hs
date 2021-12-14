import Data.List (groupBy)
import Data.Array.IArray
import Data.Set (Set, insert, empty, fromList, delete, findMin, findMax, member)
import qualified Data.Set as S (filter, map)

type Point = (Int, Int)
data Fold = X Int | Y Int deriving Show

main = do
    (points, folds) <- readFile "input.txt" >>= return . parseInp . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 folds points
    putStrLn $ (++) "Part 2: \n" $ unlines $ part2 points folds
    return ()

part1 fs = length . foldPaper (head fs)
part2 pts = showPoints . foldr foldPaper pts . reverse

showPoints :: Set Point -> [String]
showPoints pts = let
    xs = S.map (\(x, _) -> x) pts
    ys = S.map (\(_, y) -> y) pts
    (xl, xu) = (findMin xs, findMax xs)
    (yl, yu) = (findMin ys, findMax ys)
    in [[if (x, y) `member` pts then '@' else ' ' | x <- [xl..xu]] | y <- [yl..yu]]

foldPaper :: Fold -> Set Point -> Set Point
foldPaper f s = let
    pts = S.filter (filterFold f) s
    rPts = S.map (reflect f) pts
    in foldr insert (foldr delete s pts) rPts

filterFold :: Fold -> Point -> Bool
filterFold (X d) (x, y) = x > d
filterFold (Y d) (x, y) = y > d

reflect :: Fold -> Point -> Point
reflect (X d) (x, y) = (2 * d - x, y)
reflect (Y d) (x, y) = (x, 2 * d - y)

parseInp :: [String] -> (Set Point, [Fold])
parseInp xs = let [a, _, b] = groupBy (\a b -> a /= "" && b /= "") xs
              in (fromList $ map parsePts a, map (toFold . last . words) b)

toFold :: String -> Fold
toFold ('x':_:x) = X $ read x
toFold ('y':_:y) = Y $ read y

parsePts :: String -> Point
parsePts xs = let [a, _, b] = groupBy (\a b -> a /= ',' && b /= ',') xs
              in (read a, read b)