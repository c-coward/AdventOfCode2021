-- import Data.Map (Map, findWithDefault, empty, insert, fromList, elems, keys)
import qualified Data.Set as S (unions, fromList, toList)
import Data.List (groupBy, nub, sort)
import Data.Array.IArray (Array, (!), array, listArray, inRange, bounds, indices, elems)
import Data.Function (on)

type Point = (Int, Int)
type Image = (Array Point Bool, Bool)
type Code = Array Int Bool

main = do
    (code, img) <- readFile "input.txt" >>= return . parseInp . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 code img
    putStrLn $ (++) "Part 2: " $ show $ part2 code img

part1 code = getPtsSize . runGens 2 code
part2 code = getPtsSize . runGens 50 code

runGens :: Int -> Code -> Image -> Image
runGens 0 c i = i
runGens x c i = runGens (x - 1) c (nextGen c i)

getPtsSize :: Image -> Int
getPtsSize (a, _) = sum $ map fromEnum $ elems a

nextGen :: Code -> Image -> Image
nextGen code img@(pts, cnd) = let
    pts' = S.toList $ S.unions $ map (S.fromList . neighbs) $ indices pts
    ((ly, lx), (uy, ux)) = bounds pts
    in (array ((ly-1,lx-1),(uy+1,ux+1)) $ map (decode code img) pts',
        if code ! 0 then not cnd else cnd)

decode :: Code -> Image -> Point -> (Point, Bool)
decode code img pt = let
    pts' = neighbs pt
    bits = foldl (\b a -> 2*b + a) 0 $ map (fromEnum . (img ?)) pts'
    in (pt, code ! bits)

neighbs :: Point -> [Point]
neighbs (y, x) = [(y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1]]

(?) :: Image -> Point -> Bool
(m, c) ? p
    | inRange b p = m ! p
    | otherwise   = c
    where b = bounds m

parseInp :: [String] -> (Code, Image)
parseInp (x:_:ys) = (listArray (0, length x - 1) x', (go ys', False)) where
    x' = map (== '#') x
    ys' = map (map (== '#')) ys
    go zs = listArray ((1, 1), (length zs, length $ head zs)) $ concat zs