import Data.Map (Map, findWithDefault, empty, insert, fromList, elems, keys)
import qualified Data.Set as S (unions, fromList, toList)
import Data.List (groupBy, nub, sort)
import Data.Array.IArray (Array, (!), listArray, inRange)
import Data.Function (on)

type Point = (Int, Int)
type Image = (Map Point Bool, Bool)
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
    pts' = S.toList $ S.unions $ map (S.fromList . neighbs) $ keys pts
    in (fromList $ map (decode code img) pts',
        if code ! 0 then not cnd else cnd)

decode :: Code -> Image -> Point -> (Point, Bool)
decode code img pt = let
    pts' = neighbs pt
    bits = toIdx $ map (fromEnum . (img ?)) pts'
    in (pt, code ! bits)

toIdx :: [Int] -> Int
toIdx = foldl (\b a -> 2 * b + a) 0

neighbs :: Point -> [Point]
neighbs (y, x) = [(y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1]]

(m, c) ? p = findWithDefault c p m

parseInp :: [String] -> (Code, Image)
parseInp (x:_:ys) = (listArray (0, length x - 1) x', (go 0 0 ys, False)) where
    x' = map (== '#') x
    go _ _ [] = empty
    go i j ([]:yss) = go (i + 1) 0 yss
    go i j (('#':ys):yss) = insert (i, j) True $ go i (j + 1) (ys:yss)
    go i j ((_:ys):yss) = insert (i, j) False $ go i (j + 1) (ys:yss)