import Data.Array.IArray (Array, listArray, bounds, indices, amap, inRange,
    (//), (!))
import Data.Char (digitToInt, intToDigit)
import Data.Set (Set, insert, empty, notMember, toList)

type Point = (Int, Int)
type Board = Array Point Int
type Vis = Set Point

main = do
    input <- readFile "input.txt" >>= return . inpToArr . lines
    putStrLn $ show $ part1 input
    putStrLn $ show $ part2 input

part1 = snd . countFlashes 99 . flash
part2 = findSync 1 . flash

findSync :: Int -> (Board, Int) -> Int
findSync i (b, c)
    | c == length (indices b) = i
    | otherwise               = findSync (i + 1) $ flash b

countFlashes :: Int -> (Board, Int) -> (Board, Int)
countFlashes 0 ib = ib
countFlashes x (b, i) = let (b', i') = flash b in countFlashes (x - 1) (b', i + i')

neighbs :: Board -> Point -> [Point]
neighbs b (y, x) = filter (inRange $ bounds b)
    [(y + i, x + j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]

flash :: Board -> (Board, Int)
flash = step3 . step2 . step1

step1 :: Board -> Board
step1 = amap (+ 1)

step2 :: Board -> (Board, [Point])
step2 b = go b empty $ filter ((> 9) . (b !)) $ indices b where
    go :: Board -> Set Point -> [Point] -> (Board, [Point])
    go b vs [] = (b, toList vs) -- No more flashes
    go b vs fs = let
        fns = concat $ map (neighbs b) fs -- Get vals to update
        b' = foldl updateBoard b fns -- Update Board
        vs' = foldl (flip insert) vs fs -- Add flashes to vis
        fs' = filter (and . sequence [(`notMember` vs'), ((> 9) . (b' !))])
            $ indices b' -- Get next flash gen
        in go b' vs' fs'

step3 :: (Board, [Point]) -> (Board, Int)
step3 (b, fs) = (b // [(i, 0) | i <- fs], length fs)

updateBoard :: Board -> Point -> Board
updateBoard b pt = b // [(pt, 1 + b ! pt)]

showBoard :: Board -> String
showBoard b = unlines [[intToDigit (b ! (i, j)) | j <- [lj..uj]] | i <- [li..ui]] where
    ((li, lj), (ui, uj)) = bounds b

inpToArr :: [String] -> Board
inpToArr (xs) = listArray ((1,1), (length xs, length $ head xs))
        $ map digitToInt $ concat xs