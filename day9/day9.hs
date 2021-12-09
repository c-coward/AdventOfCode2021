import Data.Array.IArray
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set (Set, empty, insert, member)

type Point = (Int, Int)

main = do
    input <- readFile "input.txt" >>= return . inpToArr . lines
    putStrLn $ show $ part1 input
    putStrLn $ show $ part2 input

part1 = sum . map ((1 + ) . snd) . lowPoints
part2 a = product $ take 3 $ reverse $ sort $ map (length . findBasin a empty . fst) $ lowPoints a

basinSize :: Array Point Int -> Int
basinSize = length . filter (== 10) . elems

findBasin :: Array Point Int -> Set Point -> Point -> Set Point
findBasin a v x
    | x `member` v = v
    | (a ! x) == 9 = v
    | otherwise    = foldl (findBasin a) (insert x v) $ getNeighbs x a

lowPoints :: Array Point Int -> [(Point, Int)]
lowPoints a = filter (flip isLowPoint a . fst) $ assocs a

isLowPoint :: Point -> Array Point Int -> Bool
isLowPoint x a = let
    h = a ! x
    in (&&) (h /= 9) $ all (( > h) . (a ! )) $ getNeighbs x a

neighbs :: [Point]
neighbs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

getNeighbs :: Point -> Array Point Int -> [Point]
getNeighbs x a = filter (flip inb a) $ map (pAdd x) neighbs

inpToArr :: [String] -> Array Point Int
inpToArr xs = listArray ((1,1),(length xs, length $ head xs)) $ concatMap (map digitToInt) xs

pAdd :: Point -> Point -> Point
pAdd (a, b) (c, d) = (a + c, b + d)

inb :: Point -> Array Point e -> Bool
inb x = elem x . indices