import Data.Map (Map, empty, insertWith, fromList, fromListWith, toList, elems, assocs, (!))
import qualified Data.Map as M (map)
import Data.List (group, sort, sortBy)

type Rules = Map (Char, Char) Char
type Pairs = Map (Char, Char) Int

main = do
    (tmp, rls) <- readFile "input.txt" >>= return . parseInp . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 rls tmp
    putStrLn $ (++) "Part 2: " $ show $ part2 rls tmp

part1 rls tmp = uncurry (-) $ lastHead $ sort $ elems $ countOcc tmp $ (!! 10) $ iterate (applyRules rls) $ mkPairs tmp
part2 rls tmp = uncurry (-) $ lastHead $ sort $ elems $ countOcc tmp $ (!! 40) $ iterate (applyRules rls) $ mkPairs tmp

lastHead = (\xs -> (last xs, head xs))

-- Only count the first char in a pair to avoid double counting on overlaps
-- Needs the original string since the last char won't get counted normally
countOcc :: String -> Pairs -> Map Char Int
countOcc s = insertWith (+) (last s) 1 . fromListWith (+) . map (\((a, b), c) -> (a, c)) . assocs

applyRule :: Rules -> ((Char, Char), Int) -> [((Char, Char), Int)]
applyRule rls (p@(a,b), c) = [((a, rls ! p), c), ((rls ! p, b), c)]

applyRules :: Rules -> Pairs -> Pairs
applyRules rls = fromListWith (+) . concatMap (applyRule rls) . toList

mkPairs :: String -> Pairs
mkPairs [x] = empty
mkPairs (a:b:cs) = insertWith (+) (a, b) 1 $ mkPairs (b:cs)

parseInp :: [String] -> (String, Rules)
parseInp (x:_:xs) = (x, fromList $ map (go . words) xs) where

    go :: [String] -> ((Char, Char), Char)
    go ([a,b]:_:[[c]]) = ((a,b), c)