import Data.Map (Map, empty, insertWith, fromList, fromListWith, toList, elems, assocs, (!))
import qualified Data.Map as M (map)
import Data.List (group, sort, sortBy)

type Rules = Map (Char, Char) Char
type Pairs = Map (Char, Char) Int

main = do
    (tmp, rls) <- readFile "input.txt" >>= return . parseInp . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 rls tmp
    putStrLn $ (++) "Part 2: " $ show $ part2 rls tmp

part1 rls = uncurry (-) . lastHead . sort . elems . countOcc . (!! 10) . iterate (applyRules rls)
part2 rls = uncurry (-) . lastHead . sort . elems . countOcc . (!! 40) . iterate (applyRules rls)

lastHead = (\xs -> (last xs, head xs))

countOcc :: Pairs -> Map Char Int
countOcc = fromListWith (+) . map (\((a, b), c) -> (a, c)) . assocs

applyRule :: Rules -> ((Char, Char), Int) -> [((Char, Char), Int)]
applyRule _ ((b, '0'), c)  = [((b, '0'), c)] -- Special case for the last character
applyRule rls (p@(a,b), c) = [((a, rls ! p), c), ((rls ! p, b), c)]

applyRules :: Rules -> Pairs -> Pairs
applyRules rls = fromListWith (+) . concatMap (applyRule rls) . toList

parseInp :: [String] -> (Pairs, Rules)
parseInp (x:_:xs) = (mkPairs x, fromList $ map (go . words) xs) where
    go :: [String] -> ((Char, Char), Char)
    go ([a,b]:_:[[c]]) = ((a,b), c)

    mkPairs :: String -> Pairs
    mkPairs [x] = fromList [((x, '0'), 1)] -- Special case to count the last character
    mkPairs (a:b:cs) = insertWith (+) (a, b) 1 $ mkPairs (b:cs)