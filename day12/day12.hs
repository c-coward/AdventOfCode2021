import Data.Map (Map, empty, insertWith, (!))
import Data.Set (Set, insert, notMember, member)
import qualified Data.Set as S (empty)
import Data.Char (isUpper)

type CaveMap = Map Cave [Cave]
data Cave = L String | S String deriving (Show, Eq, Ord)

main = do
    input <- readFile "input.txt" >>= return . toMap . map parseEdge . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = getPaths S.empty (S "start")
part2 = getPaths2 S.empty (False, S "start")

getPaths :: Set Cave -> Cave -> CaveMap -> Int
getPaths vs curr cmap = case curr of
    S "end" -> 1
    _       -> let
        vs' | small curr = insert curr vs -- Add small caves to the visited list
            | otherwise  = vs
        in sum $ map (flip (getPaths vs') cmap)
            $ filter (`notMember` vs') (cmap ! curr) -- Don't revisit small caves

getPaths2 :: Set Cave -> (Bool, Cave) -> CaveMap -> Int
getPaths2 vs (twc,curr) cmap = case curr of
    S "end" -> 1
    _       -> let
        vs' | small curr = insert curr vs
            | otherwise  = vs
        cs  | twc       = filter (`notMember` vs') (cmap ! curr) -- Only revisit a cave once
            | otherwise = cmap ! curr
        cs' = filter (/= S "start") cs -- Make sure start is visited only once
        twc' = map ((twc ||) . (`member` vs')) cs' -- Update twice if going to a previously visited cave
        in sum $ map (flip (getPaths2 vs') cmap) $ zip twc' cs'

addEdge :: (Cave, Cave) -> CaveMap -> CaveMap
addEdge (a, b) = insertWith (++) a [b] . insertWith (++) b [a]

parseEdge :: String -> (Cave, Cave)
parseEdge xs = let (a, (_:b)) = span (/= '-') xs in (toCave a, toCave b)

small :: Cave -> Bool
small (S _) = True
small _     = False

toCave :: String -> Cave
toCave xs | any isUpper xs = L xs
          | otherwise      = S xs

toMap :: [(Cave, Cave)] -> CaveMap
toMap = foldr addEdge empty