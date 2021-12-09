import Data.Set (Set, union, intersection, union, fromList)
import Data.Map (Map, findWithDefault, empty, insert, insertWith)

main = do
    input <- readFile "input.txt" >>= return . map splitInp . lines
    putStrLn $ show $ part1 input
    putStrLn $ show $ part2 input

part1 = sum . map (length . findCounts [2,3,4,7] . snd)
part2 = solve . fst . head

solve xs = find069 xs $ find235 xs $ find7 xs $ find4 xs $ find1 xs empty

findCounts :: [Int] -> [String] -> [String]
findCounts cs = filter ((`elem` cs) . length)

splitInp :: String -> ([String], [String])
splitInp = (\(a, b) -> (a, tail b)) . span (/= "|") . words

posMap :: Map Char (Set Int)
posMap = foldl (\m x -> insert x eight m) empty "abcdefg"

-- Identify easy cases
findEasy :: Int -> Set Int -> [String] -> Map Char (Set Int) -> Map Char (Set Int)
findEasy k s xss m = foldl (\m' xs ->
        foldl (\m'' x -> insertWith (intersection) x s m'') m' xs)
    m (filter ((== k) . length) xss)

find1 = findEasy 2 one
find7 = findEasy 3 seven
find4 = findEasy 4 four
find8 = findEasy 7 eight -- Useless, since everything is assumed an 8 at the start

-- Hard cases can be defined by unions
find235 = findEasy 5 (two `union` three `union` five)
find069 = findEasy 6 (zero `union` six `union` nine)

-- Signal Position Set definitions
zero  = fromList [0,1,2,4,5,6];  one   = fromList [2,5]
two   = fromList [0,2,3,4,6];    three = fromList [0,2,3,5,6]
four  = fromList [1,2,3,5];      five  = fromList [0,1,3,5,6]
six   = fromList [0,1,3,4,5,6];  seven = fromList [0,2,5]
eight = fromList [0..6];         nine  = fromList [0,1,2,3,5,6]

{-
SIGNAL POSITIONS:

 0000
1    2
1    2
 3333
4    5
4    5
 6666

-}