import Data.List (intersect)
import Data.Map (Map, findWithDefault)

main = do
    input <- readFile "input.txt" >>= return . lines
    putStrLn $ show $ part1 input

part1 = sum . map (length . findCounts [2,3,4,7] . snd . splitInp)

findCounts :: [Int] -> [String] -> [String]
findCounts cs = filter ((`elem` cs) . length)

splitInp :: String -> ([String], [String])
splitInp = (\(a, b) -> (a, tail b)) . span (/= "|") . words

zero  = [0,1,2,4,5,6];  one   = [2,5]
two   = [0,2,3,4,6];    three = [0,2,3,5,6]
four  = [1,2,3,5];      five  = [0,1,3,5,6]
six   = [0,1,3,4,5,6];  seven = [0,2,5]
eight = [0..6];         nine  = [0,1,2,3,5,6]

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