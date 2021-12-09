import Data.Map (fromList, (!))
import Data.List (partition, (\\), sort)

import Debug.Trace (trace)

main = do
    input <- readFile "input.txt" >>= return . map splitInp . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = sum . map (length . findCounts [2,3,4,7] . snd)
part2 = sum . map (digsToInt . deriveValues)

digsToInt :: [Int] -> Int
digsToInt = foldl (\b a -> (b * 10) + a) 0

-- See all the work I did for derivation at the bottom
deriveValues :: ([String], [String]) -> [Int]
deriveValues (ins, outs) = let
    (one, no1ins) = part ((== 2) . length) ins
    (four, no4ins) = part ((== 4) . length) no1ins
    (sev, no7ins) = part ((== 3) . length) no4ins
    (eight, no8ins) = part ((== 7) . length) no7ins

    onethree = four \\ one

    (three, no3ins) = part (\s -> subStr one s && 5 == length s) no8ins
    (five, no5ins) = part (\s -> subStr onethree s && 5 == length s) no3ins
    (two, no2ins) = part ((5 ==) . length) no5ins

    (nine, no9ins) = part (\s -> subStr four s && 6 == length s) no2ins
    (six, no6ins) = part (\s -> subStr five s && 6 == length s) no9ins
    (zero, no0ins) = part ((6 ==) . length) no6ins

    derived = fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4),
                        (five, 5), (six, 6), (sev, 7), (eight, 8), (nine, 9)]

    in map ((derived !) . sort) outs

part :: (String -> Bool) -> [String] -> (String, [String])
part f = (\(a, b) -> (sort $ head a, b)) . partition f

subStr :: String -> String -> Bool
subStr a b = all (`elem` b) a

findCounts :: [Int] -> [String] -> [String]
findCounts cs = filter ((`elem` cs) . length)

splitInp :: String -> ([String], [String])
splitInp = (\(a, b) -> (a, tail b)) . span (/= "|") . words

{-

DERIVING SIGNAL SETS FROM DISPLAY VALUES:
    [1,3] = .4 - .1 { This is the only one needed }

DERIVING DISPLAY VALUE FROM SIGNALS + LENGTH
    -- TRIVIAL
    .1 = LENGTHOF 2
    .7 = LENGTHOF 3
    .4 = LENGTHOF 4
    .8 = LENGTHOF 7

    -- DERIVED
    .3 = LENGTHOF 5 & .1
    .5 = LENGTHOF 5 & [1,3]
    .2 = REMAINING LENGTHOF 5

    .9 = LENGTHOF 6 & .4
    .6 = LENGTHOF 6 & .5
    .0 = REMAINING LENGTHOF 6 

SIGNAL POSITIONS:
    0000
   1    2
   1    2
    3333
   4    5
   4    5
    6666

DISPLAY VALUE SIGNAL POSITION SETS:
    .0 = [0,1,2,4,5,6];   .1 = [2,5]
    .2 = [0,2,3,4,6];     .3 = [0,2,3,5,6]
    .4 = [1,2,3,5];       .5 = [0,1,3,5,6]
    .6 = [0,1,3,4,5,6];   .7 = [0,2,5]
    .8 = [0,1,2,3,4,5,6]; .9 = [0,1,2,3,5,6]

-}