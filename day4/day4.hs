import Data.Char (isDigit)
import Data.List (groupBy, transpose, inits, sort, (\\))
import Data.Maybe (catMaybes)

type Board = [[Int]]

main = do
   (calls, boards) <- readFile "input.txt" >>= return . parseInp . lines
   putStrLn $ (++) "Part 1: " $ show $ part1 calls boards
   putStrLn $ (++) "Part 2: " $ show $ part2 calls boards

part1 cs = snd . head . sort . map (flip winScore cs)
part2 cs = snd . last . sort . map (flip winScore cs)

winScore :: Board -> [[Int]] -> (Int, Int)
winScore b = head . catMaybes . map (score b)

score :: Board -> [Int] -> Maybe (Int, Int)
score b c
    | isBingo c b = Just (length c, (*) (last c) $ sum $ concat b \\ c)
    | otherwise   = Nothing

checkRows, checkCols, isBingo :: [Int] -> Board -> Bool
checkRows cs = any (and . \xs -> map (`elem` cs) xs)
checkCols cs = checkRows cs . transpose
isBingo cs = or . sequence [checkRows cs, checkCols cs]

parseInp :: [String] -> ([[Int]], [Board])
parseInp (x:xs) = (inits (parseInts x), parseBoards xs)

groupInp :: ([a] -> Bool) -> (a -> Bool) -> [a] -> [[a]]
groupInp g f = filter g . groupBy (\a b -> f a && f b)
parseInts = map read . groupInp (isDigit . head) isDigit :: String -> [Int]
parseBoards = map (map parseInts) . groupInp (/= [""]) (not . null)