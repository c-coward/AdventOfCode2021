import Data.Array.IArray (Array,
    listArray, assocs, (!), (//), elems)
import Data.Char (isDigit)
import Data.List (groupBy, nubBy)
import Data.Maybe (catMaybes, fromJust)

type Cell = Maybe Int
type Pos = (Int, Int)

-- Board with identifier and last-command memory
type Board = (Int, (Int, Array Pos Cell))

main = do
   (input, boards) <- readFile "input.txt" >>= return . parseInp . lines
   putStrLn $ (++) "Part 1: " $ show $ part1 boards input
   putStrLn $ (++) "Part 2: " $ show $ part2 boards input

part1 bs = getScore . winner . findBingos . scanl updateBoards bs
part2 bs = getScore . lastWinner . findBingos . scanl updateBoards bs

matchBoard :: Board -> Board -> Bool
matchBoard (x, _) (y, _) = x == y

-- Map over the generations of boards, filtering for winning boards in each one
findBingos :: [[Board]] -> [[Board]]
findBingos = map (filter (or . sequence [checkRows, checkCols]))

-- Find the first winning board
winner = head . head . dropWhile null
-- Find the last winning board
lastWinner = last . nubBy matchBoard . concat

getScore :: Board -> Int
getScore (_, (i, b)) = (i*) $ sum $ catMaybes $ elems b

-- Get all points on the line defined by the two points
-- First point describes the starting position
-- Second point describes the slope
getPosLine :: Pos -> Pos -> [Pos]
getPosLine s = init . scanl tupAdd s . replicate 5

-- See whether a board has a bingo based on position criteria
checkBingo :: Board -> Pos -> Pos -> Bool
checkBingo (_, (_, b)) s = all (null . (b ! )) . getPosLine s

checkRows b = any (flip (checkBingo b) (1, 0)) (getPosLine (1, 1) (0, 1))
checkCols b = any (flip (checkBingo b) (0, 1)) (getPosLine (1, 1) (1, 0))

-- Mark matching cells as Nothing (null)
markCell :: Cell -> (a, Cell) -> (a, Cell)
markCell x (i, y) = if y == x then (i, Nothing) else (i, y)

-- Update a board with the next number called
updateBoard :: Cell -> Board -> Board
updateBoard c (i, (_, b))= (i, (,) (fromJust 0 c) $ (b //) $ map (markCell c) $ assocs b)
updateBoards :: [Board] -> Cell -> [Board]
updateBoards bs c = map (updateBoard c) bs

tupAdd :: Pos -> Pos -> Pos
tupAdd (a, b) (c, d) = (a+c, b+d)

parseInp :: [String] -> ([Cell], [Board])
parseInp (x:xs) = (parseInts x, parseBoards xs) where
    parseBoard = (,) (-1) . listArray ((1,1),(5,5)) . concatMap parseInts

    -- Associate each board with an ID
    identify bs = zip [1..length bs] bs

    groupInp :: ([a] -> Bool) -> (a -> Bool) -> [a] -> [[a]]
    groupInp g f = filter g . groupBy (\a b -> f a && f b)

    -- Returns Maybe Int's since all the numbers I needed were Maybes anyways
    parseInts = map (Just . read) . groupInp (isDigit . head) isDigit :: String -> [Cell]
    parseBoards = identify . map parseBoard . groupInp (/= [""]) (not . null)