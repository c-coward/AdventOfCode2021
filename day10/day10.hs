import Data.List (sort)

main = do
    input <- readFile "input.txt" >>= return . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = sum . map (score . findFirstIllegal)
part2 = takeMid . sort . map (scoreMatch . findUnmatched) . filter (null . findFirstIllegal)

takeMid :: [Int] -> Int
takeMid xs = xs !! (length xs `div` 2)

findUnmatched :: String -> String
findUnmatched = go "" where
    go xs [] = map unmatch xs
    go xs (y:ys)
        | y `elem` "([{<" = go (y:xs) ys
        | match y == head xs = go (tail xs) ys
        | otherwise = go xs ys

findFirstIllegal :: String -> Maybe Char
findFirstIllegal = go "" where
    go :: String -> String -> Maybe Char
    go _ [] = Nothing
    go xs (y:ys)
        | y `elem` "([{<" = go (y:xs) ys
        | match y == head xs = go (tail xs) ys
        | otherwise = Just y

match :: Char -> Char
match ')' = '('
match ']' = '['
match '}' = '{'
match '>' = '<'
unmatch '(' = ')'
unmatch '[' = ']'
unmatch '{' = '}'
unmatch '<' = '>'

score :: Maybe Char -> Int
score mc = case mc of
    Nothing -> 0
    Just c  -> case c of
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137

scoreMatch :: String -> Int
scoreMatch = foldl (\b a -> 5 * b + score2 a) 0 where
    score2 :: Char -> Int
    score2 ')' = 1
    score2 ']' = 2
    score2 '}' = 3
    score2 '>' = 4