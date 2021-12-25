main = do
    input <- readFile "input.txt" >>= return . map getVals . chunks 18 . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = maximum . generateAllMONADs
part2 = minimum . generateAllMONADs

generateAllMONADs inp = concatMap (\w -> generateMONADs w [] [] inp) [1..9]

generateMONADs :: Int -> [Int] -> [Int] -> [(Int, Int)] -> [Int]
generateMONADs w s is [] = [toNum $ reverse is] -- Valid setup found
generateMONADs w s is ((a, b):as)
    | a < 0 && a + (head s) /= w = [] -- If unable to pop from stack, invalid setup
    | otherwise = concatMap (\w' -> generateMONADs w' s' (w:is) as) $ [1..9]
    where s' = if a > 0 then (b + w) : s else tail s -- Push or pop from stack

toNum :: [Int] -> Int
toNum = foldl (\b -> (10 * b +)) 0

getVals :: [String] -> (Int, Int)
getVals xs = (a, b) where
    a = read . last . words $ xs !! 5
    b = read . last . words $ xs !! 15

chunks _ [] = []
chunks n xs = let (a, b) = splitAt n xs in a : chunks n b