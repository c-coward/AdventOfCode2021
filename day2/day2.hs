import Debug.Trace (traceShowId)

main = do
    input <- readFile "input.txt" >>= return . map (tup . words) . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = (\(a,b) -> a * b) . foldl process (0, 0)
part2 = (\(a,b,_) -> a * b) . foldl process2 (0, 0, 0)

process :: (Int, Int) -> (String, Int) -> (Int, Int)
process (p, d) ("forward", x) = (p + x, d)
process (p, d) ("up", x) = (p, d - x)
process (p, d) ("down", x) = (p, d + x)

process2 :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
process2 (p, d, a) ("forward", x) = (p + x, d + a * x, a)
process2 (p, d, a) ("up", x) = (p, d, a - x)
process2 (p, d, a) ("down", x) = (p, d, a + x)

tup :: [String] -> (String, Int)
tup [a, b] = (a, read b)