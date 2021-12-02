main = do
    input <- readFile "input.txt" >>= return . map read . lines :: IO [Int]
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = sum . map fromEnum . myScan (<)
part2 = sum . map fromEnum . myScan (<) . myScan2 (\x y z -> x + y + z)

myScan :: (a -> a -> b) -> [a] -> [b]
myScan _ [x] = []
myScan f (x:y:zs) = (f x y) : myScan f (y:zs)

myScan2 :: (a -> a -> a -> b) -> [a] -> [b]
myScan2 _ [a,b] = []
myScan2 f (a:b:c:ds) = f a b c : myScan2 f (b:c:ds)