import Data.Char (isDigit)

data SnailNum = L Int | T SnailNum SnailNum

main = do
    input <- readFile "input.txt" >>= return . map (fst . parseSnailNum) . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = magnitude . foldl1 (<+>)
part2 = maximum . getAllMags

getAllMags :: [SnailNum] -> [Int]
getAllMags [x] = []
getAllMags (x:xs) = map (magnitude . (x <+>)) xs ++ map (magnitude . (<+> x)) xs ++ getAllMags xs

magnitude :: SnailNum -> Int
magnitude (L x) = x
magnitude (T a b) = 3 * (magnitude a) + 2 * (magnitude b)

a <+> b = split $ explode $ T a b

explode :: SnailNum -> SnailNum
explode t = case explode' 0 t of
    (t', Just _)  -> explode t'
    (t', _)       -> t'
    where
    explode' :: Int -> SnailNum -> (SnailNum, Maybe (Int, Int))
    explode' _ (L x) = (L x, Nothing)
    explode' d t@(T (L a) (L b))
        | d >= 4 = (L 0, Just (a, b))
        | otherwise = (t, Nothing)
    explode' d t@(T a b) = case explode' (d + 1) a of
        (ta, Just (l, r)) -> (T ta (overflow 0 r b), Just (l, 0))
        (ta, _)           -> case explode' (d + 1) b of
            (tb, Just (l, r)) -> (T (overflow 1 l ta) tb, Just (0, r))
            (tb, _)           -> (T ta tb, Nothing)

split :: SnailNum -> SnailNum
split t = case split' t of
    (t', True)  -> split $ explode t'
    (t', _)     -> t'
    where
    split' :: SnailNum -> (SnailNum, Bool)
    split' (L x)
        | x >= 10 = (T (L $ x `div` 2) (L $ x `div` 2 + (x `mod` 2)), True)
        | otherwise = (L x, False)
    split' (T a b) = case split' a of
        (ta, True) -> (T ta b, True)
        (ta, _)    -> case split' b of
            (tb, True) -> (T ta tb, True)
            (tb, _)    -> (T ta tb, False)

overflow :: Int -> Int -> SnailNum -> SnailNum
overflow _ x (L a) = L $ a + x
overflow 0 x (T a b) = T (overflow 0 x a) b
overflow 1 x (T a b) = T a $ overflow 1 x b

parseSnailNum :: String -> (SnailNum, String)
parseSnailNum ('[':xs) = let
    (l, xs') = parseSnailNum xs
    (r, xs'') = parseSnailNum xs'
    in (T l r, xs'')
parseSnailNum (x:xs)
    | isDigit x = let (digs, xs') = span isDigit xs in (L $ read (x:digs), xs')
    | otherwise = parseSnailNum xs