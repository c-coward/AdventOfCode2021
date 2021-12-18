import Data.Char (isDigit)

data SnailNum = L Int | T SnailNum SnailNum
    -- deriving (Show)
instance Show SnailNum where
    show (T a b) = "[" ++ show a ++ "," ++ show b ++ "]"
    show (L x) = show x

main = do
    input <- readFile "input.txt" >>= return . map (fst . parseSnailNum) . lines
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = magnitude . foldl1 (<+>)
part2 = maximum . getAllMags

getAllMags :: [SnailNum] -> [Int]
getAllMags [] = []
getAllMags (x:xs) = map (magnitude . (x <+>)) xs ++ map (magnitude . (<+> x)) xs ++ getAllMags xs

magnitude :: SnailNum -> Int
magnitude (L x) = x
magnitude (T a b) = 3 * (magnitude a) + 2 * (magnitude b)

a <+> b = reduce $ T a b

reduce :: SnailNum -> SnailNum
reduce = split . explode

explode :: SnailNum -> SnailNum
explode t = case explode' 0 t of
    (t', Just _)  -> explode t'
    (t', Nothing) -> t'
    where
    explode' :: Int -> SnailNum -> (SnailNum, Maybe (Int, Int))
    explode' d (L x) = (L x, Nothing)
    explode' d t@(T (L a) (L b))
        | d >= 4 = (L 0, Just (a, b))
        | otherwise = (t, Nothing)
    explode' d t@(T a b) = case explode' (d + 1) a of
        (ta, Just (l, r)) -> (T ta (addRight r b), Just (l, 0))
        (ta, Nothing)     -> case explode' (d + 1) b of
            (tb, Just (l, r)) -> (T (addLeft l ta) tb, Just (0, r))
            (tb, Nothing)     -> (T ta tb, Nothing)

split :: SnailNum -> SnailNum
split t = case split' t of
    (t', True)  -> split $ explode t'
    (t', False) -> t'
    where
    split' :: SnailNum -> (SnailNum, Bool)
    split' (L x)
        | x >= 10 = (T (L $ x `div` 2) (L $ x `div` 2 + (x `mod` 2)), True)
        | otherwise = (L x, False)
    split' (T a b) = case split' a of
        (ta, True) -> (T ta b, True)
        (ta, False) -> case split' b of
            (tb, True) -> (T ta tb, True)
            (tb, False) -> (T ta tb, False)

addRight :: Int -> SnailNum -> SnailNum
addRight x (L a) = L (a + x)
addRight x (T a b) = T (addRight x a) b

addLeft :: Int -> SnailNum -> SnailNum
addLeft x (L a) = L (a + x)
addLeft x (T a b) = T a (addLeft x b)

parseSnailNum :: String -> (SnailNum, String)
parseSnailNum ('[':xs) = let
    (l, xs') = parseSnailNum xs
    (r, xs'') = parseSnailNum xs'
    in (T l r, xs'')
parseSnailNum (',':xs) = parseSnailNum xs
parseSnailNum (']':xs) = parseSnailNum xs
parseSnailNum xs = let (digs, xs') = span (isDigit) xs
    in (L $ read digs, xs')