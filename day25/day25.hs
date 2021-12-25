import Data.Array.IArray
type Point = (Int, Int)
type Cucumbers = Array Point Char

main = do
    input <- readFile "input.txt" >>= return . toCuc . lines
    putStrLn $ show $ part1 input
    -- putStrLn $ printBoard $ stepCuc input

part1 = snd . runTillHalt

runTillHalt :: Cucumbers -> (Cucumbers, Int)
runTillHalt = go 1 where
    go x cs
        | cs' == cs = (cs, x)
        | otherwise = go (x + 1) cs'
        where cs' = stepCuc cs

stepCuc = moveCuc down . moveCuc east
down = ((1, 0), 'v'); east = ((0, 1), '>')

moveCuc :: (Point, Char) -> Cucumbers -> Cucumbers
moveCuc (d, t) cs = cs // concat [[(i', c), (i, '.')] | (i, c) <- assocs cs,
        c == t, let i' = pAdd cs i d, cs ! i' == '.']

toCuc :: [String] -> Cucumbers
toCuc xs = listArray ((1,1), (length xs, length $ head xs)) $ concat xs

pAdd arr (a, b) (c, d) =
    let ((lx,ly),(ux,uy)) = bounds arr
    in (clamp (lx,ux) $ a + c, clamp (ly,uy) $ b + d)
clamp (l, u) p = if p > u then (p - u - 1 + l) else p