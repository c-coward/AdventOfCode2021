import Data.Char (digitToInt, intToDigit, isSpace)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Control.Applicative

main = do
    input <- readFile "input.txt" >>= return . hexToBin . concat . lines -- Strip newline
    putStrLn $ (++) "Part 1: " $ show $ part1 input
    putStrLn $ (++) "Part 2: " $ show $ part2 input

part1 = versionSum . fst . fromMaybe (L 0 0, "") . parse packet
part2 = process . fst . fromMaybe (L 0 0, "") . parse packet

process :: Packet -> Int
process (L _ x) = x
process (O _ o ps) = let ps' = map process ps in case o of
    0 -> sum ps'
    1 -> product ps'
    2 -> minimum ps'
    3 -> maximum ps'
    5 -> fromEnum $ all (head ps' >) $ tail ps'
    6 -> fromEnum $ all (head ps' <) $ tail ps'
    7 -> fromEnum $ all (head ps' ==) $ tail ps'

versionSum :: Packet -> Int
versionSum (L v _) = v
versionSum (O v _ ps) = v + sum (map versionSum ps)

-- Packet Parsing
data Packet = L Int Int -- Literal Version Value
            | O Int Int [Packet] -- Operator Version Opcode SubPackets
            deriving (Show, Eq)

grab n = P (\s -> if (length s < n) then Nothing else Just (splitAt n s))
next3 = grab 3 >>= return . binToInt -- Used in version and type

ver = next3 -- Parse the version number
lit = next3 >>= \x -> if x == 4 then return True else empty -- Check if literal value
opc = next3 -- Parse the next 3 bits as an opcode

len 0 = grab 15 >>= return . binToInt
len 1 = grab 11 >>= return . binToInt

literal = do
    char '1'
    a <- grab 4
    b <- literal
    return $ a ++ b
    <|> do
    char '0'
    grab 4

packet = do
    v <- ver
    lit
    l <- literal
    return $ L v (binToInt l)
    <|> do
    v <- ver
    o <- opc
    i <- item >>= return . binToInt . (\x -> [x]) -- Length Type ID
    l <- len i
    ps <- packets i l
    return $ O v o ps

packets 0 l = do
    xs <- grab l
    let (Just (ps, _)) = parse packets0 xs
    return ps
packets 1 l = packets1 l

packets0 = do
    p <- packet
    ps <- packets0
    return (p:ps)
    <|> return []

packets1 0 = return []
packets1 x = do
    p <- packet
    ps <- packets1 (x - 1)
    return (p:ps)

-- Reference for parsing library: http://www.cs.nott.ac.uk/~pszgmh/Parsing.hs
newtype Parser a = P (String -> Maybe (a, String))
parse :: Parser a -> String -> Maybe (a, String)
parse (P f) = f
item = P (\s -> if (null s) then Nothing else Just (head s,tail s))
sat p = item >>= (\x -> if p x then return x else empty)
char c = sat (== c)
space = many (sat isSpace) >> return ()
token p = space >> p >>= (\r -> space >> return r)
instance Functor Parser where
    fmap g p = P (\s -> case parse p s of
        Nothing     -> Nothing
        Just (v, rs) -> Just (g v, rs))
instance Applicative Parser where
    pure x = P (\s -> Just (x, s))
    pg <*> px = P (\s -> case parse pg s of
        Nothing -> Nothing
        Just (g, rs) -> parse (fmap g px) rs)
instance Monad Parser where
    p >>= f = P (\s -> case parse p s of
        Nothing -> Nothing
        Just (x, rs) -> parse (f x) rs)
instance Alternative Parser where
    empty = P (\s -> Nothing)
    pf <|> pg = P (\s -> case parse pf s of
        Nothing -> parse pg s
        r       -> r)

-- String manipulation
binToInt :: String -> Int
binToInt = foldl (\b a -> 2 * b + a) 0 . map digitToInt

hexToBin :: String -> String
hexToBin = concat . map (intToBin . digitToInt)

intToBin :: Int -> String
intToBin = pad4 . reverse . unfoldr nextBinDig

nextBinDig :: Int -> Maybe (Char, Int)
nextBinDig 0 = Nothing
nextBinDig n = Just (intToDigit $ n `mod` 2, n `div` 2)

pad4 :: String -> String
pad4 xs = ['0' | _ <- [1..4 - length xs]] ++ xs