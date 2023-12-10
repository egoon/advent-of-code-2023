import System.IO
import Data.Char (isDigit)
import Data.Text (replace)
import Data.Text.Read (decimal)
import Day2 (toInt)

data Part = Number Int Int Int Int -- number, x1, x2, y
    | Symbol Int Int -- x, y
    | Dot
    deriving Show

day3 :: IO()
day3 = do 
    content <- readFile "input/day3"

    let parts = parse $ lines content

    let numbers = filter isNumber parts 

    let symbols = filter isSymbol parts 

    let adjNs = filter (anyAdj symbols) numbers

    putStrLn $ show numbers
    putStrLn $ show symbols

    putStrLn $ show adjNs

    putStrLn $ show $ sum $ map (\(Number n _ _ _) -> n) adjNs



parse :: [String] -> [Part]
parse l = parse' [] 0 0 l

parse' :: [Part] -> Int -> Int -> [String] -> [Part]
parse' parts x y [] = parts -- end of input
parse' parts x y ([]:ss) = parse' (Dot:parts) 0 (y+1) ss -- en of line
parse' parts x y (('.':cs):ss) = parse' (Dot:parts) (x+1) y (cs:ss) -- dot (.)
parse' ((Number n nx1 nx2 ny):parts) x y ((c:cs):ss)
    | isDigit c = parse' ((Number (n*10+(toInt (c:[]))) nx1 x ny):parts) (x+1) y (cs:ss) -- continued Number
    | otherwise = parse' ((Symbol x y):(Number n nx1 nx2 ny):parts) (x+1) y (cs:ss) -- Symbol directly after Number
parse' parts x y ((c:cs):ss)
    | isDigit c = parse' ((Number (toInt (c:[])) x x y):parts) (x+1) y (cs:ss) -- new Number
    | otherwise = parse' ((Symbol x y):parts) (x+1) y (cs:ss) -- Symbol

isNumber (Number _ _ _ _) = True
isNumber _ = False

isSymbol (Symbol _ _)  = True
isSymbol _ = False

isAdj :: Part -> Part -> Bool
isAdj (Number _ x1 x2 ny) (Symbol x sy) 
    | (x1-1) > x = False
    | (x2+1) < x = False
    | (ny-1) > sy = False
    | (ny+1) < sy = False
    | otherwise = True
isAdj _ _ = False

anyAdj :: [Part] -> Part -> Bool
anyAdj [] _  = False
anyAdj (s:ss) n  
    | isAdj n s = True
    | otherwise = anyAdj ss n