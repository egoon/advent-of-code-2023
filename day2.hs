module Day2 where 
 
import System.IO
import Data.Char
import Data.Text (replace)

day2 :: IO()
day2 = do
    content <- readFile "input/day2"
    let gameLines = lines (filter (\c -> (isDigit c) || (isAlpha c) || (isSpace c)) content)
    let gameWords = map words gameLines
    let games = map asGame gameWords

    let validGames = filter possibleGame games

    putStrLn $ show $ sum $ map (\(G n _ _ _) -> n) validGames

    putStrLn $ show $ sum $ map (\(G _ r g b) -> r*g*b) games 

data Game = Error | G Int Int Int Int -- game number, red, green, blue
    deriving Show


asGame :: [String] -> Game
asGame [] = Error
asGame (_:[]) = Error
asGame (g:n:t)
    | g == "Game" = asGame' (G (toInt n) 0 0 0) t
    | otherwise = Error

asGame' g [] = g
asGame' g (_:[]) = Error
asGame' (G n r g b) (x:c:t)
    | c == "red"   = asGame' (G n (max r (toInt x)) g b) t
    | c == "green" = asGame' (G n r (max g (toInt x)) b) t
    | c == "blue"  = asGame' (G n r g (max b (toInt x))) t
    | otherwise    = G n r b g


toInt :: String -> Int
toInt [] = 0
toInt ns = toInt' 0 ns


toInt' :: Int -> String -> Int 
toInt' x [] = x
toInt' x (n:ns) 
    | isDigit n = toInt' (10*x + digitToInt n) ns
    | otherwise = x


possibleGame :: Game -> Bool
possibleGame Error = False
possibleGame (G _ r g b) = r < 13 && g < 14 && b < 15