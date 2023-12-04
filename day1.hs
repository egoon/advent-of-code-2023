import System.IO
import Data.Char

day1 :: IO()
day1 = do
    content <- readFile "test-data/day1"

    let digits = map doDigits (lines content)

    let values = map (\s -> 10 * digitToInt (head s) + digitToInt (last(s))) digits

    putStrLn $ show (sum values)

doDigits :: String -> String
doDigits s 
    | s == []               = []
    | isDigit (head s)      = (head s) : doDigits (tail s)
    | take 3 s == "one"     = '1' : doDigits (tail s)
    | take 3 s == "two"     = '2' : doDigits (tail s)
    | take 5 s == "three"   = '3' : doDigits (tail s)
    | take 4 s == "four"    = '4' : doDigits (tail s)
    | take 4 s == "five"    = '5' : doDigits (tail s)
    | take 3 s == "six"     = '6' : doDigits (tail s)
    | take 5 s == "seven"   = '7' : doDigits (tail s)
    | take 5 s == "eight"   = '8' : doDigits (tail s)
    | take 4 s == "nine"    = '9' : doDigits (tail s)
    | take 4 s == "zero"    = '0' : doDigits (tail s)
    | otherwise             = doDigits (tail s)



