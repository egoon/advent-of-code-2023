import System.IO
import Tokenizer

day4 :: IO()
day4 = do 
    content <- readFile "input/day4.txt"

    let ts = map tokens $ lines content
    -- putStrLn $ show ts

    let cards = map card ts
    putStrLn $ show cards 

    let scores = map evaluate cards
    putStrLn $ show scores

    putStrLn $ show $ sum scores



data Card = C Int [Int] [Int]
    deriving Show

card :: [Token] -> Card
card ((Text "Card"):(Num n):Colon:ts) = card' (C n [] []) ts
card _ = C 0 [] [] -- error case

card' c (Pipe:ts) = card'' c ts
card' (C n ws _) ((Num w):ts) = card' (C n (w:ws) []) ts
card' c _ = c -- error case

card'' (C n ws hs) ((Num h):ts) = card'' (C n ws (h:hs)) ts
card'' c _ = c -- error case

contains :: Int -> [Int] -> Bool
contains x [] = False
contains x (y:ys)
    | (x == y) = True
    | otherwise = contains x ys

evaluate c = evaluate' c 0

evaluate' (C _ _ []) 0 = 0
evaluate' (C _ _ []) s = 2^(s-1)
evaluate' (C n ws (h:hs)) s
    | contains h ws = evaluate' (C n ws hs) (s+1)
    | otherwise = evaluate' (C n ws hs) s