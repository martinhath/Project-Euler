triangle :: Int -> Int
triangle n = quot (n*(n+1)) 2


istriangle :: Int -> Bool
istriangle n = n == (head $ filter ((<=) n) $ map triangle [1..])

wordtonum :: String -> Int
wordtonum = sum . map (\n -> fromEnum n - 64)

istriangleword :: String -> Bool
istriangleword = istriangle . wordtonum 

removechar :: Char -> String -> String
removechar _ [] = []
removechar c (x:xs)
    | x == c    = removechar c xs
    | otherwise = x:(removechar c xs)

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace o n (x:xs) 
    | o == x    = n:(replace o n xs)
    | otherwise = x:(replace o n xs)

towords :: String -> [String]
towords = lines . replace ',' '\n' . removechar '\"'

main :: IO ()
main = do
    a <- readFile "p042_words"
    putStrLn $ show $ length $ filter istriangleword $ towords a
