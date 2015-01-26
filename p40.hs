import Data.Char

d :: Int -> Char
d n = flip (!!) n $ foldr ((++) . show) "" [0..]

sol :: Int
sol = product $ map (digitToInt . d) $ map (10^) [0..6]

main :: IO ()
main = do
    putStrLn $ show $ sol
